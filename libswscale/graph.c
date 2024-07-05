/*
 * Copyright (C) 2024 Niklas Haas
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include "libavutil/avassert.h"
#include "libavutil/error.h"
#include "libavutil/mem.h"
#include "libavutil/pixdesc.h"
#include "libavutil/slicethread.h"

#include "libswscale/swscale.h"

#include "swscale_internal.h"
#include "graph.h"

/* slice_align should be a power of two, or 0 to disable slice threading */
static SwsPass *pass_add(SwsGraph *graph, void *priv, int w, int h,
                         SwsField in, SwsField out, int slice_align)
{
    SwsPass *pass = av_mallocz(sizeof(*pass));
    int ret;

    pass->graph  = graph;
    pass->input  = in;
    pass->output = out;
    pass->priv   = priv;
    pass->width  = w;
    pass->height = h;

    if (!slice_align) {
        pass->slice_h = pass->height;
        pass->num_slices = 1;
    } else {
        pass->slice_h = (pass->height + graph->num_threads - 1) / graph->num_threads;
        pass->slice_h = FFALIGN(pass->height, slice_align);
        pass->num_slices = (pass->height + pass->slice_h - 1) / pass->slice_h;
    }

    ret = av_dynarray_add_nofree(&graph->passes, &graph->num_passes, pass);
    if (ret < 0)
        av_freep(&pass);
    return pass;
}

/* Set output linesize before calling this */
static int pass_alloc_output(SwsPass *pass)
{
    const int aligned_h = pass->num_slices * pass->slice_h;
    const int *linesize = pass->output.linesize;

    size_t offset[4];
    size_t total_size = 0;
    for (int i = 0; i < 4; i++) {
        const size_t size = FFABS(linesize[i]) * aligned_h;
        offset[i] = total_size;
        total_size = FFALIGN(total_size + size, 16);
    }

    av_assert0(!pass->buf);
    pass->buf = av_malloc(total_size);
    if (!pass->buf)
        return AVERROR(ENOMEM);

    for (int i = 0; i < 4; i++) {
        uint8_t *base = pass->buf + offset[i];
        if (linesize[i] < 0)
            base -= linesize[i] * (aligned_h - 1);
        pass->output.data[i] = linesize[i] ? base : NULL;
    }

    return 0;
}

static void setup_swscale(const SwsField *out, const SwsField *in,
                          const SwsPass *pass)
{
    SwsContext *c = pass->priv;
    if (c->flags & SWS_BITEXACT && c->dither == SWS_DITHER_ED && c->dither_error[0]) {
        for (int i = 0; i < 4; i++)
            memset(c->dither_error[i], 0, sizeof(c->dither_error[0][0]) * (c->dstW + 2));
    }

    if (usePal(c->srcFormat))
        ff_update_palette(c, (const uint32_t *) in->data[1]);
}

static void run_rgb0(const SwsField *out, const SwsField *in, int slice_y,
                     const SwsPass *pass)
{
    SwsContext *c = pass->priv;
    const int x0 = c->src0Alpha - 1;
    const int w4 = 4 * pass->width;
    const int slice_h = pass->slice_h;
    const int src_stride = in->linesize[0];
    const int dst_stride = out->linesize[0];
    const uint8_t *src = in->data[0] + slice_y * src_stride;
    uint8_t *dst = out->data[0] + slice_y * dst_stride;

    for (int y = 0; y < slice_h; y++) {
        memcpy(dst, src, w4 * sizeof(*dst));
        for (int x = x0; x < w4; x += 4)
            dst[x] = 0xFF;

        src += src_stride;
        dst += dst_stride;
    }
}

static void run_xyz2rgb(const SwsField *out, const SwsField *in, int y,
                        const SwsPass *pass)
{
    ff_xyz12Torgb48(pass->priv, out->data[0] + y * out->linesize[0], out->linesize[0],
                    in->data[0] + y * in->linesize[0], in->linesize[0],
                    pass->width, pass->slice_h);
}

static void run_rgb2xyz(const SwsField *out, const SwsField *in, int y,
                        const SwsPass *pass)
{
    ff_rgb48Toxyz12(pass->priv, out->data[0] + y * out->linesize[0], out->linesize[0],
                    in->data[0] + y * in->linesize[0], in->linesize[0],
                    pass->width, pass->slice_h);
}

static void run_unscaled(const SwsField *out, const SwsField *in, int y,
                         const SwsPass *pass)
{
    SwsContext *const sws = pass->priv;
    const uint8_t *src_data[4];

    for (int i = 0; i < 4; i++) {
        const int vshift = (i == 1 || i == 2) ? sws->chrSrcVSubSample : 0;
        src_data[i] = in->data[i];
        if (src_data[i] && (!i || !usePal(sws->srcFormat)))
            src_data[i] += (y >> vshift) * in->linesize[i];
    }

    sws->convert_unscaled(sws, src_data, in->linesize, y, pass->slice_h,
                          out->data, out->linesize);
}

static void run_swscale(const SwsField *out, const SwsField *in, int y,
                        const SwsPass *pass)
{
    SwsContext *const sws = pass->priv;
    const SwsGraph *const graph = pass->graph;
    const int slice_h = pass->slice_h;
    const int src_h = graph->src.height;
    const int h = FFMIN(slice_h, graph->dst.height - y);
    const int sub_y = graph->dst.desc->log2_chroma_h;
    const uint8_t *src_data[4];
    uint8_t *dst_data[4];
    int src_stride[4];

    for (int i = 0; i < 4; i++) {
        const int vshift = (i == 1 || i == 2) ? sub_y : 0;
        src_stride[i] = in->linesize[i];
        src_data[i] = in->data[i];
        dst_data[i] = out->data[i];
        if (dst_data[i] && (!i || !usePal(sws->dstFormat)))
            dst_data[i] += (y >> vshift) * out->linesize[i];
    }

    ff_swscale(sws, src_data, src_stride, 0, src_h, dst_data, out->linesize, y, h);
}

static void get_chroma_pos(SwsGraph *graph, int *h_chr_pos, int *v_chr_pos,
                           const SwsFormat *fmt)
{
    enum AVChromaLocation chroma_loc = fmt->loc;
    const int sub_x = fmt->desc->log2_chroma_w;
    const int sub_y = fmt->desc->log2_chroma_h;
    int x_pos, y_pos;

    /* Explicitly default to center siting for compatibility with swscale */
    if (chroma_loc == AVCHROMA_LOC_UNSPECIFIED) {
        chroma_loc = AVCHROMA_LOC_CENTER;
        graph->incomplete |= sub_x || sub_y;
    }

    /* av_chroma_location_enum_to_pos() always gives us values in the range from
     * 0 to 256, but we need to adjust this to the true value range of the
     * subsampling grid, which may be larger for h/v_sub > 1 */
    av_chroma_location_enum_to_pos(&x_pos, &y_pos, chroma_loc);
    x_pos *= (1 << sub_x) - 1;
    y_pos *= (1 << sub_y) - 1;

    /* Fix vertical chroma position for interlaced frames */
    if (sub_y && fmt->interlaced) {
        /* When vertically subsampling, chroma samples are effectively only
         * placed next to even rows. To access them from the odd field, we need
         * to account for this shift by offsetting the distance of one luma row.
         *
         * For 4x vertical subsampling (v_sub == 2), they are only placed
         * next to every *other* even row, so we need to shift by three luma
         * rows to get to the chroma sample. */
        if (graph->field == FIELD_BOTTOM)
            y_pos += (256 << sub_y) - 256;

        /* Luma row distance is doubled for fields, so halve offsets */
        y_pos >>= 1;
    }

    /* Explicitly strip chroma offsets when not subsampling, because it
     * interferes with the operation of flags like SWS_FULL_CHR_H_INP */
    *h_chr_pos = sub_x ? x_pos : -513;
    *v_chr_pos = sub_y ? y_pos : -513;
}

static int init_pass(SwsGraph *graph, SwsContext *sws,
                     SwsField input, SwsField output)
{
    const int src_w = sws->srcW, src_h = sws->srcH;
    const int dst_w = sws->dstW, dst_h = sws->dstH;
    const int unscaled = src_w == dst_w && src_h == dst_h;
    int align = sws_receive_slice_alignment(sws);
    SwsPass *pass;
    int ret;

    if (sws->cascaded_context[0]) {
        const int num_cascaded = sws->cascaded_context[2] ? 3 : 2;
        for (int i = 0; i < num_cascaded; i++) {
            SwsContext *sub = sws->cascaded_context[i];

            if (i + 1 == num_cascaded) {
                ret = init_pass(graph, sub, input, output);
            } else {
                /* Steal the intermediate buffers that were already allocated */
                SwsField tmp;
                av_assert0(i < 2);
                memcpy(tmp.data, sws->cascaded_tmp[i], sizeof(tmp.data));
                memcpy(tmp.linesize, sws->cascaded_tmpStride[i], sizeof(tmp.linesize));

                ret = init_pass(graph, sub, input, tmp);
                input = tmp;
            }

            if (ret < 0)
                return ret;
        }

        return 0;
    }

    if (sws->dither == SWS_DITHER_ED && !sws->convert_unscaled)
        align = 0; /* disable slice threading */

    if (sws->src0Alpha && !sws->dst0Alpha && isALPHA(sws->dstFormat)) {
        SwsField tmp = { .linesize = { FFALIGN(src_w * sizeof(uint8_t[4]), 16) }};
        SwsPass *sub = pass_add(graph, sws, src_w, src_h, input, tmp, 1);
        if (!sub || pass_alloc_output(sub) < 0)
            return AVERROR(ENOMEM);
        sub->run = run_rgb0;
        input = sub->output;
    }

    if (sws->srcXYZ && !(sws->dstXYZ && unscaled)) {
        SwsField tmp = { .linesize = { FFALIGN(src_w * sizeof(uint16_t[3]), 16) }};
        SwsPass *sub = pass_add(graph, sws, src_w, src_h, input, tmp, 1);
        if (!sub || pass_alloc_output(sub) < 0)
            return AVERROR(ENOMEM);
        sub->run = run_xyz2rgb;
        input = sub->output;
    }

    pass = pass_add(graph, sws, dst_w, dst_h, input, output, align);
    if (!pass)
        return AVERROR(ENOMEM);
    pass->setup  = setup_swscale;
    pass->run    = sws->convert_unscaled ? run_unscaled : run_swscale;

    if (sws->dstXYZ && !(sws->srcXYZ && unscaled)) {
        SwsPass *sub = pass_add(graph, sws, dst_w, dst_h, output, output, 1);
        if (!sub)
            return AVERROR(ENOMEM);
        sub->run = run_rgb2xyz;
    }

    return 0;
}

static int init_passes(SwsGraph *graph)
{
    int sws_flags, sws_filter, ret;

    SwsContext2 *const ctx     = graph->ctx;
    const SwsFormat *const src = &graph->src;
    const SwsFormat *const dst = &graph->dst;
    const SwsScaler scaler     = sws_get_scaler(ctx);
    const SwsScaler scaler_sub = sws_get_scaler_sub(ctx);
    const SwsDither dither     = sws_get_dither(ctx);
    SwsContext *sws;

FF_DISABLE_DEPRECATION_WARNINGS
    sws_filter = ctx->sws_flags & (SWS_POINT         |
                                   SWS_AREA          |
                                   SWS_BILINEAR      |
                                   SWS_FAST_BILINEAR |
                                   SWS_BICUBIC       |
                                   SWS_X             |
                                   SWS_GAUSS         |
                                   SWS_LANCZOS       |
                                   SWS_SINC          |
                                   SWS_SPLINE        |
                                   SWS_BICUBLIN);

    sws_flags = ctx->sws_flags ^ sws_filter;
FF_ENABLE_DEPRECATION_WARNINGS

    switch (scaler) {
    case SWS_SCALER_NEAREST:  sws_filter = SWS_POINT;    break;
    case SWS_SCALER_BILINEAR: sws_filter = SWS_BILINEAR; break;
    case SWS_SCALER_BICUBIC:  sws_filter = SWS_BICUBIC;  break;
    case SWS_SCALER_GAUSSIAN: sws_filter = SWS_GAUSS;    break;
    case SWS_SCALER_LANCZOS:  sws_filter = SWS_LANCZOS;  break;
    }

    if (scaler == SWS_SCALER_BICUBIC && scaler_sub == SWS_SCALER_BILINEAR)
        sws_filter = SWS_BICUBLIN;
    else if (scaler_sub && scaler_sub != scaler)
        av_log(ctx, AV_LOG_WARNING, "Subsampling scaler ignored\n");

    sws_flags |= sws_filter;
    if (ctx->flags & SWS_FLAG_BITEXACT)
        sws_flags |= SWS_BITEXACT | SWS_ACCURATE_RND;
    if (!(ctx->flags & SWS_FLAG_ALIAS))
        sws_flags |= SWS_FULL_CHR_H_INP;

    graph->incomplete |= src->range == AVCOL_RANGE_UNSPECIFIED;
    graph->incomplete |= dst->range == AVCOL_RANGE_UNSPECIFIED;

    sws = graph->sws = sws_alloc_context();
    if (!sws)
        return AVERROR(ENOMEM);

    sws->flags     = sws_flags;
    sws->dither    = dither;
    sws->srcW      = src->width;
    sws->srcH      = src->height;
    sws->srcFormat = src->format;
    sws->srcRange  = src->range == AVCOL_RANGE_JPEG;

    sws->dstW      = dst->width;
    sws->dstH      = dst->height;
    sws->dstFormat = dst->format;
    sws->dstRange  = dst->range == AVCOL_RANGE_JPEG;
    get_chroma_pos(graph, &sws->src_h_chr_pos, &sws->src_v_chr_pos, src);
    get_chroma_pos(graph, &sws->dst_h_chr_pos, &sws->dst_v_chr_pos, dst);

    ret = sws_init_context(sws, NULL, NULL);
    if (ret < 0)
        return ret;

    /* Set correct color matrices */
    {
        int in_full, out_full, brightness, contrast, saturation;
        const int *inv_table, *table;
        sws_getColorspaceDetails(sws, (int **)&inv_table, &in_full,
                                (int **)&table, &out_full,
                                &brightness, &contrast, &saturation);

        inv_table = sws_getCoefficients(src->csp);
        table     = sws_getCoefficients(dst->csp);

        graph->incomplete |= src->csp != dst->csp &&
                            src->csp == AVCOL_SPC_UNSPECIFIED ||
                            dst->csp == AVCOL_SPC_UNSPECIFIED;

        sws_setColorspaceDetails(sws, inv_table, in_full, table, out_full,
                                brightness, contrast, saturation);
    }

    ret = init_pass(graph, sws, SWS_INPUT, SWS_OUTPUT);
    if (ret < 0)
        return ret;

    return 0;
}

const uint8_t sws_input_sentinel, sws_output_sentinel;

static const SwsField *resolve_field(SwsGraph *graph, const SwsField *field)
{
    if (field->data[0] == &sws_input_sentinel)
        return &graph->exec.input;
    else if (field->data[0] == &sws_output_sentinel)
        return &graph->exec.output;
    else
        return field;
}

static void sws_graph_worker(void *priv, int jobnr, int threadnr, int nb_jobs,
                             int nb_threads)
{
    SwsGraph *graph = priv;
    const SwsPass *pass = graph->exec.pass;
    const SwsField *input  = resolve_field(graph, &pass->input);
    const SwsField *output = resolve_field(graph, &pass->output);
    const int slice_y = jobnr * pass->slice_h;

    pass->run(output, input, slice_y, pass);
}

int sws_graph_init(SwsContext2 *ctx, SwsGraph *graph,
                   const SwsFormat *dst, const SwsFormat *src,
                   int field)
{
    int ret;

    graph->ctx = ctx;
    graph->src = *src;
    graph->dst = *dst;
    graph->field = field;

    ret = avpriv_slicethread_create(&graph->slicethread, (void *) graph,
                                    sws_graph_worker, NULL, ctx->threads);
    if (ret == AVERROR(ENOSYS))
        graph->num_threads = 1;
    else if (ret < 0)
        goto error;
    else
        graph->num_threads = ret;

    ret = init_passes(graph);
    if (ret < 0)
        goto error;

    return 0;

error:
    sws_graph_uninit(graph);
    return ret;
}

void sws_graph_uninit(SwsGraph *graph)
{
    avpriv_slicethread_free(&graph->slicethread);

    for (int i = 0; i < graph->num_passes; i++) {
        SwsPass *pass = graph->passes[i];
        if (pass->uninit)
            pass->uninit(pass->priv);
        av_free(pass->buf);
        av_free(pass);
    }
    av_free(graph->passes);

    sws_freeContext(graph->sws);
    memset(graph, 0, sizeof(*graph));
}

void sws_graph_run(SwsGraph *graph, const SwsField *out, const SwsField *in)
{
    graph->exec.input  = *in;
    graph->exec.output = *out;

    for (int i = 0; i < graph->num_passes; i++) {
        const SwsPass *pass = graph->passes[i];
        graph->exec.pass = pass;
        if (pass->setup)
            pass->setup(out, in, pass);
        avpriv_slicethread_execute(graph->slicethread, pass->num_slices, 0);
    }
}
