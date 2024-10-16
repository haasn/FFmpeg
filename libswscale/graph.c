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
#include "libavutil/macros.h"
#include "libavutil/mem.h"
#include "libavutil/opt.h"
#include "libavutil/pixdesc.h"
#include "libavutil/slicethread.h"

#include "libswscale/swscale.h"
#include "libswscale/utils.h"

#include "swscale_internal.h"
#include "graph.h"

/* slice_align should be a power of two, or 0 to disable slice threading */
static SwsPass *pass_add(SwsGraph *graph, void *priv, int w, int h,
                         sws_filter_run_t run, SwsImg in, SwsImg out,
                         int slice_align)
{
    SwsPass *pass = av_mallocz(sizeof(*pass));
    int ret;

    pass->graph  = graph;
    pass->run    = run;
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
        pass->slice_h = FFALIGN(pass->slice_h, slice_align);
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

static int vshift(const SwsFormat *fmt, int plane)
{
    const int is_pal = usePal(fmt->format);
    const int is_chroma = (plane == 1 || plane == 2) && !is_pal;
    return is_chroma ? fmt->desc->log2_chroma_h : 0;
}

/* Shift an image vertically by y lines */
static SwsImg shift_img(const SwsFormat *fmt, SwsImg img, int y)
{
    for (int i = 0; i < 4 && img.data[i]; i++)
        img.data[i] += (y >> vshift(fmt, i)) * img.linesize[i];
    return img;
}

static void setup_swscale(const SwsImg *out, const SwsImg *in,
                          const SwsPass *pass)
{
    SwsInternal *c = pass->priv;
    if (c->opts.flags & SWS_BITEXACT && c->opts.dither == SWS_DITHER_ED && c->dither_error[0]) {
        for (int i = 0; i < 4; i++)
            memset(c->dither_error[i], 0, sizeof(c->dither_error[0][0]) * (c->opts.dst_w + 2));
    }

    if (usePal(c->opts.src_format))
        ff_update_palette(c, (const uint32_t *) in->data[1]);
}

static void run_copy(const SwsImg *out_base, const SwsImg *in_base,
                     int y, int h, const SwsPass *pass)
{
    const SwsGraph *graph = pass->graph;
    SwsImg in  = shift_img(&graph->src, *in_base,  y);
    SwsImg out = shift_img(&graph->dst, *out_base, y);

    for (int i = 0; i < FF_ARRAY_ELEMS(in.data) && in.data[i]; i++) {
        const int lines = h >> vshift(&graph->src, i);
        if (in.linesize[i] == out.linesize[i]) {
            memcpy(out.data[i], in.data[i], lines * out.linesize[i]);
        } else {
            const int linesize = FFMIN(out.linesize[i], in.linesize[i]);
            for (int j = 0; j < lines; j++) {
                memcpy(out.data[i], in.data[i], linesize);
                in.data[i]  += in.linesize[i];
                out.data[i] += out.linesize[i];
            }
        }
    }
}

static void run_rgb0(const SwsImg *out, const SwsImg *in, int y, int h,
                     const SwsPass *pass)
{
    SwsInternal *c = pass->priv;
    const int x0 = c->src0Alpha - 1;
    const int w4 = 4 * pass->width;
    const int src_stride = in->linesize[0];
    const int dst_stride = out->linesize[0];
    const uint8_t *src = in->data[0] + y * src_stride;
    uint8_t *dst = out->data[0] + y * dst_stride;

    for (int y = 0; y < h; y++) {
        memcpy(dst, src, w4 * sizeof(*dst));
        for (int x = x0; x < w4; x += 4)
            dst[x] = 0xFF;

        src += src_stride;
        dst += dst_stride;
    }
}

static void run_xyz2rgb(const SwsImg *out, const SwsImg *in, int y, int h,
                        const SwsPass *pass)
{
    ff_xyz12Torgb48(pass->priv, out->data[0] + y * out->linesize[0], out->linesize[0],
                    in->data[0] + y * in->linesize[0], in->linesize[0],
                    pass->width, h);
}

static void run_rgb2xyz(const SwsImg *out, const SwsImg *in, int y, int h,
                        const SwsPass *pass)
{
    ff_rgb48Toxyz12(pass->priv, out->data[0] + y * out->linesize[0], out->linesize[0],
                    in->data[0] + y * in->linesize[0], in->linesize[0],
                    pass->width, h);
}

static void run_unscaled(const SwsImg *out, const SwsImg *in_base,
                         int y, int h, const SwsPass *pass)
{
    SwsInternal *c = pass->priv;
    const SwsImg in = shift_img(&pass->graph->src, *in_base, y);
    c->convert_unscaled(c, (const uint8_t *const *) in.data, in.linesize, y, h,
                        out->data, out->linesize);
}

static void run_swscale(const SwsImg *out_base, const SwsImg *in,
                        int y, int h, const SwsPass *pass)
{
    SwsInternal *c = pass->priv;
    const SwsGraph *const graph = pass->graph;
    const SwsImg out = shift_img(&graph->dst, *out_base, y);
    const int src_h = graph->src.height;

    if (pass->num_slices > 1) {
        SwsInternal *parent = c;
        av_assert1(c->nb_slice_ctx == pass->num_slices);
        c = sws_internal(c->slice_ctx[y / pass->slice_h]);

        if (usePal(c->opts.src_format)) {
            memcpy(c->pal_yuv, parent->pal_yuv, sizeof(c->pal_yuv));
            memcpy(c->pal_rgb, parent->pal_rgb, sizeof(c->pal_rgb));
        }
    }


    ff_swscale(c, (const uint8_t *const *) in->data, in->linesize, 0, src_h,
               out.data, out.linesize, y, h);
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
                     SwsImg input, SwsImg output)
{
    SwsInternal *c = sws_internal(sws);
    const int src_w = c->opts.src_w, src_h = c->opts.src_h;
    const int dst_w = c->opts.dst_w, dst_h = c->opts.dst_h;
    const int unscaled = src_w == dst_w && src_h == dst_h;
    int align = c->dst_slice_align;
    SwsPass *pass;
    int ret;

    if (c->cascaded_context[0]) {
        const int num_cascaded = c->cascaded_context[2] ? 3 : 2;
        for (int i = 0; i < num_cascaded; i++) {
            SwsContext *sub = c->cascaded_context[i];

            if (i + 1 == num_cascaded) {
                ret = init_pass(graph, sub, input, output);
            } else {
                /* Steal the intermediate buffers that were already allocated */
                SwsImg tmp;
                av_assert1(i < FF_ARRAY_ELEMS(c->cascaded_tmp));
                memcpy(tmp.data, c->cascaded_tmp[i], sizeof(tmp.data));
                memcpy(tmp.linesize, c->cascaded_tmpStride[i], sizeof(tmp.linesize));

                ret = init_pass(graph, sub, input, tmp);
                input = tmp;
            }

            if (ret < 0)
                return ret;
        }

        return 0;
    }

    if (c->opts.dither == SWS_DITHER_ED && !c->convert_unscaled)
        align = 0; /* disable slice threading */

    if (c->src0Alpha && !c->dst0Alpha && isALPHA(c->opts.dst_format)) {
        SwsImg tmp = { .linesize = { FFALIGN(src_w * sizeof(uint8_t[4]), 16) }};
        SwsPass *sub = pass_add(graph, c, src_w, src_h, run_rgb0, input, tmp, 1);
        if (!sub || pass_alloc_output(sub) < 0)
            return AVERROR(ENOMEM);
        input = sub->output;
    }

    if (c->srcXYZ && !(c->dstXYZ && unscaled)) {
        SwsImg tmp = { .linesize = { FFALIGN(src_w * sizeof(uint16_t[3]), 16) }};
        SwsPass *sub = pass_add(graph, c, src_w, src_h, run_xyz2rgb, input, tmp, 1);
        if (!sub || pass_alloc_output(sub) < 0)
            return AVERROR(ENOMEM);
        input = sub->output;
    }

    pass = pass_add(graph, c, dst_w, dst_h,
                    c->convert_unscaled ? run_unscaled : run_swscale,
                    input, output, align);
    if (!pass)
        return AVERROR(ENOMEM);
    pass->setup = setup_swscale;

    /**
     * For slice threading, we need to create sub contexts, similar to how
     * swscale normally handles it internally. The most important difference
     * is that we handle cascaded contexts before threaded contexts; whereas
     * context_init_threaded() does it the other way around.
     */

    if (pass->num_slices > 1 && !c->convert_unscaled) {
        c->slice_ctx = av_calloc(pass->num_slices, sizeof(*c->slice_ctx));
        if (!c->slice_ctx)
            return AVERROR(ENOMEM);

        for (int i = 0; i < pass->num_slices; i++) {
            SwsInternal *c2;
            c->slice_ctx[i] = sws_alloc_context();
            if (!c->slice_ctx[i])
                return AVERROR(ENOMEM);
            c->nb_slice_ctx++;

            c2 = sws_internal(c->slice_ctx[i]);
            c2->parent = sws;
            c2->opts = c->opts;

            ret = sws_init_single_context(c->slice_ctx[i], NULL, NULL);
            if (ret < 0)
                return ret;

            sws_setColorspaceDetails(c->slice_ctx[i], c->srcColorspaceTable,
                                     c->opts.src_range, c->dstColorspaceTable,
                                     c->opts.dst_range, c->brightness, c->contrast,
                                     c->saturation);

            for (int i = 0; i < FF_ARRAY_ELEMS(c->srcColorspaceTable); i++) {
                c2->srcColorspaceTable[i] = c->srcColorspaceTable[i];
                c2->dstColorspaceTable[i] = c->dstColorspaceTable[i];
            }
        }
    }

    if (c->dstXYZ && !(c->srcXYZ && unscaled)) {
        SwsPass *sub = pass_add(graph, c, dst_w, dst_h, run_rgb2xyz, output, output, 1);
        if (!sub)
            return AVERROR(ENOMEM);
    }

    return 0;
}

/**
 * Sentinel values to refer to the overall image input / output during
 * filter graph construction, as the true values are not known.
 */
static uint8_t sws_input_sentinel, sws_output_sentinel;

static const SwsImg *resolve_img(SwsGraph *graph, const SwsImg *img)
{
    if (img->data[0] == &sws_input_sentinel)
        return &graph->exec.input;
    else if (img->data[0] == &sws_output_sentinel)
        return &graph->exec.output;
    else
        return img;
}

static int init_passes(SwsGraph *graph)
{
    SwsContext *const ctx     = graph->ctx;
    const SwsFormat *const src = &graph->src;
    const SwsFormat *const dst = &graph->dst;
    const SwsDither dither     = ctx->dither;
    SwsContext *sws;
    int ret;

    const SwsImg input  = { .data = { &sws_input_sentinel }};
    const SwsImg output = { .data = { &sws_output_sentinel }};

    graph->noop = ff_fmt_equal(dst, src);
    if (graph->noop) {
        /* Threaded memcpy pass */
        SwsPass *copy = pass_add(graph, NULL, dst->width, dst->height,
                                 run_copy, input, output, 1);
        if (!copy)
            return AVERROR(ENOMEM);
        return 0;
    }

    graph->incomplete |= src->range == AVCOL_RANGE_UNSPECIFIED;
    graph->incomplete |= dst->range == AVCOL_RANGE_UNSPECIFIED;

    sws = graph->sws = sws_alloc_context();
    if (!sws)
        return AVERROR(ENOMEM);

    sws->flags      = ctx->flags;
    sws->dither     = dither;
    sws->src_w      = src->width;
    sws->src_h      = src->height;
    sws->src_format = src->format;
    sws->src_range  = src->range == AVCOL_RANGE_JPEG;

    sws->dst_w      = dst->width;
    sws->dst_h      = dst->height;
    sws->dst_format = dst->format;
    sws->dst_range  = dst->range == AVCOL_RANGE_JPEG;
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

    ret = init_pass(graph, sws, input, output);
    if (ret < 0)
        return ret;

    return 0;
}

static void sws_graph_worker(void *priv, int jobnr, int threadnr, int nb_jobs,
                             int nb_threads)
{
    SwsGraph *graph = priv;
    const SwsPass *pass = graph->exec.pass;
    const SwsImg *input  = resolve_img(graph, &pass->input);
    const SwsImg *output = resolve_img(graph, &pass->output);
    const int slice_y = jobnr * pass->slice_h;
    const int slice_h = FFMIN(pass->slice_h, pass->height - slice_y);

    pass->run(output, input, slice_y, slice_h, pass);
}

int sws_graph_create(SwsContext *ctx,  const SwsFormat *dst, const SwsFormat *src,
                     int field, SwsGraph **out_graph)
{
    int ret;
    SwsGraph *graph = av_mallocz(sizeof(*graph));
    if (!graph)
        return AVERROR(ENOMEM);

    graph->ctx = ctx;
    graph->src = *src;
    graph->dst = *dst;
    graph->field = field;
    graph->opts = *ctx;
    graph->opts.internal = NULL; /* sanity */

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

    *out_graph = graph;
    return 0;

error:
    sws_graph_free(&graph);
    return ret;
}

void sws_graph_free(SwsGraph **pgraph)
{
    SwsGraph *graph = *pgraph;
    if (!graph)
        return;

    avpriv_slicethread_free(&graph->slicethread);

    for (int i = 0; i < graph->num_passes; i++) {
        SwsPass *pass = graph->passes[i];
        if (pass->uninit)
            pass->uninit(pass);
        av_free(pass->buf);
        av_free(pass);
    }
    av_free(graph->passes);

    sws_freeContext(graph->sws);
    av_free(graph);
    *pgraph = NULL;
}

/* Tests only options relevant to SwsGraph */
static int opts_equal(const SwsContext *c1, const SwsContext *c2)
{
    return c1->flags       == c2->flags       &&
           c1->threads     == c2->threads     &&
           c1->dither      == c2->dither      &&
           c1->alpha_blend == c2->alpha_blend &&
           c1->gamma_flag  == c2->gamma_flag  &&
           !memcmp(c1->scaler_params, c2->scaler_params, sizeof(c1->scaler_params));

}

int sws_graph_reinit(SwsContext *ctx, const SwsFormat *dst, const SwsFormat *src,
                     int field, SwsGraph **out_graph)
{
    const SwsGraph *graph = *out_graph;
    if (graph && ff_fmt_equal(&graph->src, src) &&
                 ff_fmt_equal(&graph->dst, dst) &&
                 opts_equal(ctx, &graph->opts))
        return 0;

    sws_graph_free(out_graph);
    return sws_graph_create(ctx, dst, src, field, out_graph);
}


void sws_graph_run(SwsGraph *graph, const SwsImg *out, const SwsImg *in)
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
