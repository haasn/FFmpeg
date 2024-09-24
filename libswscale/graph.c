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

/* may only be called on power of two slice_h */
SwsFilterOp *sws_add_filter(SwsGraph *graph, int new_w, int new_h,
                            int slice_h, size_t priv_size)
{
    const int aligned_h = FFALIGN(new_h, slice_h);
    SwsFilterOp *filter;

    if (graph->num_filters == SWS_MAX_FILTERS)
        return NULL;

    filter = &graph->filters[graph->num_filters++];
    filter->new_w = new_w;
    filter->new_h = new_h;
    filter->slice_h = slice_h;
    filter->num_slices = (aligned_h + slice_h - 1) / slice_h;
    filter->stride = FFALIGN(new_w, 64);
    filter->tmp = av_malloc(aligned_h * filter->stride);
    if (!filter->tmp)
        return NULL;

    if (priv_size) {
        filter->priv = av_mallocz(priv_size);
        if (!filter->priv)
            return NULL;
    }

    return filter;
}

SwsPixelOp *sws_add_post_op(SwsFilterOp *filter, size_t priv_size)
{
    SwsPixelOp *op;
    if (filter->num_post_ops == SWS_MAX_OPS)
        return NULL;

    op = &filter->post_ops[filter->num_post_ops++];
    if (priv_size) {
        op->priv = av_mallocz(priv_size);
        if (!op->priv)
            return NULL;
    }

    return op;
}

static void uninit_filters(SwsGraph *graph)
{
    for (int i = 0; i < graph->num_filters; i++) {
        SwsFilterOp *filter = &graph->filters[i];
        for (int j = 0; j < filter->num_post_ops; j++)
            av_free(filter->post_ops[j].priv);
        if (filter->uninit)
            filter->uninit(filter->priv);
        av_free(filter->tmp);
        av_free(filter->priv);
    }

    memset(graph->filters, 0, sizeof(graph->filters));
}

typedef struct SwsWrapper {
    const SwsFilterOp *filter;
    const SwsGraph *graph;
    SwsContext *sws[];
} SwsWrapper;

static void uninit_fallback(void *priv)
{
    SwsWrapper *wrapper = priv;
    for (int i = 0; i < wrapper->filter->num_slices; i++)
        sws_freeContext(wrapper->sws[i]);
}

static void read_fallback(int y, sws_pixel_t *out, void *priv)
{
    const SwsWrapper *const wrapper = priv;
    const SwsGraph *const graph = wrapper->graph;
    const int slice_h = wrapper->filter->slice_h;
    SwsContext *const sws = wrapper->sws[y / slice_h];
    const SwsField *const src = graph->exec.src;
    const SwsField *const dst = graph->exec.dst;
    const int h = FFMIN(slice_h, graph->dst.height - y);
    const int sub_y = graph->dst.desc->log2_chroma_h;
    uint8_t *dst_data[4];

    for (int i = 0; i < 4; i++) {
        const int vshift = (i == 1 || i == 2) ? sub_y : 0;
        dst_data[i] = dst->data[i];
        if (dst_data[i])
            dst_data[i] += (y >> vshift) * dst->linesize[i];
    }

    ff_scale_internal(sws, (const uint8_t *const *) src->data, src->linesize,
                      0, graph->src.height, dst_data, dst->linesize, y, h);
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

static int init_fallback(SwsGraph *graph)
{
    int sws_flags, sws_filter, slice_h, num_slices, ret;

    SwsContext2 *const ctx     = graph->ctx;
    const SwsFormat *const src = &graph->src;
    const SwsFormat *const dst = &graph->dst;
    const SwsScaler scaler     = sws_get_scaler(ctx);
    const SwsScaler scaler_sub = sws_get_scaler_sub(ctx);
    const SwsDither dither     = sws_get_dither(ctx);
    const int dst_align        = sws_fmt_align(dst->format);
    const int src_align        = sws_fmt_align(src->format);
    const int align            = FFMAX(dst_align, src_align);
    SwsFilterOp *filter;
    SwsWrapper *wrapper;

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
    if (scaler_sub != SWS_SCALER_NEAREST)
        sws_flags |= SWS_FULL_CHR_H_INT;

    graph->incomplete |= src->range == AVCOL_RANGE_UNSPECIFIED;
    graph->incomplete |= dst->range == AVCOL_RANGE_UNSPECIFIED;

    slice_h = (dst->height + graph->num_threads - 1) / graph->num_threads;
    slice_h = FFALIGN(slice_h, align);
    num_slices = (dst->height + slice_h - 1) / slice_h;

    uninit_filters(graph);
    graph->num_filters = 1;

    filter             = &graph->filters[0];
    filter->new_w      = dst->width;
    filter->new_h      = dst->height;
    filter->slice_h    = slice_h;
    filter->num_slices = num_slices;
    filter->read       = read_fallback;
    filter->uninit     = uninit_fallback;
    filter->priv       = av_mallocz(sizeof(*wrapper) + num_slices * sizeof(SwsContext *));
    if (!filter->priv)
        return AVERROR(ENOMEM);

    wrapper = filter->priv;
    wrapper->filter = filter;
    wrapper->graph = graph;

    for (int i = 0; i < filter->num_slices; i++) {
        SwsContext *sws = wrapper->sws[i] = sws_alloc_context();
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

        /* Need to disable threading when using ED */
        if (sws->dither == SWS_DITHER_ED) {
            filter->num_slices = 1;
            filter->slice_h = dst->height;
            break;
        }
    }

    return 0;
}

static int read_input(SwsGraph *graph)
{
    return AVERROR(ENOTSUP);
}

static int scale_image(SwsGraph *graph)
{
    const SwsFormat *src = &graph->src;
    const SwsFormat *dst = &graph->dst;
    if (src->width == dst->width)
        goto vertical;

    return AVERROR(ENOTSUP);

vertical:
    if (src->height == dst->height)
        return 0;

    return AVERROR(ENOTSUP);
}

static int write_output(SwsGraph *graph)
{
    return AVERROR(ENOTSUP);
}

static int init_filters(SwsGraph *graph)
{
    int ret;

    if ((ret = read_input(graph)) < 0)
        goto error;
    if ((ret = scale_image(graph)) < 0)
        goto error;
    if ((ret = write_output(graph)) < 0)
        goto error;

    return 0;

error:
    if (ret == AVERROR(ENOTSUP))
        return init_fallback(graph);
    else
        return ret;
}

static void sws_graph_worker(void *priv, int jobnr, int threadnr, int nb_jobs,
                             int nb_threads)
{
    SwsGraph *graph = priv;
    const SwsFilterOp *filter = graph->exec.filter;
    const int w = filter->new_w, h = filter->new_h;
    const int slice_y = jobnr * filter->slice_h;
    const int slice_h = FFMIN(filter->slice_h, h - slice_y);
    sws_pixel_t *data;

    if (!filter->read) {
        /* Pure output pass */
        filter->write(slice_y, NULL, filter->priv);
        return;
    }

    data = filter->tmp + slice_y * filter->stride;
    filter->read(slice_y, data, filter->priv);

    /* Run post-processing in chunks */
    for (int i = 0; i < slice_h; i++) {
        for (int j = 0; j < filter->num_post_ops; j++) {
            const SwsPixelOp *op = &filter->post_ops[j];
            op->process(data + i * filter->stride, w, op->priv);
            /* TODO: subdivide further, based on benchmarking results */
        }
    }

    if (filter->write)
        filter->write(slice_y, data, filter->priv);
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

    ret = init_filters(graph);
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
    uninit_filters(graph);
    memset(graph, 0, sizeof(*graph));
}

void sws_graph_run(SwsGraph *graph, const SwsField *dst, const SwsField *src)
{
    graph->exec.src = src;
    graph->exec.dst = dst;

    for (int i = 0; i < graph->num_filters; i++) {
        const SwsFilterOp *filter = &graph->filters[i];
        graph->exec.filter = filter;
        avpriv_slicethread_execute(graph->slicethread, filter->num_slices, 0);
    }
}
