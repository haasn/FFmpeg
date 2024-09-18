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
#include "libavutil/imgutils_internal.h"
#include "libavutil/mem.h"
#include "libavutil/opt.h"
#include "libavutil/pixdesc.h"

#include "avscale_internal.h"
#include "libswscale/avscale.h"
#include "swscale_internal.h"
#include "graph.h"

int avscale_test_format(enum AVPixelFormat format, int output)
{
    return output ? sws_isSupportedOutput(format) : sws_isSupportedInput(format);
}

int avscale_test_colorspace(enum AVColorSpace csp, int output)
{
    switch (csp) {
    case AVCOL_SPC_UNSPECIFIED:
    case AVCOL_SPC_RGB:
    case AVCOL_SPC_BT709:
    case AVCOL_SPC_BT470BG:
    case AVCOL_SPC_SMPTE170M:
    case AVCOL_SPC_FCC:
    case AVCOL_SPC_SMPTE240M:
    case AVCOL_SPC_BT2020_NCL:
        return 1;
    default:
        return 0;
    }
}

int avscale_test_primaries(enum AVColorPrimaries prim, int output)
{
    return 1;
}

int avscale_test_transfer(enum AVColorTransferCharacteristic trc, int output)
{
    return 1;
}

int avscale_test_fmt(const AVScaleFormat *fmt, int output)
{
    return avscale_test_format    (fmt->format, output) &&
           avscale_test_colorspace(fmt->csp,    output) &&
           avscale_test_primaries (fmt->prim,   output) &&
           avscale_test_transfer  (fmt->trc,    output);
}

static void get_chroma_pos(int *h_chr_pos, int *v_chr_pos, int field,
                           const AVScaleFormat *fmt)
{
    enum AVChromaLocation chroma_loc = fmt->loc;
    const int sub_x = fmt->desc->log2_chroma_w;
    const int sub_y = fmt->desc->log2_chroma_h;
    int x_pos, y_pos;

    /* Explicitly default to center siting for compatibility with swscale */
    if (chroma_loc == AVCHROMA_LOC_UNSPECIFIED)
        chroma_loc = AVCHROMA_LOC_CENTER;

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
        if (field == FIELD_BOTTOM)
            y_pos += (256 << sub_y) - 256;

        /* Luma row distance is doubled for fields, so halve offsets */
        y_pos >>= 1;
    }

    /* Explicitly strip chroma offsets when not subsampling, because it
     * interferes with the operation of flags like SWS_FULL_CHR_H_INP */
    *h_chr_pos = sub_x ? x_pos : -513;
    *v_chr_pos = sub_y ? y_pos : -513;
}

static int init_fallback(AVScaleContext *ctx, AVScaleGraph *graph,
                         const AVScaleFormat *dst, const AVScaleFormat *src,
                         int field)
{
    enum AVDitherMode dither = avscale_get_dither(ctx);
    enum AVScaleFilter filter = avscale_get_filter(ctx);
    enum AVScaleFilter filter_sub = avscale_get_filter_sub(ctx);
    int sws_filter, ret;

    SwsContext *sws = graph->fallback = sws_alloc_context();
    if (!sws)
        return AVERROR(ENOMEM);

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

    AV_NOWARN_DEPRECATED({ sws->flags = ctx->sws_flags ^ sws_filter; })
    sws->nb_threads = ctx->threads;
    if (ctx->flags & AV_SCALE_BITEXACT)
        sws->flags |= SWS_BITEXACT | SWS_ACCURATE_RND;

    switch (filter) {
    case AV_SCALE_NEAREST:  sws_filter = SWS_POINT;    break;
    case AV_SCALE_BILINEAR: sws_filter = SWS_BILINEAR; break;
    case AV_SCALE_BICUBIC:  sws_filter = SWS_BICUBIC;  break;
    case AV_SCALE_GAUSSIAN: sws_filter = SWS_GAUSS;    break;
    case AV_SCALE_LANCZOS:  sws_filter = SWS_LANCZOS;  break;
    }

    if (filter == AV_SCALE_BICUBIC && filter_sub == AV_SCALE_BILINEAR)
        sws_filter = SWS_BICUBLIN;
    else if (filter_sub && filter_sub != filter) {
        av_log(ctx, AV_LOG_WARNING, "Subsampling filter ignored\n");
    }

    sws->flags |= sws_filter;

    // TODO: set correct dithering mode

    sws->srcW      = src->width;
    sws->srcH      = src->height;
    sws->srcFormat = src->format;
    sws->srcRange  = src->range == AVCOL_RANGE_JPEG;

    sws->dstW      = dst->width;
    sws->dstH      = dst->height;
    sws->dstFormat = dst->format;
    sws->dstRange  = dst->range == AVCOL_RANGE_JPEG;

    get_chroma_pos(&sws->src_h_chr_pos, &sws->src_v_chr_pos, field, src);
    get_chroma_pos(&sws->dst_h_chr_pos, &sws->dst_v_chr_pos, field, dst);

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

        sws_setColorspaceDetails(sws, inv_table, in_full, table, out_full,
                                 brightness, contrast, saturation);
    }

    return 0;
}

int avscale_graph_init(AVScaleContext *ctx, AVScaleGraph *graph,
                       const AVScaleFormat *dst, const AVScaleFormat *src,
                       int field)
{
    return init_fallback(ctx, graph, dst, src, field);
}

void avscale_graph_uninit(AVScaleGraph *graph)
{
    sws_freeContext(graph->fallback);
    graph->fallback = NULL;
}

void avscale_graph_run(AVScaleGraph *graph, const AVScaleField *dst,
                       const AVScaleField *src, int y, int h)
{
    if (graph->fallback) {
        sws_scale(graph->fallback, (const uint8_t **) src->data, src->linesize,
                  y, h, dst->data, dst->linesize);
        return;
    }
}
