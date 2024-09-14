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
#include "graph.h"

typedef struct AVScaleInternal {
    /* Currently active options  */
    AVScaleContext opts; /* (shallow) shadow copy of main context */
    AVScaleFormat src_fmt, dst_fmt;

    /* Active filter graph */
    AVScaleGraph graph[2]; /* top/prog, bottom */
} AVScaleInternal;

enum AVDitherMode avscale_default_dither(int preset)
{
    if (preset <= AV_SCALE_VERYFAST)
        return AV_DITHER_NONE;
    else if (preset <= AV_SCALE_SLOW)
        return AV_DITHER_BAYER;
    else
        return AV_DITHER_FULL;
}

enum AVScaleFilter avscale_default_filter(int preset)
{
    if (preset <= AV_SCALE_ULTRAFAST)
        return AV_SCALE_NEAREST;
    else if (preset <= AV_SCALE_FASTER)
        return AV_SCALE_BILINEAR;
    else if (preset <= AV_SCALE_MEDIUM)
        return AV_SCALE_BICUBIC;
    else
        return AV_SCALE_LANCZOS;
}

enum AVScaleFilter avscale_default_filter_sub(int preset)
{
    if (preset <= AV_SCALE_VERYFAST)
        return AV_SCALE_NEAREST;
    else if (preset <= AV_SCALE_FAST)
        return AV_SCALE_BILINEAR;
    else if (preset <= AV_SCALE_SLOWER)
        return AV_SCALE_BICUBIC;
    else
        return AV_SCALE_LANCZOS;
}

static void uninit(AVScaleContext *ctx)
{
    AVScaleInternal *s = ctx->internal;
    s->src_fmt = s->dst_fmt = (AVScaleFormat) { .format = AV_PIX_FMT_NONE };
    memset(&s->opts, 0, sizeof(s->opts));
    for (int i = 0; i < FF_ARRAY_ELEMS(s->graph); i++)
        avscale_graph_uninit(&s->graph[i]);
}

/**
 * This function also sanitizes and strips the input data, removing irrelevant
 * fields for certain formats */
static AVScaleFormat fmt_from_frame(const AVFrame *frame)
{
    const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(frame->format);
    AVScaleFormat fmt = {
        .width  = frame->width,
        .height = frame->height,
        .format = frame->format,
        .range  = frame->color_range,
        .prim   = frame->color_primaries,
        .trc    = frame->color_trc,
        .csp    = frame->colorspace,
        .loc    = frame->chroma_location,
        .desc   = desc,
    };

    av_assert1(fmt.width > 0);
    av_assert1(fmt.height > 0);
    av_assert1(fmt.format != AV_PIX_FMT_NONE);
    av_assert0(desc);
    if (desc->flags & (AV_PIX_FMT_FLAG_RGB | AV_PIX_FMT_FLAG_PAL | AV_PIX_FMT_FLAG_BAYER)) {
        /* RGB-like family */
        fmt.csp   = AVCOL_SPC_RGB;
        fmt.range = AVCOL_RANGE_JPEG;
    } else if (desc->flags & AV_PIX_FMT_FLAG_XYZ) {
        fmt.csp   = AVCOL_SPC_UNSPECIFIED;
        fmt.prim  = AVCOL_PRI_SMPTE428;
        fmt.trc   = AVCOL_TRC_SMPTE428;
    } else if (desc->nb_components < 3) {
        /* Grayscale formats */
        fmt.prim  = AVCOL_PRI_UNSPECIFIED;
        fmt.csp   = AVCOL_SPC_UNSPECIFIED;
        if (desc->flags & AV_PIX_FMT_FLAG_FLOAT)
            fmt.range = AVCOL_RANGE_UNSPECIFIED;
        else
            fmt.range = AVCOL_RANGE_JPEG; // FIXME: this restriction should be lifted
    }

    switch (frame->format) {
    case AV_PIX_FMT_YUVJ420P:
    case AV_PIX_FMT_YUVJ411P:
    case AV_PIX_FMT_YUVJ422P:
    case AV_PIX_FMT_YUVJ444P:
    case AV_PIX_FMT_YUVJ440P:
        fmt.range = AVCOL_RANGE_JPEG;
        break;
    }

    if (!desc->log2_chroma_w && !desc->log2_chroma_h)
        fmt.loc = AVCHROMA_LOC_UNSPECIFIED;

    if (frame->flags & AV_FRAME_FLAG_INTERLACED) {
        av_assert1(!(fmt.height & 1));
        fmt.height >>= 1;
        fmt.interlaced = 1;
    }

    return fmt;
}

int avscale_test_frame(const AVFrame *frame, int output)
{
    const AVScaleFormat fmt = fmt_from_frame(frame);
    return avscale_test_fmt(&fmt, output);
}

int avscale_is_noop(const AVFrame *dst, const AVFrame *src)
{
    AVScaleFormat dst_fmt = fmt_from_frame(dst);
    AVScaleFormat src_fmt = fmt_from_frame(src);
    return avscale_fmt_equal(&dst_fmt, &src_fmt);
}

AVScaleField avscale_get_field(const AVFrame *frame, int field)
{
    AVScaleField f = {
#define COPY4(x) { x[0], x[1], x[2], x[3] }
        .data     = COPY4(frame->data),
        .linesize = COPY4(frame->linesize),
    };

    if (!(frame->flags & AV_FRAME_FLAG_INTERLACED)) {
        av_assert1(!field);
        return f;
    }

    if (field == FIELD_BOTTOM) {
        /* Odd rows, offset by one line */
        const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(frame->format);
        for (int i = 0; i < FF_ARRAY_ELEMS(f.data); i++) {
            f.data[i] += f.linesize[i];
            if (desc->flags & AV_PIX_FMT_FLAG_PAL)
                break;
        }
    }

    /* Take only every second line */
    for (int i = 0; i < FF_ARRAY_ELEMS(f.linesize); i++)
        f.linesize[i] <<= 1;

    return f;
}

/* Subset of av_frame_ref() that only copies (video) data buffers */
static int frame_refcopy(AVFrame *dst, const AVFrame *src, int y, int h)
{
    int ret;

    /* TODO: handle hwframes? */

    /* copy frame data if it's not refcounted, or if dst buffers exist */
    if (!src->buf[0] || dst->buf[0]) {
        if (!dst->data[0]) {
            ret = av_frame_get_buffer(dst, 0);
            if (ret < 0)
                return ret;
        }
        ret = av_frame_copy(dst, src);
        if (ret < 0)
            return ret;
        return 0;
    }

    /* ref the buffers */
    for (int i = 0; i < FF_ARRAY_ELEMS(src->buf); i++) {
        if (!src->buf[i])
            continue;
        dst->buf[i] = av_buffer_ref(src->buf[i]);
        if (!dst->buf[i])
            return AVERROR(ENOMEM);
    }

    memcpy(dst->data,     src->data,     sizeof(src->data));
    memcpy(dst->linesize, src->linesize, sizeof(src->linesize));
    return 0;
}

int avscale_slice_alignment(const AVFrame *frame)
{
    const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(frame->format);
    int interlaced = !!(frame->flags & AV_FRAME_FLAG_INTERLACED);

    if (desc->flags & AV_PIX_FMT_FLAG_BAYER) {
        av_assert0(!interlaced); // FIXME
        return 2;
    } else {
        return 1 << (desc->log2_chroma_h + interlaced);
    }
}

int avscale_frame(AVScaleContext *ctx, AVFrame *dst, const AVFrame *src)
{
    if (!src)
        return AVERROR(EINVAL);

    return avscale_frame_slice(ctx, dst, src, 0, src->height);
}

int avscale_frame_slice(AVScaleContext *ctx, AVFrame *dst,
                        const AVFrame *src, int y, int h)
{
    AVScaleInternal *s = ctx->internal;
    int align, ret;
    if (!src || !dst)
        return AVERROR(EINVAL);

    /* Check slice bounds and alignment */
    if (y < 0 || h <= 0 || y + h > src->height) {
        av_log(ctx, AV_LOG_ERROR, "Slice [%d, %d) out of frame range [0, %d).\n",
               y, y + h, src->height);
        return AVERROR(EINVAL);
    }

    align = avscale_slice_alignment(src);
    if (y & align || h & align && h != src->height) {
        av_log(ctx, AV_LOG_ERROR, "Slice [%d, %d) not aligned to a multiple of "
               "%d for subsampled/interlaced frame\n", y, y + h, align);
        return AVERROR(EINVAL);
    }

    ret = avscale_frame_setup(ctx, dst, src);
    if (ret < 0)
        goto fail;

    if (src->data[0]) {
        if (avscale_fmt_equal(&s->dst_fmt, &s->src_fmt)) {
            ret = frame_refcopy(dst, src, y, h);
            if (ret < 0)
                goto fail;
        } else {
            if (!dst->data[0]) {
                ret = av_frame_get_buffer(dst, 0);
                if (ret < 0)
                    return ret;
            }

            for (int field = 0; field < 2; field++) {
                AVScaleField dst_field = avscale_get_field(dst, field);
                AVScaleField src_field = avscale_get_field(src, field);
                avscale_graph_run(&s->graph[field], &dst_field, &src_field,
                                  y >> s->src_fmt.interlaced,
                                  h >> s->src_fmt.interlaced);
                if (!s->src_fmt.interlaced)
                    break;
            }
        }
    }

    return 0;

fail:
    av_frame_unref(dst);
    return ret;
}

int avscale_frame_setup(AVScaleContext *ctx, const AVFrame *dst,
                        const AVFrame *src)
{
    AVScaleInternal *s = ctx->internal;
    AVScaleFormat src_fmt, dst_fmt;
    const char *err_msg;
    int ret;

    if (!src || !dst)
        return AVERROR(EINVAL);

    src_fmt = fmt_from_frame(src);
    dst_fmt = fmt_from_frame(dst);

    if ((src->flags ^ dst->flags) & AV_FRAME_FLAG_INTERLACED) {
        av_log(ctx, AV_LOG_ERROR, "Cannot convert interlaced to progressive "
                                  "frames or vice versa.\n");
        return AVERROR(EINVAL);
    }

    /* Try to re-use already initialized graph */
    if (avscale_fmt_equal(&dst_fmt, &s->dst_fmt) &&
        avscale_fmt_equal(&src_fmt, &s->src_fmt) &&
        avscale_opts_equal(ctx, &s->opts))
        return 0;

    /* TODO: test for underdefined values on AV_SCALE_STRICT */

    if (!avscale_test_fmt(&src_fmt, 0)) {
        err_msg = "Unsupported input";
        ret = AVERROR(ENOTSUP);
        goto fail;
    }

    if (!avscale_test_fmt(&dst_fmt, 1)) {
        err_msg = "Unsupported output";
        ret = AVERROR(ENOTSUP);
        goto fail;
    }

    /* TODO: remove once implemented */
    if (dst_fmt.prim != src_fmt.prim || dst_fmt.trc != src_fmt.trc) {
        av_log(ctx, AV_LOG_WARNING, "Conversions between different primaries / "
               "transfer functions are not currently implemented, expect "
               "wrong results.\n");
    }

    /* Re-initialize scaling graph */
    uninit(ctx);
    for (int field = 0; field < 2; field++) {
        ret = avscale_graph_init(ctx, &s->graph[field], &dst_fmt, &src_fmt, field);
        if (ret < 0) {
            err_msg = "Failed initializing scaling graph";
            goto fail;
        }

        if (!src_fmt.interlaced)
            break;
    }

    s->src_fmt = src_fmt;
    s->dst_fmt = dst_fmt;
    s->opts = *ctx;
    return 0;

fail:
    av_log(ctx, AV_LOG_ERROR, "%s (%s): fmt:%s csp:%s prim:%s trc:%s ->"
                                      " fmt:%s csp:%s prim:%s trc:%s\n",
           err_msg, av_err2str(ret),
           av_get_pix_fmt_name(src_fmt.format), av_color_space_name(src_fmt.csp),
           av_color_primaries_name(src_fmt.prim), av_color_transfer_name(src_fmt.trc),
           av_get_pix_fmt_name(dst_fmt.format), av_color_space_name(dst_fmt.csp),
           av_color_primaries_name(dst_fmt.prim), av_color_transfer_name(dst_fmt.trc));

    uninit(ctx);
    return ret;
}

#define OFFSET(x) offsetof(AVScaleContext, x)
#define VE AV_OPT_FLAG_VIDEO_PARAM | AV_OPT_FLAG_ENCODING_PARAM

FF_DISABLE_DEPRECATION_WARNINGS
static const AVOption avscale_options[] = {
    { "scale_flags",  "avscale flags",  OFFSET(flags), AV_OPT_TYPE_FLAGS, { .i64 = 0 }, 0, UINT_MAX,    .flags = VE, .unit = "scale_flags" },
        { "bitexact", "bit-exact mode", 0,             AV_OPT_TYPE_CONST, { .i64 = AV_SCALE_BITEXACT }, .flags = VE, .unit = "scale_flags" },
        { "strict",   "strict mode",    0,             AV_OPT_TYPE_CONST, { .i64 = AV_SCALE_STRICT },   .flags = VE, .unit = "scale_flags" },

    { "threads",  "number of threads",   OFFSET(threads), AV_OPT_TYPE_INT,   { .i64 = 1 }, 0, INT_MAX, .flags = VE, .unit = "threads" },
        { "auto", "automatic selection", 0,               AV_OPT_TYPE_CONST, { .i64 = 0 },             .flags = VE, .unit = "threads" },

    { "preset", "quality preset", OFFSET(preset), AV_OPT_TYPE_INT,    { .i64 = AV_SCALE_FAST },      .flags = VE, .unit = "preset",
                                  .min = AV_SCALE_ULTRAFAST, .max = AV_SCALE_PLACEBO },
        { "ultrafast", "",        0,              AV_OPT_TYPE_CONST,  { .i64 = AV_SCALE_ULTRAFAST }, .flags = VE, .unit = "preset" },
        { "superfast", "",        0,              AV_OPT_TYPE_CONST,  { .i64 = AV_SCALE_SUPERFAST }, .flags = VE, .unit = "preset" },
        { "veryfast",  "",        0,              AV_OPT_TYPE_CONST,  { .i64 = AV_SCALE_VERYFAST },  .flags = VE, .unit = "preset" },
        { "faster",    "",        0,              AV_OPT_TYPE_CONST,  { .i64 = AV_SCALE_FASTER },    .flags = VE, .unit = "preset" },
        { "fast",      "",        0,              AV_OPT_TYPE_CONST,  { .i64 = AV_SCALE_FAST },      .flags = VE, .unit = "preset" },
        { "medium",    "",        0,              AV_OPT_TYPE_CONST,  { .i64 = AV_SCALE_MEDIUM },    .flags = VE, .unit = "preset" },
        { "slow",      "",        0,              AV_OPT_TYPE_CONST,  { .i64 = AV_SCALE_SLOW },      .flags = VE, .unit = "preset" },
        { "slower",    "",        0,              AV_OPT_TYPE_CONST,  { .i64 = AV_SCALE_SLOWER },    .flags = VE, .unit = "preset" },
        { "veryslow",  "",        0,              AV_OPT_TYPE_CONST,  { .i64 = AV_SCALE_VERYSLOW },  .flags = VE, .unit = "preset" },
        { "placebo",   "",        0,              AV_OPT_TYPE_CONST,  { .i64 = AV_SCALE_PLACEBO },   .flags = VE, .unit = "preset" },

    { "dither",     "dither mode",              OFFSET(dither), AV_OPT_TYPE_INT,   { .i64 = AV_DITHER_AUTO },   .flags = VE, .unit = "dither", .max = AV_DITHER_FULL },
        { "auto",   "automatic selection",      0,              AV_OPT_TYPE_CONST, { .i64 = AV_DITHER_AUTO },   .flags = VE, .unit = "dither" },
        { "none",   "no dithering",             0,              AV_OPT_TYPE_CONST, { .i64 = AV_DITHER_NONE },   .flags = VE, .unit = "dither" },
        { "bayer",  "ordered matrix dither",    0,              AV_OPT_TYPE_CONST, { .i64 = AV_DITHER_BAYER },  .flags = VE, .unit = "dither" },
        { "full",   "full error diffusion",     0,              AV_OPT_TYPE_CONST, { .i64 = AV_DITHER_FULL },   .flags = VE, .unit = "dither" },

    { "filter",       "scaling filter",         OFFSET(filter),     AV_OPT_TYPE_INT,   { .i64 = AV_SCALE_AUTO     }, .flags = VE, .unit = "filter", .max = AV_SCALE_LANCZOS },
    { "filter_sub",   "subsampling filter",     OFFSET(filter_sub), AV_OPT_TYPE_INT,   { .i64 = AV_SCALE_AUTO     }, .flags = VE, .unit = "filter", .max = AV_SCALE_LANCZOS },
        { "auto",     "automatic selection",    0,                  AV_OPT_TYPE_CONST, { .i64 = AV_SCALE_AUTO     }, .flags = VE, .unit = "filter" },
        { "nearest",  "nearest neighour",       0,                  AV_OPT_TYPE_CONST, { .i64 = AV_SCALE_NEAREST  }, .flags = VE, .unit = "filter" },
        { "bilinear", "bilinear filtering",     0,                  AV_OPT_TYPE_CONST, { .i64 = AV_SCALE_BILINEAR }, .flags = VE, .unit = "filter" },
        { "bicubic",  "2-tap cubic B-spline",   0,                  AV_OPT_TYPE_CONST, { .i64 = AV_SCALE_BICUBIC  }, .flags = VE, .unit = "filter" },
        { "gaussian", "gaussian approximation", 0,                  AV_OPT_TYPE_CONST, { .i64 = AV_SCALE_GAUSSIAN }, .flags = VE, .unit = "filter" },
        { "lanczos",  "3-tap sinc/sinc",        0,                  AV_OPT_TYPE_CONST, { .i64 = AV_SCALE_LANCZOS  }, .flags = VE, .unit = "filter" },

    { "sws_flags",          "swscale flags", OFFSET(sws_flags),  AV_OPT_TYPE_FLAGS, { .i64 = 0 }, 0, UINT_MAX,      .flags = VE, .unit = "sws_flags" },
        { "fast_bilinear",  "fast bilinear",                 0,  AV_OPT_TYPE_CONST, { .i64  = SWS_FAST_BILINEAR  }, .flags = VE, .unit = "sws_flags" },
        { "bilinear",       "bilinear",                      0,  AV_OPT_TYPE_CONST, { .i64  = SWS_BILINEAR       }, .flags = VE, .unit = "sws_flags" },
        { "bicubic",        "bicubic",                       0,  AV_OPT_TYPE_CONST, { .i64  = SWS_BICUBIC        }, .flags = VE, .unit = "sws_flags" },
        { "experimental",   "experimental",                  0,  AV_OPT_TYPE_CONST, { .i64  = SWS_X              }, .flags = VE, .unit = "sws_flags" },
        { "neighbor",       "nearest neighbor",              0,  AV_OPT_TYPE_CONST, { .i64  = SWS_POINT          }, .flags = VE, .unit = "sws_flags" },
        { "area",           "averaging area",                0,  AV_OPT_TYPE_CONST, { .i64  = SWS_AREA           }, .flags = VE, .unit = "sws_flags" },
        { "bicublin",       "luma bicubic, chroma bilinear", 0,  AV_OPT_TYPE_CONST, { .i64  = SWS_BICUBLIN       }, .flags = VE, .unit = "sws_flags" },
        { "gauss",          "Gaussian",                      0,  AV_OPT_TYPE_CONST, { .i64  = SWS_GAUSS          }, .flags = VE, .unit = "sws_flags" },
        { "sinc",           "sinc",                          0,  AV_OPT_TYPE_CONST, { .i64  = SWS_SINC           }, .flags = VE, .unit = "sws_flags" },
        { "lanczos",        "Lanczos",                       0,  AV_OPT_TYPE_CONST, { .i64  = SWS_LANCZOS        }, .flags = VE, .unit = "sws_flags" },
        { "spline",         "natural bicubic spline",        0,  AV_OPT_TYPE_CONST, { .i64  = SWS_SPLINE         }, .flags = VE, .unit = "sws_flags" },
        { "print_info",     "print info",                    0,  AV_OPT_TYPE_CONST, { .i64  = SWS_PRINT_INFO     }, .flags = VE, .unit = "sws_flags" },
        { "accurate_rnd",   "accurate rounding",             0,  AV_OPT_TYPE_CONST, { .i64  = SWS_ACCURATE_RND   }, .flags = VE, .unit = "sws_flags" },
        { "full_chroma_int","full chroma interpolation",     0,  AV_OPT_TYPE_CONST, { .i64  = SWS_FULL_CHR_H_INT }, .flags = VE, .unit = "sws_flags" },
        { "full_chroma_inp","full chroma input",             0,  AV_OPT_TYPE_CONST, { .i64  = SWS_FULL_CHR_H_INP }, .flags = VE, .unit = "sws_flags" },
        { "bitexact",       "bit-exact mode",                0,  AV_OPT_TYPE_CONST, { .i64  = SWS_BITEXACT       }, .flags = VE, .unit = "sws_flags" },
        { "error_diffusion","error diffusion dither",        0,  AV_OPT_TYPE_CONST, { .i64  = SWS_ERROR_DIFFUSION}, .flags = VE, .unit = "sws_flags" },

    { NULL }
};
FF_ENABLE_DEPRECATION_WARNINGS

static const AVClass ff_avscale_class = {
    .class_name = "AVScale",
    .item_name  = av_default_item_name,
    .option     = avscale_options,
    .version    = LIBAVUTIL_VERSION_INT,
    .category   = AV_CLASS_CATEGORY_SWSCALER,
};

const AVClass *avscale_get_class(void)
{
    return &ff_avscale_class;
}

AVScaleContext *avscale_alloc_context(void)
{
    AVScaleInternal *s;
    AVScaleContext *ctx = av_mallocz(sizeof(*ctx));
    if (!ctx)
        return NULL;

    s = ctx->internal = av_mallocz(sizeof(*ctx->internal));
    if (!s) {
        av_free(ctx);
        return NULL;
    }

    ctx->av_class = &ff_avscale_class;
    av_opt_set_defaults(ctx);
    s->src_fmt.format = s->dst_fmt.format = AV_PIX_FMT_NONE;
    return ctx;
}

void avscale_free_context(AVScaleContext **pctx)
{
    AVScaleContext *ctx = *pctx;
    if (!ctx)
        return;

    uninit(ctx);
    av_free(ctx->internal);
    av_free(ctx);
    *pctx = NULL;
}
