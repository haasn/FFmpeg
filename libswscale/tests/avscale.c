/*
 * Copyright (C) 2024      Nikles Haas
 * Copyright (C) 2003-2011 Michael Niedermayer <michaelni@gmx.at>
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <stdarg.h>

#undef HAVE_AV_CONFIG_H
#include "libavutil/cpu.h"
#include "libavutil/pixdesc.h"
#include "libavutil/lfg.h"
#include "libavutil/sfc64.h"
#include "libavutil/frame.h"
#include "libavutil/pixfmt.h"
#include "libavutil/avassert.h"
#include "libavutil/macros.h"

#include "libswscale/avscale.h"
#include "libswscale/swscale.h"

enum {
    WIDTH  = 96,
    HEIGHT = 96,
};

struct options {
    enum AVPixelFormat src_fmt;
    enum AVPixelFormat dst_fmt;
};

struct mode {
    enum AVScaleFilter filter;
    enum AVDitherMode dither;
    enum AVScaleFlags flags;
};

static double prob = 1.0;
static FFSFC64 prng_state;

static int fmt_comps(enum AVPixelFormat fmt)
{
    const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(fmt);
    int comps = desc->nb_components >= 3 ? 0b111 : 0b1;
    if (desc->flags & AV_PIX_FMT_FLAG_ALPHA)
        comps |= 0b1000;
    return comps;
}

static void get_mse(int mse[4], const AVFrame *a, const AVFrame *b, int comps)
{
    av_assert1(a->format == AV_PIX_FMT_YUVA420P);
    av_assert1(b->format == a->format);
    av_assert1(b->width == a->width && b->height == a->height);

    for (int p = 0; p < 4; p++) {
        const int is_chroma = p == 1 || p == 2;
        const int stride_a = a->linesize[p];
        const int stride_b = b->linesize[p];
        const int w = (a->width + is_chroma) >> is_chroma;
        const int h = (a->height + is_chroma) >> is_chroma;
        uint64_t sum = 0;

        if (comps & (1 << p)) {
            for (int y = 0; y < h; y++) {
                for (int x = 0; x < w; x++) {
                    int d = a->data[p][y * stride_a + x] - b->data[p][y * stride_b + x];
                    sum += d * d;
                }
            }
        } else {
            const int ref = is_chroma ? 128 : 0xFF;
            for (int y = 0; y < h; y++) {
                for (int x = 0; x < w; x++) {
                    int d = a->data[p][y * stride_a + x] - ref;
                    sum += d * d;
                }
            }
        }

        mse[p] = sum / (w * h);
    }
}

static int run_test(AVScaleContext *ctx, enum AVPixelFormat src_fmt,
                    enum AVPixelFormat dst_fmt, int dst_w, int dst_h,
                    struct mode mode, const AVFrame *ref)
{
    AVFrame *src = NULL, *dst = NULL, *out = NULL;
    struct SwsContext *ctx_ref = NULL;
    AVScaleContext *ctx_test = NULL;
    int mse[4], ret = -1;

    if (ff_sfc64_get(&prng_state) > UINT64_MAX * prob)
        return 0;

    ctx_test = avscale_alloc_context();
    if (!ctx_test)
        goto error;
    ctx_test->flags  = mode.flags;
    ctx_test->filter = mode.filter;
    ctx_test->dither = mode.dither;

    src = av_frame_alloc();
    dst = av_frame_alloc();
    out = av_frame_alloc();
    if (!src || !dst || !out)
        goto error;

    av_frame_copy_props(src, ref);
    av_frame_copy_props(dst, ref);
    av_frame_copy_props(out, ref);
    src->width  = out->width  = ref->width;
    src->height = out->height = ref->height;
    out->format = ref->format;
    src->format = src_fmt;
    dst->format = dst_fmt;
    dst->width  = dst_w;
    dst->height = dst_h;

    if (avscale_frame(ctx, src, ref) < 0) {
        fprintf(stderr, "Failed %s ---> %s\n", av_get_pix_fmt_name(ref->format),
                av_get_pix_fmt_name(src->format));
        goto error;
    }

    printf(" %s %dx%d -> %s %3dx%3d, flags=%d filter=%d dither=%d, ",
           av_get_pix_fmt_name(src->format), src->width, src->height,
           av_get_pix_fmt_name(dst->format), dst->width, dst->height,
           mode.flags, mode.filter, mode.dither);
    fflush(stdout);

    if (avscale_frame(ctx_test, dst, src) < 0) {
        fprintf(stderr, "Failed %s ---> %s\n", av_get_pix_fmt_name(src->format),
                av_get_pix_fmt_name(dst->format));
        goto error;
    }

    if (avscale_frame(ctx, out, dst) < 0) {
        fprintf(stderr, "Failed %s ---> %s\n", av_get_pix_fmt_name(ref->format),
                av_get_pix_fmt_name(out->format));
        goto error;
    }

    get_mse(mse, out, ref, fmt_comps(src_fmt) & fmt_comps(dst_fmt));
    printf("MSE={%5d %5d %5d %5d}\n", mse[0], mse[1], mse[2], mse[3]);

    ret = 0; /* fall through */
 error:
    av_frame_free(&src);
    av_frame_free(&dst);
    av_frame_free(&out);
    avscale_free_context(&ctx_test);
    return ret;
}

static int run_all_tests(AVScaleContext *ctx, const AVFrame *ref,
                         struct options opts)
{
    const int dst_w[] = { WIDTH  - WIDTH  / 3, WIDTH,  WIDTH  + WIDTH  / 3 };
    const int dst_h[] = { HEIGHT - HEIGHT / 3, HEIGHT, HEIGHT + HEIGHT / 3 };
    enum AVPixelFormat src_fmt, dst_fmt;
    const struct mode modes[] = {
        {0},
    };

    for (src_fmt = opts.src_fmt != AV_PIX_FMT_NONE ? opts.src_fmt : 0;
         src_fmt < AV_PIX_FMT_NB; src_fmt++) {

        if (!avscale_test_format(src_fmt, 0) || !avscale_test_format(src_fmt, 1))
            continue;

        for (dst_fmt = opts.dst_fmt != AV_PIX_FMT_NONE ? opts.dst_fmt : 0;
             dst_fmt < AV_PIX_FMT_NB; dst_fmt++) {

            if (!avscale_test_format(dst_fmt, 0) || !avscale_test_format(dst_fmt, 1))
                continue;

            printf("%s -> %s\n", av_get_pix_fmt_name(src_fmt), av_get_pix_fmt_name(dst_fmt));
            fflush(stdout);

            for (int m = 0; m < FF_ARRAY_ELEMS(modes); m++)
                for (int w = 0; w < FF_ARRAY_ELEMS(dst_w); w++)
                    for (int h = 0; h < FF_ARRAY_ELEMS(dst_h); h++)
                        if (run_test(ctx, src_fmt, dst_fmt, dst_w[w], dst_h[h],
                                     modes[m], ref) < 0)
                            return -1;

            if (opts.dst_fmt != AV_PIX_FMT_NONE)
                break;
        }
        if (opts.src_fmt != AV_PIX_FMT_NONE)
            break;
    }

    return 0;
}

int main(int argc, char **argv)
{
    struct options opts = {
        .src_fmt = AV_PIX_FMT_NONE,
        .dst_fmt = AV_PIX_FMT_NONE,
    };

    AVScaleContext *ctx = NULL;
    AVFrame *rgb = NULL, *ref = NULL;
    AVLFG rand;
    int ret = -1;

    for (int i = 1; i < argc; i += 2) {
        if (!strcmp(argv[i], "-help") || !strcmp(argv[i], "--help")) {
            fprintf(stderr,
                    "ctx [options...]\n"
                    "   -help\n"
                    "       This text\n"
                    "   -p <percentage between 0 and 100>\n"
                    "       The percentage of tests or comparisons to perform. Doing all tests will take long and generate over a hundred MB text output\n"
                    "       It is often convenient to perform a random subset\n"
                    "   -dst <pixfmt>\n"
                    "       Only test the specified destination pixel format\n"
                    "   -src <pixfmt>\n"
                    "       Only test the specified source pixel format\n"
                    "   -cpuflags <cpuflags>\n"
                    "       Uses the specified cpuflags in the tests\n"
            );
            return 0;
        }
        if (argv[i][0] != '-' || i + 1 == argc)
            goto bad_option;
        if (!strcmp(argv[i], "-cpuflags")) {
            unsigned flags = av_get_cpu_flags();
            int res = av_parse_cpu_caps(&flags, argv[i + 1]);
            if (res < 0) {
                fprintf(stderr, "invalid cpu flags %s\n", argv[i + 1]);
                goto error;
            }
            av_force_cpu_flags(flags);
        } else if (!strcmp(argv[i], "-src")) {
            opts.src_fmt = av_get_pix_fmt(argv[i + 1]);
            if (opts.src_fmt == AV_PIX_FMT_NONE) {
                fprintf(stderr, "invalid pixel format %s\n", argv[i + 1]);
                goto error;
            }
        } else if (!strcmp(argv[i], "-dst")) {
            opts.dst_fmt = av_get_pix_fmt(argv[i + 1]);
            if (opts.dst_fmt == AV_PIX_FMT_NONE) {
                fprintf(stderr, "invalid pixel format %s\n", argv[i + 1]);
                goto error;
            }
        } else if (!strcmp(argv[i], "-p")) {
            prob = atof(argv[i + 1]) / 100.0;
        } else {
bad_option:
            fprintf(stderr, "bad option or argument missing (%s) see -help\n", argv[i]);
            goto error;
        }
    }

    ff_sfc64_init(&prng_state, 0, 0, 0, 12);
    av_lfg_init(&rand, 1);

    ctx = avscale_alloc_context();
    if (!ctx)
        goto error;
    ctx->filter = AV_SCALE_BILINEAR;

    rgb = av_frame_alloc();
    if (!rgb)
        goto error;
    rgb->width  = WIDTH  / 12;
    rgb->height = HEIGHT / 12;
    rgb->format = AV_PIX_FMT_RGBA;
    if (av_frame_get_buffer(rgb, 32) < 0)
        goto error;

    for (int y = 0; y < rgb->height; y++) {
        for (int x = 0; x < rgb->width; x++) {
            for (int c = 0; c < 4; c++)
                rgb->data[0][y * rgb->linesize[0] + x * 4 + c] = av_lfg_get(&rand);
        }
    }

    ref = av_frame_alloc();
    if (!ref)
        goto error;
    ref->width  = WIDTH;
    ref->height = HEIGHT;
    ref->format = AV_PIX_FMT_YUVA420P;

    if (avscale_frame(ctx, ref, rgb) < 0)
        goto error;

    if (run_all_tests(ctx, ref, opts) < 0)
        goto error;

    ret = 0; /* fall through */
error:
    avscale_free_context(&ctx);
    av_frame_free(&rgb);
    av_frame_free(&ref);
    return ret;
}
