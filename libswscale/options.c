/*
 * Copyright (C) 2024 Niklas Haas
 * Copyright (C) 2001-2003 Michael Niedermayer <michaelni@gmx.at>
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

#include "libavutil/opt.h"
#include "swscale.h"
#include "swscale_internal.h"

#define OFFSET(x) offsetof(SwsContext2, x)
#define DEFAULT 0
#define VE AV_OPT_FLAG_VIDEO_PARAM | AV_OPT_FLAG_ENCODING_PARAM

FF_DISABLE_DEPRECATION_WARNINGS
static const AVOption swscale_options2[] = {
    { "scale_flags",  "avscale flags",  OFFSET(flags), AV_OPT_TYPE_FLAGS, { .i64 = 0 }, 0, UINT_MAX,    .flags = VE, .unit = "scale_flags" },
        { "bitexact", "bit-exact mode", 0,             AV_OPT_TYPE_CONST, { .i64 = SWS_FLAG_BITEXACT }, .flags = VE, .unit = "scale_flags" },
        { "strict",   "strict mode",    0,             AV_OPT_TYPE_CONST, { .i64 = SWS_FLAG_STRICT },   .flags = VE, .unit = "scale_flags" },

    { "threads",  "number of threads",   OFFSET(threads), AV_OPT_TYPE_INT,   { .i64 = 1 }, 0, INT_MAX, .flags = VE, .unit = "threads" },
        { "auto", "automatic selection", 0,               AV_OPT_TYPE_CONST, { .i64 = 0 },             .flags = VE, .unit = "threads" },

    { "quality", "quality preset", OFFSET(quality), AV_OPT_TYPE_INT,  { .i64 = 0 },               .flags = VE, .unit = "preset", .max = SWS_Q_MAX },
        { "none",      "",        0,              AV_OPT_TYPE_CONST,  { .i64 = SWS_Q_NONE },      .flags = VE, .unit = "preset" },
        { "ultrafast", "",        0,              AV_OPT_TYPE_CONST,  { .i64 = SWS_Q_ULTRAFAST }, .flags = VE, .unit = "preset" },
        { "superfast", "",        0,              AV_OPT_TYPE_CONST,  { .i64 = SWS_Q_SUPERFAST }, .flags = VE, .unit = "preset" },
        { "veryfast",  "",        0,              AV_OPT_TYPE_CONST,  { .i64 = SWS_Q_VERYFAST },  .flags = VE, .unit = "preset" },
        { "faster",    "",        0,              AV_OPT_TYPE_CONST,  { .i64 = SWS_Q_FASTER },    .flags = VE, .unit = "preset" },
        { "fast",      "",        0,              AV_OPT_TYPE_CONST,  { .i64 = SWS_Q_FAST },      .flags = VE, .unit = "preset" },
        { "medium",    "",        0,              AV_OPT_TYPE_CONST,  { .i64 = SWS_Q_MEDIUM },    .flags = VE, .unit = "preset" },
        { "slow",      "",        0,              AV_OPT_TYPE_CONST,  { .i64 = SWS_Q_SLOW },      .flags = VE, .unit = "preset" },
        { "slower",    "",        0,              AV_OPT_TYPE_CONST,  { .i64 = SWS_Q_SLOWER },    .flags = VE, .unit = "preset" },
        { "veryslow",  "",        0,              AV_OPT_TYPE_CONST,  { .i64 = SWS_Q_VERYSLOW },  .flags = VE, .unit = "preset" },
        { "placebo",   "",        0,              AV_OPT_TYPE_CONST,  { .i64 = SWS_Q_PLACEBO },   .flags = VE, .unit = "preset" },

    { "scaler",       "scaling filter",         OFFSET(scaler),     AV_OPT_TYPE_INT,   { .i64 = SWS_SCALER_AUTO     }, .flags = VE, .unit = "filter", .max = SWS_SCALER_NB-1 },
    { "scaler_sub",   "subsampling filter",     OFFSET(scaler_sub), AV_OPT_TYPE_INT,   { .i64 = SWS_SCALER_AUTO     }, .flags = VE, .unit = "filter", .max = SWS_SCALER_NB-1 },
        { "auto",     "automatic selection",    0,                  AV_OPT_TYPE_CONST, { .i64 = SWS_SCALER_AUTO     }, .flags = VE, .unit = "filter" },
        { "nearest",  "nearest neighbour",      0,                  AV_OPT_TYPE_CONST, { .i64 = SWS_SCALER_NEAREST  }, .flags = VE, .unit = "filter" },
        { "bilinear", "bilinear filtering",     0,                  AV_OPT_TYPE_CONST, { .i64 = SWS_SCALER_BILINEAR }, .flags = VE, .unit = "filter" },
        { "bicubic",  "2-tap cubic B-spline",   0,                  AV_OPT_TYPE_CONST, { .i64 = SWS_SCALER_BICUBIC  }, .flags = VE, .unit = "filter" },
        { "gaussian", "gaussian approximation", 0,                  AV_OPT_TYPE_CONST, { .i64 = SWS_SCALER_GAUSSIAN }, .flags = VE, .unit = "filter" },
        { "lanczos",  "3-tap sinc/sinc",        0,                  AV_OPT_TYPE_CONST, { .i64 = SWS_SCALER_LANCZOS  }, .flags = VE, .unit = "filter" },

    { "dither",       "dither mode",                OFFSET(dither), AV_OPT_TYPE_INT,   { .i64 = SWS_DITHER_AUTO },    .flags = VE, .unit = "dither", .max = SWS_DITHER_NB-1 },
        { "auto",     "automatic selection",        0,              AV_OPT_TYPE_CONST, { .i64 = SWS_DITHER_AUTO },    .flags = VE, .unit = "dither" },
        { "none",     "no dithering",               0,              AV_OPT_TYPE_CONST, { .i64 = SWS_DITHER_NONE },    .flags = VE, .unit = "dither" },
        { "bayer",    "ordered matrix dither",      0,              AV_OPT_TYPE_CONST, { .i64 = SWS_DITHER_BAYER },   .flags = VE, .unit = "dither" },
        { "full",     "full error diffusion",       0,              AV_OPT_TYPE_CONST, { .i64 = SWS_DITHER_ED },      .flags = VE, .unit = "dither" },
        { "a_dither", "arithmetic addition dither", 0,              AV_OPT_TYPE_CONST, { .i64 = SWS_DITHER_A_DITHER}, .flags = VE, .unit = "dither" },
        { "x_dither", "arithmetic xor dither",      0,              AV_OPT_TYPE_CONST, { .i64 = SWS_DITHER_X_DITHER}, .flags = VE, .unit = "dither" },

    { "alphablend",          "mode for alpha -> non alpha",   OFFSET(alpha_blend), AV_OPT_TYPE_INT,    { .i64  = SWS_ALPHA_BLEND_NONE},         .flags = VE, .unit = "alphablend", .max = SWS_ALPHA_BLEND_NB-1 },
        { "none",            "ignore alpha",                  0,                   AV_OPT_TYPE_CONST,  { .i64  = SWS_ALPHA_BLEND_NONE},         .flags = VE, .unit = "alphablend" },
        { "uniform_color",   "blend onto a uniform color",    0,                   AV_OPT_TYPE_CONST,  { .i64  = SWS_ALPHA_BLEND_UNIFORM},      .flags = VE, .unit = "alphablend" },
        { "checkerboard",    "blend onto a checkerboard",     0,                   AV_OPT_TYPE_CONST,  { .i64  = SWS_ALPHA_BLEND_CHECKERBOARD}, .flags = VE, .unit = "alphablend" },

    { "sws_flags",          "swscale flags", OFFSET(sws_flags),  AV_OPT_TYPE_FLAGS, { .i64 = SWS_BICUBIC },        .flags = VE, .unit = "sws_flags", .max = UINT_MAX },
        { "fast_bilinear",  "fast bilinear",                 0,  AV_OPT_TYPE_CONST, { .i64 = SWS_FAST_BILINEAR  }, .flags = VE, .unit = "sws_flags" },
        { "bilinear",       "bilinear",                      0,  AV_OPT_TYPE_CONST, { .i64 = SWS_BILINEAR       }, .flags = VE, .unit = "sws_flags" },
        { "bicubic",        "bicubic",                       0,  AV_OPT_TYPE_CONST, { .i64 = SWS_BICUBIC        }, .flags = VE, .unit = "sws_flags" },
        { "experimental",   "experimental",                  0,  AV_OPT_TYPE_CONST, { .i64 = SWS_X              }, .flags = VE, .unit = "sws_flags" },
        { "neighbor",       "nearest neighbor",              0,  AV_OPT_TYPE_CONST, { .i64 = SWS_POINT          }, .flags = VE, .unit = "sws_flags" },
        { "area",           "averaging area",                0,  AV_OPT_TYPE_CONST, { .i64 = SWS_AREA           }, .flags = VE, .unit = "sws_flags" },
        { "bicublin",       "luma bicubic, chroma bilinear", 0,  AV_OPT_TYPE_CONST, { .i64 = SWS_BICUBLIN       }, .flags = VE, .unit = "sws_flags" },
        { "gauss",          "Gaussian",                      0,  AV_OPT_TYPE_CONST, { .i64 = SWS_GAUSS          }, .flags = VE, .unit = "sws_flags" },
        { "sinc",           "sinc",                          0,  AV_OPT_TYPE_CONST, { .i64 = SWS_SINC           }, .flags = VE, .unit = "sws_flags" },
        { "lanczos",        "Lanczos",                       0,  AV_OPT_TYPE_CONST, { .i64 = SWS_LANCZOS        }, .flags = VE, .unit = "sws_flags" },
        { "spline",         "natural bicubic spline",        0,  AV_OPT_TYPE_CONST, { .i64 = SWS_SPLINE         }, .flags = VE, .unit = "sws_flags" },
        { "print_info",     "print info",                    0,  AV_OPT_TYPE_CONST, { .i64 = SWS_PRINT_INFO     }, .flags = VE, .unit = "sws_flags" },
        { "accurate_rnd",   "accurate rounding",             0,  AV_OPT_TYPE_CONST, { .i64 = SWS_ACCURATE_RND   }, .flags = VE, .unit = "sws_flags" },
        { "full_chroma_int","full chroma interpolation",     0,  AV_OPT_TYPE_CONST, { .i64 = SWS_FULL_CHR_H_INT }, .flags = VE, .unit = "sws_flags" },
        { "full_chroma_inp","full chroma input",             0,  AV_OPT_TYPE_CONST, { .i64 = SWS_FULL_CHR_H_INP }, .flags = VE, .unit = "sws_flags" },
        { "bitexact",       "bit-exact mode",                0,  AV_OPT_TYPE_CONST, { .i64 = SWS_BITEXACT       }, .flags = VE, .unit = "sws_flags" },
        { "error_diffusion","error diffusion dither",        0,  AV_OPT_TYPE_CONST, { .i64 = SWS_ERROR_DIFFUSION}, .flags = VE, .unit = "sws_flags" },

    { NULL }
};
FF_ENABLE_DEPRECATION_WARNINGS

static const AVClass ff_swscale_class2 = {
    .class_name = "swscaler",
    .item_name  = av_default_item_name,
    .option     = swscale_options2,
    .version    = LIBAVUTIL_VERSION_INT,
    .category   = AV_CLASS_CATEGORY_SWSCALER,
};

const AVClass *sws_get_class2(void)
{
    return &ff_swscale_class2;
}

#undef OFFSET
#define OFFSET(x) offsetof(SwsContext, x)

static const AVOption swscale_options[] = {
    { "sws_flags",       "scaler flags",                  OFFSET(flags),     AV_OPT_TYPE_FLAGS,  { .i64  = SWS_BICUBIC        }, 0,      UINT_MAX,        VE, .unit = "sws_flags" },
    { "fast_bilinear",   "fast bilinear",                 0,                 AV_OPT_TYPE_CONST,  { .i64  = SWS_FAST_BILINEAR  }, INT_MIN, INT_MAX,        VE, .unit = "sws_flags" },
    { "bilinear",        "bilinear",                      0,                 AV_OPT_TYPE_CONST,  { .i64  = SWS_BILINEAR       }, INT_MIN, INT_MAX,        VE, .unit = "sws_flags" },
    { "bicubic",         "bicubic",                       0,                 AV_OPT_TYPE_CONST,  { .i64  = SWS_BICUBIC        }, INT_MIN, INT_MAX,        VE, .unit = "sws_flags" },
    { "experimental",    "experimental",                  0,                 AV_OPT_TYPE_CONST,  { .i64  = SWS_X              }, INT_MIN, INT_MAX,        VE, .unit = "sws_flags" },
    { "neighbor",        "nearest neighbor",              0,                 AV_OPT_TYPE_CONST,  { .i64  = SWS_POINT          }, INT_MIN, INT_MAX,        VE, .unit = "sws_flags" },
    { "area",            "averaging area",                0,                 AV_OPT_TYPE_CONST,  { .i64  = SWS_AREA           }, INT_MIN, INT_MAX,        VE, .unit = "sws_flags" },
    { "bicublin",        "luma bicubic, chroma bilinear", 0,                 AV_OPT_TYPE_CONST,  { .i64  = SWS_BICUBLIN       }, INT_MIN, INT_MAX,        VE, .unit = "sws_flags" },
    { "gauss",           "Gaussian",                      0,                 AV_OPT_TYPE_CONST,  { .i64  = SWS_GAUSS          }, INT_MIN, INT_MAX,        VE, .unit = "sws_flags" },
    { "sinc",            "sinc",                          0,                 AV_OPT_TYPE_CONST,  { .i64  = SWS_SINC           }, INT_MIN, INT_MAX,        VE, .unit = "sws_flags" },
    { "lanczos",         "Lanczos",                       0,                 AV_OPT_TYPE_CONST,  { .i64  = SWS_LANCZOS        }, INT_MIN, INT_MAX,        VE, .unit = "sws_flags" },
    { "spline",          "natural bicubic spline",        0,                 AV_OPT_TYPE_CONST,  { .i64  = SWS_SPLINE         }, INT_MIN, INT_MAX,        VE, .unit = "sws_flags" },
    { "print_info",      "print info",                    0,                 AV_OPT_TYPE_CONST,  { .i64  = SWS_PRINT_INFO     }, INT_MIN, INT_MAX,        VE, .unit = "sws_flags" },
    { "accurate_rnd",    "accurate rounding",             0,                 AV_OPT_TYPE_CONST,  { .i64  = SWS_ACCURATE_RND   }, INT_MIN, INT_MAX,        VE, .unit = "sws_flags" },
    { "full_chroma_int", "full chroma interpolation",     0,                 AV_OPT_TYPE_CONST,  { .i64  = SWS_FULL_CHR_H_INT }, INT_MIN, INT_MAX,        VE, .unit = "sws_flags" },
    { "full_chroma_inp", "full chroma input",             0,                 AV_OPT_TYPE_CONST,  { .i64  = SWS_FULL_CHR_H_INP }, INT_MIN, INT_MAX,        VE, .unit = "sws_flags" },
    { "bitexact",        "",                              0,                 AV_OPT_TYPE_CONST,  { .i64  = SWS_BITEXACT       }, INT_MIN, INT_MAX,        VE, .unit = "sws_flags" },
    { "error_diffusion", "error diffusion dither",        0,                 AV_OPT_TYPE_CONST,  { .i64  = SWS_ERROR_DIFFUSION}, INT_MIN, INT_MAX,        VE, .unit = "sws_flags" },

    { "srcw",            "source width",                  OFFSET(srcW),      AV_OPT_TYPE_INT,    { .i64 = 16                 }, 1,       INT_MAX,        VE },
    { "srch",            "source height",                 OFFSET(srcH),      AV_OPT_TYPE_INT,    { .i64 = 16                 }, 1,       INT_MAX,        VE },
    { "dstw",            "destination width",             OFFSET(dstW),      AV_OPT_TYPE_INT,    { .i64 = 16                 }, 1,       INT_MAX,        VE },
    { "dsth",            "destination height",            OFFSET(dstH),      AV_OPT_TYPE_INT,    { .i64 = 16                 }, 1,       INT_MAX,        VE },
    { "src_format",      "source format",                 OFFSET(srcFormat), AV_OPT_TYPE_PIXEL_FMT,{ .i64 = DEFAULT          }, 0,       INT_MAX, VE },
    { "dst_format",      "destination format",            OFFSET(dstFormat), AV_OPT_TYPE_PIXEL_FMT,{ .i64 = DEFAULT          }, 0,       INT_MAX, VE },
    { "src_range",       "source is full range",          OFFSET(srcRange),  AV_OPT_TYPE_BOOL,   { .i64 = DEFAULT            }, 0,       1,              VE },
    { "dst_range",       "destination is full range",     OFFSET(dstRange),  AV_OPT_TYPE_BOOL,   { .i64 = DEFAULT            }, 0,       1,              VE },
    { "param0",          "scaler param 0",                OFFSET(param[0]),  AV_OPT_TYPE_DOUBLE, { .dbl = SWS_PARAM_DEFAULT  }, INT_MIN, INT_MAX,        VE },
    { "param1",          "scaler param 1",                OFFSET(param[1]),  AV_OPT_TYPE_DOUBLE, { .dbl = SWS_PARAM_DEFAULT  }, INT_MIN, INT_MAX,        VE },

    { "src_v_chr_pos",   "source vertical chroma position in luma grid/256"  ,      OFFSET(src_v_chr_pos), AV_OPT_TYPE_INT, { .i64 = -513 }, -513,      1024,             VE },
    { "src_h_chr_pos",   "source horizontal chroma position in luma grid/256",      OFFSET(src_h_chr_pos), AV_OPT_TYPE_INT, { .i64 = -513 }, -513,      1024,             VE },
    { "dst_v_chr_pos",   "destination vertical chroma position in luma grid/256"  , OFFSET(dst_v_chr_pos), AV_OPT_TYPE_INT, { .i64 = -513 }, -513,      1024,             VE },
    { "dst_h_chr_pos",   "destination horizontal chroma position in luma grid/256", OFFSET(dst_h_chr_pos), AV_OPT_TYPE_INT, { .i64 = -513 }, -513,      1024,             VE },

    { "sws_dither",      "set dithering algorithm",       OFFSET(dither),    AV_OPT_TYPE_INT,    { .i64  = SWS_DITHER_AUTO    }, 0,       SWS_DITHER_NB,  VE, .unit = "sws_dither" },
    { "auto",            "leave choice to sws",           0,                 AV_OPT_TYPE_CONST,  { .i64  = SWS_DITHER_AUTO    }, INT_MIN, INT_MAX,        VE, .unit = "sws_dither" },
    { "bayer",           "bayer dither",                  0,                 AV_OPT_TYPE_CONST,  { .i64  = SWS_DITHER_BAYER   }, INT_MIN, INT_MAX,        VE, .unit = "sws_dither" },
    { "ed",              "error diffusion",               0,                 AV_OPT_TYPE_CONST,  { .i64  = SWS_DITHER_ED      }, INT_MIN, INT_MAX,        VE, .unit = "sws_dither" },
    { "a_dither",        "arithmetic addition dither",    0,                 AV_OPT_TYPE_CONST,  { .i64  = SWS_DITHER_A_DITHER}, INT_MIN, INT_MAX,        VE, .unit = "sws_dither" },
    { "x_dither",        "arithmetic xor dither",         0,                 AV_OPT_TYPE_CONST,  { .i64  = SWS_DITHER_X_DITHER}, INT_MIN, INT_MAX,        VE, .unit = "sws_dither" },
    { "gamma",           "gamma correct scaling",         OFFSET(gamma_flag),AV_OPT_TYPE_BOOL,   { .i64  = 0                  }, 0,       1,              VE },
    { "alphablend",      "mode for alpha -> non alpha",   OFFSET(alphablend),AV_OPT_TYPE_INT,    { .i64  = SWS_ALPHA_BLEND_NONE}, 0,       SWS_ALPHA_BLEND_NB-1, VE, .unit = "alphablend" },
    { "none",            "ignore alpha",                  0,                 AV_OPT_TYPE_CONST,  { .i64  = SWS_ALPHA_BLEND_NONE}, INT_MIN, INT_MAX,       VE, .unit = "alphablend" },
    { "uniform_color",   "blend onto a uniform color",    0,                 AV_OPT_TYPE_CONST,  { .i64  = SWS_ALPHA_BLEND_UNIFORM},INT_MIN, INT_MAX,     VE, .unit = "alphablend" },
    { "checkerboard",    "blend onto a checkerboard",     0,                 AV_OPT_TYPE_CONST,  { .i64  = SWS_ALPHA_BLEND_CHECKERBOARD},INT_MIN, INT_MAX,     VE, .unit = "alphablend" },

    { "threads",         "number of threads",             OFFSET(nb_threads),   AV_OPT_TYPE_INT, {.i64 = 1 }, 0, INT_MAX, VE, .unit = "threads" },
        { "auto",        NULL,                            0,                  AV_OPT_TYPE_CONST, {.i64 = 0 },    .flags = VE, .unit = "threads" },

    { NULL }
};

static const char *sws_context_to_name(void *ptr)
{
    return "swscaler";
}

const AVClass ff_sws_context_class = {
    .class_name = "SWScaler",
    .item_name  = sws_context_to_name,
    .option     = swscale_options,
    .parent_log_context_offset = OFFSET(parent),
    .category   = AV_CLASS_CATEGORY_SWSCALER,
    .version    = LIBAVUTIL_VERSION_INT,
};

const AVClass *sws_get_class(void)
{
    return &ff_sws_context_class;
}
