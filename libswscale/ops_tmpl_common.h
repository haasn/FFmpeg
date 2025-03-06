/**
 * Copyright (C) 2025 Niklas Haas
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

#ifndef SWSCALE_OPS_TMPL_COMMON_H
#define SWSCALE_OPS_TMPL_COMMON_H

#include <assert.h>
#include <float.h>

#include "libavutil/cpu.h"

#include "ops_internal.h"

#ifndef BIT_DEPTH
#  define BIT_DEPTH 8
#endif

#ifndef SCORE
#  define SCORE 0
#endif

#if BIT_DEPTH == 32
#  ifdef USE_FLOAT
#    define PIXEL_TYPE SWS_PIXEL_F32
#    define PIXEL_MAX  FLT_MAX
#    define pixel_t    float
#    define px         f32
#  else
#    define PIXEL_TYPE SWS_PIXEL_U32
#    define PIXEL_MAX  0xFFFFFFFFu
#    define SWAP_BYTES av_bswap32
#    define pixel_t    uint32_t
#    define px         u32
#  endif
#elif BIT_DEPTH == 16
#  define PIXEL_TYPE SWS_PIXEL_U16
#  define PIXEL_MAX  0xFFFFu
#  define SWAP_BYTES av_bswap16
#  define pixel_t    uint16_t
#  define px         u16
#elif BIT_DEPTH == 8
#  define PIXEL_TYPE SWS_PIXEL_U8
#  define PIXEL_MAX  0xFFu
#  define pixel_t    uint8_t
#  define px         u8
#else
#  error Invalid BIT_DEPTH
#endif

/* Clearing components */
typedef struct {
    pixel_t val[4];
} ClearCoeffs;

static av_always_inline void
clear(tmp_t *restrict out, const ClearCoeffs *restrict c,
      bool x, bool y, bool z, bool w)
{
    const pixel_t val0 = c->val[0];
    const pixel_t val1 = c->val[1];
    const pixel_t val2 = c->val[2];
    const pixel_t val3 = c->val[3];
    out = SWS_ASSUME_ALIGNED(out);

    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        if (x)
            out->x.px[i] = val0;
        if (y)
            out->y.px[i] = val1;
        if (z)
            out->z.px[i] = val2;
        if (w)
            out->w.px[i] = val3;
    }
}

static void *setup_clear(const SwsOp *ops)
{
    ClearCoeffs c = {0};

    for (int i = 0; i < 4; i++) {
        AVRational value = ops->clear.value[i];
        if (value.den != 0)
            c.val[i] = (pixel_t) av_q2d(value);
    }

    return av_memdup(&c, sizeof(c));
}

#define WRAP_CLEAR(X, Y, Z, W)                                                  \
static SWS_FUNC void                                                            \
clear##X##Y##Z##W(tmp_t *restrict inout, int y, const void *restrict priv)      \
{                                                                               \
    clear(inout, priv, X, Y, Z, W);                                             \
}                                                                               \
                                                                                \
static const SwsOpEntry op_clear##X##Y##Z##W = {                                \
    .num_ops = 1,                                                               \
    .ops = (const SwsOp[]) {{                                                   \
        .type  = PIXEL_TYPE,                                                    \
        .op    = SWS_OP_CLEAR,                                                  \
        .clear.value = { {0, X}, {0, Y}, {0, Z}, {0, W} },                      \
        .comps.unused = { X, Y, Z, W },                                         \
    }},                                                                         \
    .op    = clear##X##Y##Z##W,                                                 \
    .setup = setup_clear,                                                       \
};

WRAP_CLEAR(1, 0, 0, 0)
WRAP_CLEAR(0, 0, 0, 1)
WRAP_CLEAR(1, 1, 0, 0)
WRAP_CLEAR(0, 1, 1, 0)
WRAP_CLEAR(0, 0, 1, 1)
WRAP_CLEAR(1, 0, 1, 0)
WRAP_CLEAR(0, 1, 0, 1)
WRAP_CLEAR(0, 1, 1, 1)
WRAP_CLEAR(1, 0, 1, 1)
WRAP_CLEAR(1, 1, 0, 1)
WRAP_CLEAR(1, 1, 1, 1)

/* Pixel type conversions */
#define DEF_CONVERT(DEPTH)                                                      \
static av_always_inline void                                                    \
from##DEPTH(tmp_t *restrict out, const tmp_t *restrict in,                      \
            const bool x, const bool y, const bool z, const bool w)             \
{                                                                               \
    SWS_LOOP                                                                    \
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {                                  \
        if (x)                                                                  \
            out->x.px[i] = in->x.u##DEPTH[i];                                   \
        if (y)                                                                  \
            out->y.px[i] = in->y.u##DEPTH[i];                                   \
        if (z)                                                                  \
            out->z.px[i] = in->z.u##DEPTH[i];                                   \
        if (w)                                                                  \
            out->w.px[i] = in->w.u##DEPTH[i];                                   \
    }                                                                           \
}                                                                               \
                                                                                \
static av_always_inline void                                                    \
to##DEPTH(tmp_t *restrict out, const tmp_t *restrict in,                        \
          const bool x, const bool y, const bool z, const bool w)               \
{                                                                               \
    SWS_LOOP                                                                    \
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {                                  \
        if (x)                                                                  \
            out->x.u##DEPTH[i] = in->x.px[i];                                   \
        if (y)                                                                  \
            out->y.u##DEPTH[i] = in->y.px[i];                                   \
        if (z)                                                                  \
            out->z.u##DEPTH[i] = in->z.px[i];                                   \
        if (w)                                                                  \
            out->w.u##DEPTH[i] = in->w.px[i];                                   \
    }                                                                           \
}

DEF_CONVERT(8)
DEF_CONVERT(16)
DEF_CONVERT(32)

#define WRAP_CONVERT(DEPTH, X, Y, Z, W)                                         \
static SWS_FUNC void                                                            \
wrap_from##DEPTH##_##X##Y##Z##W(tmp_t *inout, int y, const void *priv)          \
{                                                                               \
    const tmp_t in = *inout;                                                    \
    from##DEPTH(inout, &in, X, Y, Z, W);                                        \
}                                                                               \
                                                                                \
static const SwsOpEntry op_from##DEPTH##_##X##Y##Z##W = {                       \
    .num_ops = 1,                                                               \
    .ops = (const SwsOp[]) {{                                                   \
        .type       = SWS_PIXEL_U##DEPTH,                                       \
        .op         = SWS_OP_CONVERT,                                           \
        .convert.to = PIXEL_TYPE,                                               \
        .comps.unused = { !X, !Y, !Z, !W },                                     \
    }},                                                                         \
    .op = wrap_from##DEPTH##_##X##Y##Z##W,                                      \
};                                                                              \
                                                                                \
static SWS_FUNC void                                                            \
wrap_to##DEPTH##_##X##Y##Z##W(tmp_t *inout, int y, const void *priv)            \
{                                                                               \
    const tmp_t in = *inout;                                                    \
    to##DEPTH(inout, &in, X, Y, Z, W);                                          \
}                                                                               \
                                                                                \
static const SwsOpEntry op_to##DEPTH##_##X##Y##Z##W = {                         \
    .num_ops = 1,                                                               \
    .ops = (const SwsOp[]) {{                                                   \
        .type       = PIXEL_TYPE,                                               \
        .op         = SWS_OP_CONVERT,                                           \
        .convert.to = SWS_PIXEL_U##DEPTH,                                       \
        .comps.unused = { !X, !Y, !Z, !W },                                     \
    }},                                                                         \
    .op = wrap_to##DEPTH##_##X##Y##Z##W,                                        \
};

WRAP_CONVERT(8,  1, 0, 0, 0)
WRAP_CONVERT(8,  1, 0, 0, 1)
WRAP_CONVERT(8,  1, 1, 1, 0)
WRAP_CONVERT(8,  1, 1, 1, 1)

WRAP_CONVERT(16, 1, 0, 0, 0)
WRAP_CONVERT(16, 1, 0, 0, 1)
WRAP_CONVERT(16, 1, 1, 1, 0)
WRAP_CONVERT(16, 1, 1, 1, 1)

WRAP_CONVERT(32, 1, 0, 0, 0)
WRAP_CONVERT(32, 1, 0, 0, 1)
WRAP_CONVERT(32, 1, 1, 1, 0)
WRAP_CONVERT(32, 1, 1, 1, 1)

typedef struct {
    pixel_t scale;
} ScaleCoefs;

static av_always_inline void
scale(tmp_t *restrict inout, const pixel_t scale,
      const bool x, const bool y, const bool z, const bool w)
{
    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        if (x)
            inout->x.px[i] *= scale;
        if (y)
            inout->y.px[i] *= scale;
        if (z)
            inout->z.px[i] *= scale;
        if (w)
            inout->w.px[i] *= scale;
    }
}

static void *setup_scale(const SwsOp *ops)
{
    ScaleCoefs c = { .scale = av_q2d(ops[0].scale.factor) };
    return av_memdup(&c, sizeof(c));
}

#define WRAP_SCALE(X, Y, Z, W)                                                  \
static SWS_FUNC void                                                            \
scale##X##Y##Z##W(tmp_t *restrict inout, int y, const void *restrict priv)      \
{                                                                               \
    const ScaleCoefs *restrict c = priv;                                        \
    scale(inout, c->scale, X, Y, Z, W);                                         \
}                                                                               \
                                                                                \
static const SwsOpEntry op_scale##X##Y##Z##W = {                                \
    .num_ops = 1,                                                               \
    .ops = (const SwsOp[]) {{                                                   \
        .type = PIXEL_TYPE,                                                     \
        .op   = SWS_OP_SCALE,                                                   \
        .comps.unused = { !X, !Y, !Z, !W },                                     \
    }},                                                                         \
    .op = scale##X##Y##Z##W,                                                    \
    .setup = setup_scale,                                                       \
};

WRAP_SCALE(1, 0, 0, 0)
WRAP_SCALE(1, 0, 0, 1)
WRAP_SCALE(1, 1, 1, 0)
WRAP_SCALE(1, 1, 1, 1)

#define OPS_COMMON      \
    op_scale1000,       \
    op_scale1001,       \
    op_scale1110,       \
    op_scale1111,       \
                        \
    op_clear1000,       \
    op_clear0001,       \
    op_clear1100,       \
    op_clear0110,       \
    op_clear0011,       \
    op_clear1010,       \
    op_clear0101,       \
    op_clear0111,       \
    op_clear1011,       \
    op_clear1101,

#endif
