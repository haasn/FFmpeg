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

#include "libavutil/avassert.h"
#include "libavutil/rational.h"

#define USE_FLOAT 1
#include "ops_tmpl_common.h"

#define clipf(x, min, max) fminf(fmaxf(x, min), max)

typedef struct DitherCoeffs {
    pixel_t matrix[SWS_MAX_DITHER][SWS_CHUNK_SIZE];
} DitherCoeffs;

static DitherCoeffs calc_dither(const SwsOp *op)
{
    DitherCoeffs c = {0};
    const int size = 1 << op->dither.size_log2;

    if (!size) {
        /* We special case this value */
        av_assert1(!av_cmp_q(op->dither.matrix[0], av_make_q(1, 2)));
        return c;
    }

    for (int y = 0; y < size; y++) {
        for (int x = 0; x < size; x++)
            c.matrix[y][x] = av_q2d(op->dither.matrix[y * size + x]);
        for (int x = size; x < SWS_CHUNK_SIZE; x++)
            c.matrix[y][x] = c.matrix[y][x % size]; /* pad to chunk size */
    }

    return c;
}

static void *setup_dither(const SwsOp *ops)
{
    DitherCoeffs c = calc_dither(&ops[0]);
    return av_memdup(&c, sizeof(c));
}

static av_always_inline void
dither(tmp_t *restrict inout, const int y_line, const DitherCoeffs *restrict c,
       const int size_log2)
{
    const int mask = (1 << size_log2) - 1;
    const int row0 = (y_line +  0) & mask;
    const int row1 = (y_line +  3) & mask;
    const int row2 = (y_line +  5) & mask;
    const int row3 = (y_line +  7) & mask;

    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        inout->x.px[i] += size_log2 ? c->matrix[row0][i] : (pixel_t) 0.5;
        inout->y.px[i] += size_log2 ? c->matrix[row1][i] : (pixel_t) 0.5;
        inout->z.px[i] += size_log2 ? c->matrix[row2][i] : (pixel_t) 0.5;
        inout->w.px[i] += size_log2 ? c->matrix[row3][i] : (pixel_t) 0.5;
    }
}

#define WRAP_DITHER(SIZE)                                                       \
static SWS_FUNC void                                                            \
dither##SIZE(tmp_t *restrict inout, int y, const void *restrict priv)           \
{                                                                               \
    static_assert(SIZE <= SWS_MAX_DITHER, "Dither matrix too large");           \
    dither(inout, y, priv, SIZE);                                               \
}                                                                               \
                                                                                \
static const SwsOpEntry op_dither##SIZE = {                                     \
    .num_ops = 1,                                                               \
    .ops = (const SwsOp[]) {{                                                   \
        .type = PIXEL_TYPE,                                                     \
        .op   = SWS_OP_DITHER,                                                  \
        .dither.size_log2 = SIZE,                                               \
    }},                                                                         \
    .op    = dither##SIZE,                                                      \
    .setup = setup_dither,                                                      \
};

WRAP_DITHER(0)
WRAP_DITHER(1)
WRAP_DITHER(2)
WRAP_DITHER(3)
WRAP_DITHER(4)

typedef struct {
    pixel_t max[4];
} ClampCoeffs;

static ClampCoeffs calc_clamp(const SwsOp *op)
{
    ClampCoeffs c;

    for (int i = 0; i < 4; i++) {
        if (op->clamp.max[i].den)
            c.max[i] = av_q2d(op->clamp.max[i]);
        else
            c.max[i] = PIXEL_MAX;
    }

    return c;
}

static void *setup_clamp(const SwsOp *ops)
{
    ClampCoeffs c = calc_clamp(&ops[0]);
    return av_memdup(&c, sizeof(c));
}

static av_always_inline void
clamp(tmp_t *restrict inout, const ClampCoeffs *restrict c,
      const bool x, const bool y, const bool z, const bool w)
{
    const pixel_t max0 = c->max[0];
    const pixel_t max1 = c->max[1];
    const pixel_t max2 = c->max[2];
    const pixel_t max3 = c->max[3];

    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        if (x)
            inout->x.px[i] = clipf(inout->x.px[i], 0, max0);
        if (y)
            inout->y.px[i] = clipf(inout->y.px[i], 0, max1);
        if (z)
            inout->z.px[i] = clipf(inout->z.px[i], 0, max2);
        if (w)
            inout->w.px[i] = clipf(inout->w.px[i], 0, max3);
    }
}

#define WRAP_CLAMP(X, Y, Z, W)                                                  \
static SWS_FUNC void                                                            \
clamp##X##Y##Z##W(tmp_t *restrict inout, int y, const void *restrict priv)      \
{                                                                               \
    clamp(inout, priv, X, Y, Z, W);                                             \
}                                                                               \
                                                                                \
static const SwsOpEntry op_clamp##X##Y##Z##W = {                                \
    .num_ops = 1,                                                               \
    .ops = (const SwsOp[]) {{                                                   \
        .type  = PIXEL_TYPE,                                                    \
        .op    = SWS_OP_CLAMP,                                                  \
        .comps.unused = { !X, !Y, !Z, !W },                                     \
    }},                                                                         \
    .op    = clamp##X##Y##Z##W,                                                 \
    .setup = setup_clamp,                                                       \
};

WRAP_CLAMP(1, 0, 0, 0)
WRAP_CLAMP(1, 0, 0, 1)
WRAP_CLAMP(1, 1, 0, 0)
WRAP_CLAMP(1, 1, 1, 0)
WRAP_CLAMP(1, 1, 1, 1)

/* Fused wrapper for the common dither+clamp+convert sequence */
typedef struct {
    DitherCoeffs dither;
    ClampCoeffs  clamp;
} OutputCoeffs;

static void *setup_output_coeffs(const SwsOp *ops)
{
    OutputCoeffs c = {
        .dither = calc_dither(&ops[0]),
        .clamp  = calc_clamp(&ops[1]),
    };

    return av_memdup(&c, sizeof(c));
}

static av_always_inline void
output8(tmp_t *restrict inout, const int y_line, const OutputCoeffs *restrict c,
        const int size, const bool x, const bool y, const bool z, const bool w)
{
    tmp_t tmp = *inout;
    dither(&tmp, y_line, &c->dither, size);
    clamp(&tmp, &c->clamp, x, y, z, w);

    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        if (x)
            inout->x.u8[i] = tmp.x.px[i];
        if (y)
            inout->y.u8[i] = tmp.y.px[i];
        if (z)
            inout->z.u8[i] = tmp.z.px[i];
        if (w)
            inout->w.u8[i] = tmp.w.px[i];
    }
}

static av_always_inline void
output16(tmp_t *restrict inout, const int y_line, const OutputCoeffs *restrict c,
         const int size, const bool x, const bool y, const bool z, const bool w)
{
    tmp_t tmp = *inout;
    dither(&tmp, y_line, &c->dither, size);
    clamp(&tmp, &c->clamp, x, y, z, w);

    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        if (x)
            inout->x.u16[i] = tmp.x.px[i];
        if (y)
            inout->y.u16[i] = tmp.y.px[i];
        if (z)
            inout->z.u16[i] = tmp.z.px[i];
        if (w)
            inout->w.u16[i] = tmp.w.px[i];
    }
}

#define WRAP_OUTPUT(DEPTH, SIZE, X, Y, Z, W)                                    \
static SWS_FUNC void                                                            \
output##DEPTH##_##SIZE##_##X##Y##Z##W(tmp_t *inout, int y, const void *priv)    \
{                                                                               \
    output##DEPTH(inout, y, priv, SIZE, X, Y, Z, W);                            \
}                                                                               \
                                                                                \
static const SwsOpEntry op_output##DEPTH##_##SIZE##_##X##Y##Z##W = {            \
    .num_ops = 3,                                                               \
    .ops = (const SwsOp[]) {{                                                   \
        .type = PIXEL_TYPE,                                                     \
        .op   = SWS_OP_DITHER,                                                  \
        .dither.size_log2 = SIZE,                                               \
        .comps.unused = { !X, !Y, !Z, !W },                                     \
    }, {                                                                        \
        .type = PIXEL_TYPE,                                                     \
        .op   = SWS_OP_CLAMP,                                                   \
        .comps.unused = { !X, !Y, !Z, !W },                                     \
    }, {                                                                        \
        .type = PIXEL_TYPE,                                                     \
        .op   = SWS_OP_CONVERT,                                                 \
        .convert.to = SWS_PIXEL_U##DEPTH,                                       \
        .comps.unused = { !X, !Y, !Z, !W },                                     \
    }},                                                                         \
    .op    = output##DEPTH##_##SIZE##_##X##Y##Z##W,                             \
    .setup = setup_output_coeffs,                                               \
};

WRAP_OUTPUT(8, 0, 1, 0, 0, 0)
WRAP_OUTPUT(8, 0, 1, 0, 0, 1)
WRAP_OUTPUT(8, 0, 1, 1, 1, 0)
WRAP_OUTPUT(8, 0, 1, 1, 1, 1)

WRAP_OUTPUT(8, 4, 1, 0, 0, 0)
WRAP_OUTPUT(8, 4, 1, 0, 0, 1)
WRAP_OUTPUT(8, 4, 1, 1, 1, 0)
WRAP_OUTPUT(8, 4, 1, 1, 1, 1)

WRAP_OUTPUT(16, 4, 1, 0, 0, 0)
WRAP_OUTPUT(16, 4, 1, 0, 0, 1)
WRAP_OUTPUT(16, 4, 1, 1, 1, 0)
WRAP_OUTPUT(16, 4, 1, 1, 1, 1)

WRAP_OUTPUT(16, 0, 1, 0, 0, 0)
WRAP_OUTPUT(16, 0, 1, 0, 0, 1)
WRAP_OUTPUT(16, 0, 1, 1, 1, 0)
WRAP_OUTPUT(16, 0, 1, 1, 1, 1)

typedef struct {
    /* Stored in split form for convenience */
    pixel_t m[4][4];
    pixel_t k[4];
} LinCoeffs;

static void *setup_linear_coeffs(const SwsOp *op)
{
    LinCoeffs c;
    while (op->op != SWS_OP_LINEAR)
        op++;

    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++)
            c.m[i][j] = av_q2d(op->lin.m[i][j]);
        c.k[i] = av_q2d(op->lin.m[i][4]);
    }

    return av_memdup(&c, sizeof(c));
}

/**
 * Fully general case for a 5x5 linear affine transformation. Should never be
 * called without constant `mask`. This function will compile down to the
 * appropriately optimized version for the required subset of operations when
 * called with a constant mask.
 */
static av_always_inline void
linear_mask(tmp_t *restrict inout, const LinCoeffs c, const uint32_t mask)
{
    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        const pixel_t x = inout->x.px[i];
        const pixel_t y = inout->y.px[i];
        const pixel_t z = inout->z.px[i];
        const pixel_t w = inout->w.px[i];

        inout->x.px[i] = ((mask & SWS_MASK(0, 0))  ? c.m[0][0] * x : x) +
                         ((mask & SWS_MASK(0, 1))  ? c.m[0][1] * y : 0) +
                         ((mask & SWS_MASK(0, 2))  ? c.m[0][2] * z : 0) +
                         ((mask & SWS_MASK(0, 3))  ? c.m[0][3] * w : 0) +
                         ((mask & SWS_MASK_OFF(0)) ? c.k[0] : 0);

        inout->y.px[i] = ((mask & SWS_MASK(1, 0))  ? c.m[1][0] * x : 0) +
                         ((mask & SWS_MASK(1, 1))  ? c.m[1][1] * y : y) +
                         ((mask & SWS_MASK(1, 2))  ? c.m[1][2] * z : 0) +
                         ((mask & SWS_MASK(1, 3))  ? c.m[1][3] * w : 0) +
                         ((mask & SWS_MASK_OFF(1)) ? c.k[1] : 0);

        inout->z.px[i] = ((mask & SWS_MASK(2, 0))  ? c.m[2][0] * x : 0) +
                         ((mask & SWS_MASK(2, 1))  ? c.m[2][1] * y : 0) +
                         ((mask & SWS_MASK(2, 2))  ? c.m[2][2] * z : z) +
                         ((mask & SWS_MASK(2, 3))  ? c.m[2][3] * w : 0) +
                         ((mask & SWS_MASK_OFF(2)) ? c.k[2] : 0);

        inout->w.px[i] = ((mask & SWS_MASK(3, 0))  ? c.m[3][0] * x : 0) +
                         ((mask & SWS_MASK(3, 1))  ? c.m[3][1] * y : 0) +
                         ((mask & SWS_MASK(3, 2))  ? c.m[3][2] * z : 0) +
                         ((mask & SWS_MASK(3, 3))  ? c.m[3][3] * w : w) +
                         ((mask & SWS_MASK_OFF(3)) ? c.k[3] : 0);
    }
}

#define WRAP_LINEAR(NAME, MASK)                                                 \
static av_always_inline void NAME(tmp_t *inout, const LinCoeffs c)              \
{                                                                               \
    linear_mask(inout, c, MASK);                                                \
}                                                                               \
                                                                                \
static SWS_FUNC void                                                            \
wrap_##NAME(tmp_t *restrict inout, int y, const void *restrict priv)            \
{                                                                               \
    NAME(inout, *(const LinCoeffs *) priv);                                     \
}                                                                               \
                                                                                \
static const SwsOpEntry op_##NAME = {                                           \
    .num_ops = 1,                                                               \
    .ops = (const SwsOp[]) {{                                                   \
        .type     = PIXEL_TYPE,                                                 \
        .op       = SWS_OP_LINEAR,                                              \
        .lin.mask = MASK,                                                       \
        .comps.unused = {                                                       \
            !((MASK) & SWS_MASK_COL(0)),                                        \
            !((MASK) & SWS_MASK_COL(1)),                                        \
            !((MASK) & SWS_MASK_COL(2)),                                        \
            !((MASK) & SWS_MASK_COL(3)),                                        \
        },                                                                      \
    }},                                                                         \
    .op    = wrap_##NAME,                                                       \
    .setup = setup_linear_coeffs,                                               \
};

WRAP_LINEAR(luma,      SWS_MASK_LUMA)
WRAP_LINEAR(alpha,     SWS_MASK_ALPHA)
WRAP_LINEAR(lumalpha,  SWS_MASK_LUMA | SWS_MASK_ALPHA)
WRAP_LINEAR(dot3,      0b111)
WRAP_LINEAR(row0,      SWS_MASK_ROW(0))
WRAP_LINEAR(row0a,     SWS_MASK_ROW(0) | SWS_MASK_ALPHA)
WRAP_LINEAR(diag3,     SWS_MASK_DIAG3)
WRAP_LINEAR(diag4,     SWS_MASK_DIAG4)
WRAP_LINEAR(diagoff3,  SWS_MASK_DIAG3 | SWS_MASK_OFF3)
WRAP_LINEAR(matrix3,   SWS_MASK_MAT3)
WRAP_LINEAR(affine3,   SWS_MASK_MAT3 | SWS_MASK_OFF3)
WRAP_LINEAR(affine3a,  SWS_MASK_MAT3 | SWS_MASK_OFF3 | SWS_MASK_ALPHA)

static const SwsOpEntry entries[] = {
    OPS_COMMON

    op_clamp1111,
    op_dither0,
    op_dither1,
    op_dither2,
    op_dither3,
    op_dither4,

    /* Pixel conversions */
    op_from8_1111,
    op_from16_1111,
    op_from32_1111,

    op_to8_1111,
    op_to16_1111,
    op_to32_1111,

    /* Linear operations. Sort these from fastest to slowest */
    op_luma,
    op_alpha,
    op_lumalpha,
    op_dot3,
    op_row0,
    op_row0a,
    op_diag3,
    op_diag4,
    op_diagoff3,
    op_matrix3,
    op_affine3,
    op_affine3a,

#if !CONFIG_SMALL
    /* Specific fast paths for fixed subsets */
    op_from8_1000,
    op_from8_1001,
    op_from8_1110,
    op_from16_1000,
    op_from16_1001,
    op_from16_1110,
    op_from32_1000,
    op_from32_1001,
    op_from32_1110,

    op_to8_1000,
    op_to8_1001,
    op_to8_1110,
    op_to16_1000,
    op_to16_1001,
    op_to16_1110,
    op_to32_1000,
    op_to32_1001,
    op_to32_1110,

    op_clamp1000,
    op_clamp1001,
    op_clamp1100,
    op_clamp1110,

    op_scale1000,
    op_scale1001,
    op_scale1110,

    /* Fused dither+clamp+convert wrappers */
    op_output8_0_1000,
    op_output8_0_1001,
    op_output8_0_1110,
    op_output8_0_1111,
    op_output8_4_1000,
    op_output8_4_1001,
    op_output8_4_1110,
    op_output8_4_1111,

    op_output16_0_1000,
    op_output16_0_1001,
    op_output16_0_1110,
    op_output16_0_1111,
    op_output16_4_1000,
    op_output16_4_1001,
    op_output16_4_1110,
    op_output16_4_1111,
#endif
};

const SwsOpTable bitfn(ff_sws_op_tmpl_float_table, BIT_DEPTH, SUFFIX) = {
    .cpu_flags   = CPU_FLAGS,
    .score       = SCORE,
    .entries     = entries,
    .num_entries = FF_ARRAY_ELEMS(entries),
};
