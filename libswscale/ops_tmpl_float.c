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
#include "ops_backend.h"

#ifndef BIT_DEPTH
#  define BIT_DEPTH 32
#endif

#if BIT_DEPTH == 32
#  define PIXEL_TYPE SWS_PIXEL_F32
#  define PIXEL_MAX  FLT_MAX
#  define pixel_t    float
#else
#  error Invalid BIT_DEPTH
#endif

#define IS_FLOAT 1
#define FMT_CHAR f
#include "ops_tmpl_common.c"

#define clipf(x, min, max) fminf(fmaxf(x, min), max)

typedef struct {
    pixel_t max[4];
} fn(ClampCoeffs);

DECL_SETUP(clamp)
{
    fn(ClampCoeffs) c;

    for (int i = 0; i < 4; i++) {
        if (op->clamp.max[i].den)
            c.max[i] = av_q2d(op->clamp.max[i]);
        else
            c.max[i] = PIXEL_MAX;
    }

    return SETUP_MEMDUP(c);
}

DECL_FUNC_PATTERN(clamp)
{
    const fn(ClampCoeffs) c = *(const fn(ClampCoeffs) *) impl->priv;

    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        if (X)
            x[i] = clipf(x[i], 0, c.max[0]);
        if (Y)
            y[i] = clipf(y[i], 0, c.max[1]);
        if (Z)
            z[i] = clipf(z[i], 0, c.max[2]);
        if (W)
            w[i] = clipf(w[i], 0, c.max[3]);
    }

    CONTINUE(pixel_t *, x, y, z, w);
}

WRAP_COMMON_PATTERNS(clamp,
    .op.op = SWS_OP_CLAMP,
    .setup = fn(setup_clamp),
    .free  = av_free,
);

#define MAX_DITHER_SIZE 16
#if MAX_DITHER_SIZE > SWS_CHUNK_SIZE
#  define DITHER_ROW_SIZE MAX_DITHER_SIZE
#else
#  define DITHER_ROW_SIZE SWS_CHUNK_SIZE
#endif

typedef struct {
    pixel_t matrix[MAX_DITHER_SIZE][DITHER_ROW_SIZE];
} fn(DitherCoeffs);

DECL_SETUP(dither)
{
    fn(DitherCoeffs) c = {0};
    const int size = 1 << op->dither.size_log2;

    if (!size) {
        /* We special case this value */
        av_assert1(!av_cmp_q(op->dither.matrix[0], av_make_q(1, 2)));
        *out_priv = NULL;
        return 0;
    }

    for (int y = 0; y < size; y++) {
        for (int x = 0; x < size; x++)
            c.matrix[y][x] = av_q2d(op->dither.matrix[y * size + x]);
        for (int x = size; x < SWS_CHUNK_SIZE; x++)
            c.matrix[y][x] = c.matrix[y][x % size]; /* pad to chunk size */
    }

    return SETUP_MEMDUP(c);
}

DECL_FUNC(dither, const bool X, const bool Y, const bool Z, const bool W,
          const int size_log2)
{
    const fn(DitherCoeffs) *restrict c = impl->priv;
    const int mask = (1 << size_log2) - 1;
    const int y_line = exec->y;
    const int row0 = (y_line + 0) & mask;
    const int row1 = (y_line + 3) & mask;
    const int row2 = (y_line + 5) & mask;
    const int row3 = (y_line + 7) & mask;
    const int base = exec->x & (SWS_CHUNK_SIZE & (MAX_DITHER_SIZE - 1));

    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        if (X)
            x[i] += size_log2 ? c->matrix[row0][base + i] : (pixel_t) 0.5;
        if (Y)
            y[i] += size_log2 ? c->matrix[row1][base + i] : (pixel_t) 0.5;
        if (Z)
            z[i] += size_log2 ? c->matrix[row2][base + i] : (pixel_t) 0.5;
        if (W)
            w[i] += size_log2 ? c->matrix[row3][base + i] : (pixel_t) 0.5;
    }

    CONTINUE(pixel_t *, x, y, z, w);
}

#define WRAP_DITHER(N, X, Y, Z, W)                                              \
DECL_IMPL(dither_##N##_##X##Y##Z##W)                                            \
{                                                                               \
    CALL(dither, X, Y, Z, W, N);                                                \
}                                                                               \
                                                                                \
DECL_ENTRY(dither_##N##_##X##Y##Z##W,                                           \
    .op.op = SWS_OP_DITHER,                                                     \
    .setup = fn(setup_dither),                                                  \
    .free  = av_free,                                                           \
    .op.dither.size_log2 = N,                                                   \
    .op.comps.unused = { !X, !Y, !Z, !W },                                      \
);

WRAP_DITHER(0, 1, 0, 0, 0)
WRAP_DITHER(0, 1, 0, 0, 1)
WRAP_DITHER(0, 1, 1, 1, 0)
WRAP_DITHER(0, 1, 1, 1, 1)

WRAP_DITHER(4, 1, 0, 0, 0)
WRAP_DITHER(4, 1, 0, 0, 1)
WRAP_DITHER(4, 1, 1, 1, 0)
WRAP_DITHER(4, 1, 1, 1, 1)

typedef struct {
    /* Stored in split form for convenience */
    pixel_t m[4][4];
    pixel_t k[4];
} fn(LinCoeffs);

DECL_SETUP(linear)
{
    fn(LinCoeffs) c;

    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++)
            c.m[i][j] = av_q2pixel(op->lin.m[i][j]);
        c.k[i] = av_q2pixel(op->lin.m[i][4]);
    }

    return SETUP_MEMDUP(c);
}

/**
 * Fully general case for a 5x5 linear affine transformation. Should never be
 * called without constant `mask`. This function will compile down to the
 * appropriately optimized version for the required subset of operations when
 * called with a constant mask.
 */
DECL_FUNC(linear_mask, const uint32_t mask)
{
    const fn(LinCoeffs) c = *(const fn(LinCoeffs) *) impl->priv;

    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        const pixel_t xx = x[i];
        const pixel_t yy = y[i];
        const pixel_t zz = z[i];
        const pixel_t ww = w[i];

        x[i]  = (mask & SWS_MASK(0, 0))  ? c.m[0][0] * xx : xx;
        x[i] += (mask & SWS_MASK(0, 1))  ? c.m[0][1] * yy : 0;
        x[i] += (mask & SWS_MASK(0, 2))  ? c.m[0][2] * zz : 0;
        x[i] += (mask & SWS_MASK(0, 3))  ? c.m[0][3] * ww : 0;
        x[i] += (mask & SWS_MASK_OFF(0)) ? c.k[0] : 0;

        y[i]  = (mask & SWS_MASK(1, 0))  ? c.m[1][0] * xx : 0;
        y[i] += (mask & SWS_MASK(1, 1))  ? c.m[1][1] * yy : yy;
        y[i] += (mask & SWS_MASK(1, 2))  ? c.m[1][2] * zz : 0;
        y[i] += (mask & SWS_MASK(1, 3))  ? c.m[1][3] * ww : 0;
        y[i] += (mask & SWS_MASK_OFF(1)) ? c.k[1] : 0;

        z[i]  = (mask & SWS_MASK(2, 0))  ? c.m[2][0] * xx : 0;
        z[i] += (mask & SWS_MASK(2, 1))  ? c.m[2][1] * yy : 0;
        z[i] += (mask & SWS_MASK(2, 2))  ? c.m[2][2] * zz : zz;
        z[i] += (mask & SWS_MASK(2, 3))  ? c.m[2][3] * ww : 0;
        z[i] += (mask & SWS_MASK_OFF(2)) ? c.k[2] : 0;

        w[i]  = (mask & SWS_MASK(3, 0))  ? c.m[3][0] * xx : 0;
        w[i] += (mask & SWS_MASK(3, 1))  ? c.m[3][1] * yy : 0;
        w[i] += (mask & SWS_MASK(3, 2))  ? c.m[3][2] * zz : 0;
        w[i] += (mask & SWS_MASK(3, 3))  ? c.m[3][3] * ww : ww;
        w[i] += (mask & SWS_MASK_OFF(3)) ? c.k[3] : 0;
    }

    CONTINUE(pixel_t *, x, y, z, w);
}

#define WRAP_LINEAR(NAME, MASK)                                                 \
DECL_IMPL(linear_##NAME)                                                        \
{                                                                               \
    CALL(linear_mask, MASK);                                                    \
}                                                                               \
                                                                                \
DECL_ENTRY(linear_##NAME,                                                       \
    .op.op = SWS_OP_LINEAR,                                                     \
    .setup = fn(setup_linear),                                                  \
    .free  = av_free,                                                           \
    .op.lin.mask = (MASK),                                                      \
    .op.comps.unused = {                                                        \
        !((MASK) & SWS_MASK_COL(0)),                                            \
        !((MASK) & SWS_MASK_COL(1)),                                            \
        !((MASK) & SWS_MASK_COL(2)),                                            \
        !((MASK) & SWS_MASK_COL(3)),                                            \
    },                                                                          \
);

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

static const SwsOpTable fn(op_table_float) = {
    .block_w = SWS_CHUNK_SIZE,
    .block_h = 1,
    .entries = {
        fn(op_convert_uint8_1000),
        fn(op_convert_uint8_1001),
        fn(op_convert_uint8_1110),
        fn(op_convert_uint8_1111),

        fn(op_convert_uint16_1000),
        fn(op_convert_uint16_1001),
        fn(op_convert_uint16_1110),
        fn(op_convert_uint16_1111),

        fn(op_convert_uint32_1000),
        fn(op_convert_uint32_1001),
        fn(op_convert_uint32_1110),
        fn(op_convert_uint32_1111),

        fn(op_clear_1110),

        fn(op_scale_1000),
        fn(op_scale_1001),
        fn(op_scale_1110),
        fn(op_scale_1111),

        fn(op_clamp_1000),
        fn(op_clamp_1001),
        fn(op_clamp_1110),
        fn(op_clamp_1111),

        fn(op_dither_0_1000),
        fn(op_dither_0_1001),
        fn(op_dither_0_1110),
        fn(op_dither_0_1111),

        fn(op_dither_4_1000),
        fn(op_dither_4_1001),
        fn(op_dither_4_1110),
        fn(op_dither_4_1111),

        fn(op_linear_luma),
        fn(op_linear_alpha),
        fn(op_linear_lumalpha),
        fn(op_linear_dot3),
        fn(op_linear_row0),
        fn(op_linear_row0a),
        fn(op_linear_diag3),
        fn(op_linear_diag4),
        fn(op_linear_diagoff3),
        fn(op_linear_matrix3),
        fn(op_linear_affine3),
        fn(op_linear_affine3a),

        {{0}}
    },
};

#undef PIXEL_TYPE
#undef PIXEL_MAX
#undef pixel_t

#undef FMT_CHAR
#undef IS_FLOAT
