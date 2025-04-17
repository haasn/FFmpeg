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

#include <float.h>

#include "../ops_internal.h"

#define DECL_ASM(TYPE, NAME, ...)                                               \
    void ff_##NAME(const SwsOpExec *, const SwsOpImpl *);                       \
    static const SwsOpEntry op_##NAME = {                                       \
        .func = ff_##NAME,                                                      \
        .op.type = SWS_PIXEL_##TYPE,                                            \
        __VA_ARGS__                                                             \
    }

#define DECL_PATTERN(TYPE, NAME, X, Y, Z, W, ...)                               \
    DECL_ASM(TYPE, p##X##Y##Z##W##_##NAME,                                      \
        .op.comps.unused = { !X, !Y, !Z, !W },                                  \
        __VA_ARGS__                                                             \
    )

#define REF_PATTERN(NAME, X, Y, Z, W)                                           \
    op_p##X##Y##Z##W##_##NAME

#define DECL_COMMON_PATTERNS(TYPE, NAME, ...)                                   \
    DECL_PATTERN(TYPE, NAME, 1, 0, 0, 0, __VA_ARGS__);                          \
    DECL_PATTERN(TYPE, NAME, 1, 0, 0, 1, __VA_ARGS__);                          \
    DECL_PATTERN(TYPE, NAME, 1, 1, 1, 0, __VA_ARGS__);                          \
    DECL_PATTERN(TYPE, NAME, 1, 1, 1, 1, __VA_ARGS__)                           \

#define REF_COMMON_PATTERNS(NAME)                                               \
    REF_PATTERN(NAME, 1, 0, 0, 0),                                              \
    REF_PATTERN(NAME, 1, 0, 0, 1),                                              \
    REF_PATTERN(NAME, 1, 1, 1, 0),                                              \
    REF_PATTERN(NAME, 1, 1, 1, 1)

#define DECL_RW(EXT, NAME, OP, ELEMS, PACKED)                                   \
    DECL_ASM(U8, NAME##ELEMS##EXT,                                              \
        .op.op = SWS_OP_##OP,                                                   \
        .op.rw = { .elems = ELEMS, .packed = PACKED },                          \
    );

static int setup_shuffle(const SwsOp *op, SwsOpPriv *out)
{
    for (int i = 0; i < 8; i++) {
        out->u8[i]     = op->shuffle.index[i];
        out->u8[i + 8] = op->shuffle.index[i] + 8;
    }
    return 0;
}

#define DECL_SHUFFLE(EXT)                                                       \
    DECL_COMMON_PATTERNS(U8, shuffle##EXT,                                      \
        .op.op = SWS_OP_SHUFFLE,                                                \
        .setup = setup_shuffle,                                                 \
        .flexible = true,                                                       \
    );

/* Don't use DECL_ASM because we want to re-use the same impl for all types */
#define DEF_CLEAR_ALPHA(EXT, IDX)                                               \
    void ff_clear_alpha##IDX##EXT(const SwsOpExec *, const SwsOpImpl *);

#define DECL_CLEAR_ALPHA(EXT, TYPE, IDX, VALUE)                                 \
    static const SwsOpEntry op_clear_alpha##IDX##_##TYPE##EXT = {               \
        .func = ff_clear_alpha##IDX##EXT,                                       \
        .op.type = SWS_PIXEL_##TYPE,                                            \
        .op.op = SWS_OP_CLEAR,                                                  \
        .op.c.q4[IDX] = { .num = VALUE, .den = 1 },                             \
        .op.comps.unused[IDX] = true,                                           \
    };

#define DECL_CLEAR_ZERO(EXT, IDX)                                               \
    DECL_ASM(U8, clear_zero##IDX##EXT,                                          \
        .op.op = SWS_OP_CLEAR,                                                  \
        .op.c.q4[IDX] = { .num = 0, .den = 1 },                                 \
        .op.comps.unused[IDX] = true,                                           \
    );

#define DECL_CLEAR(EXT, TYPE, SUFFIX, X, Y, Z, W)                               \
    DECL_PATTERN(TYPE, clear##SUFFIX##EXT, X, Y, Z, W,                          \
        .op.op = SWS_OP_CLEAR,                                                  \
        .setup = ff_sws_setup_q4,                                               \
        .flexible = true,                                                       \
    );

#define DECL_SWIZZLE(EXT, X, Y, Z, W)                                           \
    DECL_ASM(U8, swizzle_##X##Y##Z##W##EXT,                                     \
        .op.op = SWS_OP_SWIZZLE,                                                \
        .op.swizzle = SWS_SWIZZLE( X, Y, Z, W ),                                \
    );

#define DECL_CONVERT(EXT, FROM, TO)                                             \
    DECL_COMMON_PATTERNS(FROM, convert_##FROM##_##TO##EXT,                      \
        .op.op = SWS_OP_CONVERT,                                                \
        .op.convert.to = SWS_PIXEL_##TO,                                        \
    );

#define DECL_EXPAND(EXT, FROM, TO)                                              \
    DECL_COMMON_PATTERNS(FROM, expand_##FROM##_##TO##EXT,                       \
        .op.op = SWS_OP_CONVERT,                                                \
        .op.convert.to = SWS_PIXEL_##TO,                                        \
        .op.convert.expand = true,                                              \
    );

static int setup_shift(const SwsOp *op, SwsOpPriv *out)
{
    out->u16[0] = op->c.u;
    return 0;
}

#define DECL_SHIFT16(EXT)                                                       \
    DECL_COMMON_PATTERNS(U16, lshift16##EXT,                                    \
        .op.op = SWS_OP_LSHIFT,                                                 \
        .setup = setup_shift,                                                   \
    );                                                                          \
                                                                                \
    DECL_COMMON_PATTERNS(U16, rshift16##EXT,                                    \
        .op.op = SWS_OP_RSHIFT,                                                 \
        .setup = setup_shift,                                                   \
    );

#define DECL_MIN_MAX(EXT)                                                       \
    DECL_COMMON_PATTERNS(F32, min##EXT,                                         \
        .op.op = SWS_OP_MIN,                                                    \
        .setup = ff_sws_setup_q4,                                               \
        .flexible = true,                                                       \
    );                                                                          \
                                                                                \
    DECL_COMMON_PATTERNS(F32, max##EXT,                                         \
        .op.op = SWS_OP_MAX,                                                    \
        .setup = ff_sws_setup_q4,                                               \
        .flexible = true,                                                       \
    );

#define DECL_SCALE(EXT)                                                         \
    DECL_COMMON_PATTERNS(F32, scale##EXT,                                       \
        .op.op = SWS_OP_SCALE,                                                  \
        .setup = ff_sws_setup_q,                                                \
    );

/* 2x2 matrix fits inside SwsOpPriv directly, save an indirect in this case */
static_assert(sizeof(SwsOpPriv) >= sizeof(float[2][2]), "2x2 dither matrix too large");
static int setup_dither(const SwsOp *op, SwsOpPriv *out)
{
    const int size = 1 << op->dither.size_log2;
    float *matrix = out->f32;
    if (size > 2) {
        matrix = out->ptr = av_mallocz(size * size * sizeof(*matrix));
        if (!matrix)
            return AVERROR(ENOMEM);
    }

    for (int i = 0; i < size * size; i++)
        matrix[i] = (float) op->dither.matrix[i].num / op->dither.matrix[i].den;

    return 0;
}

#define DECL_DITHER(EXT, SIZE)                                                  \
    DECL_COMMON_PATTERNS(F32, dither##SIZE##EXT,                                \
        .op.op = SWS_OP_DITHER,                                                 \
        .op.dither.size_log2 = SIZE,                                            \
        .setup = setup_dither,                                                  \
        .free  = SIZE > 2 ? av_free : NULL,                                     \
    );

static int setup_linear(const SwsOp *op, SwsOpPriv *out)
{
    float *matrix = out->ptr = av_mallocz(sizeof(float[4][5]));
    if (!matrix)
        return AVERROR(ENOMEM);

    for (int y = 0; y < 4; y++) {
        for (int x = 0; x < 5; x++)
            matrix[y * 5 + x] = (float) op->lin.m[y][x].num / op->lin.m[y][x].den;
    }

    return 0;
}

#define DECL_LINEAR(EXT, NAME, MASK)                                            \
    DECL_ASM(F32, NAME##EXT,                                                    \
        .op.op = SWS_OP_LINEAR,                                                 \
        .op.lin.mask = (MASK),                                                  \
        .setup = setup_linear,                                                  \
        .free  = av_free,                                                       \
    );

#define DECL_FUNCS_8(SIZE, EXT, FLAG)                                           \
    DECL_RW(EXT, read_planar, READ, 1, false)                                   \
    DECL_RW(EXT, read_planar, READ, 2, false)                                   \
    DECL_RW(EXT, read_planar, READ, 3, false)                                   \
    DECL_RW(EXT, read_planar, READ, 4, false)                                   \
    DECL_RW(EXT, write_planar, WRITE, 1, false)                                 \
    DECL_RW(EXT, write_planar, WRITE, 2, false)                                 \
    DECL_RW(EXT, write_planar, WRITE, 3, false)                                 \
    DECL_RW(EXT, write_planar, WRITE, 4, false)                                 \
    DECL_RW(EXT, read8_packed, READ, 2, true)                                   \
    DECL_RW(EXT, read8_packed, READ, 3, true)                                   \
    DECL_RW(EXT, read8_packed, READ, 4, true)                                   \
    DECL_RW(EXT, write8_packed, WRITE, 2, true)                                 \
    DECL_RW(EXT, write8_packed, WRITE, 4, true)                                 \
    DECL_SHUFFLE(EXT)                                                           \
    DECL_SWIZZLE(EXT, 3, 0, 1, 2)                                               \
    DECL_SWIZZLE(EXT, 3, 0, 2, 1)                                               \
    DECL_SWIZZLE(EXT, 2, 1, 0, 3)                                               \
    DECL_SWIZZLE(EXT, 3, 2, 1, 0)                                               \
    DECL_SWIZZLE(EXT, 3, 1, 0, 2)                                               \
    DECL_SWIZZLE(EXT, 3, 2, 0, 1)                                               \
    DECL_SWIZZLE(EXT, 1, 2, 0, 3)                                               \
    DECL_SWIZZLE(EXT, 1, 0, 2, 3)                                               \
    DECL_SWIZZLE(EXT, 2, 0, 1, 3)                                               \
    DECL_SWIZZLE(EXT, 2, 3, 1, 0)                                               \
    DECL_SWIZZLE(EXT, 2, 1, 3, 0)                                               \
    DECL_SWIZZLE(EXT, 1, 2, 3, 0)                                               \
    DECL_SWIZZLE(EXT, 1, 3, 2, 0)                                               \
    DECL_SWIZZLE(EXT, 0, 2, 1, 3)                                               \
    DECL_SWIZZLE(EXT, 0, 2, 3, 1)                                               \
    DECL_SWIZZLE(EXT, 0, 3, 1, 2)                                               \
    DECL_SWIZZLE(EXT, 3, 1, 2, 0)                                               \
    DECL_SWIZZLE(EXT, 0, 3, 2, 1)                                               \
    DECL_SWIZZLE(EXT, 0, 0, 0, 3)                                               \
    DECL_SWIZZLE(EXT, 3, 0, 0, 0)                                               \
    DECL_SWIZZLE(EXT, 0, 0, 0, 1)                                               \
    DECL_SWIZZLE(EXT, 1, 0, 0, 0)                                               \
    DEF_CLEAR_ALPHA(EXT, 0)                                                     \
    DEF_CLEAR_ALPHA(EXT, 1)                                                     \
    DEF_CLEAR_ALPHA(EXT, 3)                                                     \
    DECL_CLEAR_ALPHA(EXT, U8, 0, 0xFF)                                          \
    DECL_CLEAR_ALPHA(EXT, U8, 1, 0xFF)                                          \
    DECL_CLEAR_ALPHA(EXT, U8, 3, 0xFF)                                          \
    DECL_CLEAR_ZERO(EXT, 0)                                                     \
    DECL_CLEAR_ZERO(EXT, 1)                                                     \
    DECL_CLEAR_ZERO(EXT, 3)                                                     \
    DECL_CLEAR(EXT, U8, b, 1, 1, 1, 0)                                          \
    DECL_CLEAR(EXT, U8, b, 0, 1, 1, 1)                                          \
    DECL_CLEAR(EXT, U8, b, 0, 0, 1, 1)                                          \
    DECL_CLEAR(EXT, U8, b, 1, 0, 0, 1)                                          \
    DECL_CLEAR(EXT, U8, b, 1, 1, 0, 0)                                          \
    DECL_CLEAR(EXT, U8, b, 0, 1, 0, 1)                                          \
    DECL_CLEAR(EXT, U8, b, 1, 0, 1, 0)                                          \
    DECL_CLEAR(EXT, U8, b, 1, 0, 0, 0)                                          \
    DECL_CLEAR(EXT, U8, b, 0, 1, 0, 0)                                          \
    DECL_CLEAR(EXT, U8, b, 0, 0, 1, 0)                                          \
                                                                                \
static const SwsOpTable ops8##EXT = {                                           \
    .cpu_flags = AV_CPU_FLAG_##FLAG,                                            \
    .block_w = SIZE,                                                            \
    .block_h = 1,                                                               \
    .entries = {                                                                \
        op_read_planar1##EXT,                                                   \
        op_read_planar2##EXT,                                                   \
        op_read_planar3##EXT,                                                   \
        op_read_planar4##EXT,                                                   \
        op_write_planar1##EXT,                                                  \
        op_write_planar2##EXT,                                                  \
        op_write_planar3##EXT,                                                  \
        op_write_planar4##EXT,                                                  \
        op_read8_packed2##EXT,                                                  \
        op_read8_packed3##EXT,                                                  \
        op_read8_packed4##EXT,                                                  \
        op_write8_packed2##EXT,                                                 \
        op_write8_packed4##EXT,                                                 \
        REF_COMMON_PATTERNS(shuffle##EXT),                                      \
        op_swizzle_3012##EXT,                                                   \
        op_swizzle_3021##EXT,                                                   \
        op_swizzle_2103##EXT,                                                   \
        op_swizzle_3210##EXT,                                                   \
        op_swizzle_3102##EXT,                                                   \
        op_swizzle_3201##EXT,                                                   \
        op_swizzle_1203##EXT,                                                   \
        op_swizzle_1023##EXT,                                                   \
        op_swizzle_2013##EXT,                                                   \
        op_swizzle_2310##EXT,                                                   \
        op_swizzle_2130##EXT,                                                   \
        op_swizzle_1230##EXT,                                                   \
        op_swizzle_1320##EXT,                                                   \
        op_swizzle_0213##EXT,                                                   \
        op_swizzle_0231##EXT,                                                   \
        op_swizzle_0312##EXT,                                                   \
        op_swizzle_3120##EXT,                                                   \
        op_swizzle_0321##EXT,                                                   \
        op_swizzle_0003##EXT,                                                   \
        op_swizzle_0001##EXT,                                                   \
        op_swizzle_3000##EXT,                                                   \
        op_swizzle_1000##EXT,                                                   \
        op_clear_alpha0_U8##EXT,                                                \
        op_clear_alpha1_U8##EXT,                                                \
        op_clear_alpha3_U8##EXT,                                                \
        op_clear_zero0##EXT,                                                    \
        op_clear_zero1##EXT,                                                    \
        op_clear_zero3##EXT,                                                    \
        REF_PATTERN(clearb##EXT, 1, 1, 1, 0),                                   \
        REF_PATTERN(clearb##EXT, 0, 1, 1, 1),                                   \
        REF_PATTERN(clearb##EXT, 0, 0, 1, 1),                                   \
        REF_PATTERN(clearb##EXT, 1, 0, 0, 1),                                   \
        REF_PATTERN(clearb##EXT, 1, 1, 0, 0),                                   \
        REF_PATTERN(clearb##EXT, 0, 1, 0, 1),                                   \
        REF_PATTERN(clearb##EXT, 1, 0, 1, 0),                                   \
        REF_PATTERN(clearb##EXT, 1, 0, 0, 0),                                   \
        REF_PATTERN(clearb##EXT, 0, 1, 0, 0),                                   \
        REF_PATTERN(clearb##EXT, 0, 0, 1, 0),                                   \
        {{0}}                                                                   \
    },                                                                          \
};

#define DECL_FUNCS_16(SIZE, EXT, FLAG)                                          \
    DECL_SHIFT16(EXT)                                                           \
    DECL_CONVERT(EXT,  U8, U16)                                                 \
    DECL_CONVERT(EXT, U16,  U8)                                                 \
    DECL_EXPAND(EXT,   U8, U16)                                                 \
    DECL_CLEAR_ALPHA(EXT, U16, 0, 0xFFFF)                                       \
    DECL_CLEAR_ALPHA(EXT, U16, 1, 0xFFFF)                                       \
    DECL_CLEAR_ALPHA(EXT, U16, 3, 0xFFFF)                                       \
    DECL_CLEAR(EXT, U16, w, 1, 1, 1, 0)                                         \
    DECL_CLEAR(EXT, U16, w, 0, 1, 1, 1)                                         \
    DECL_CLEAR(EXT, U16, w, 0, 0, 1, 1)                                         \
    DECL_CLEAR(EXT, U16, w, 1, 0, 0, 1)                                         \
    DECL_CLEAR(EXT, U16, w, 1, 1, 0, 0)                                         \
    DECL_CLEAR(EXT, U16, w, 0, 1, 0, 1)                                         \
    DECL_CLEAR(EXT, U16, w, 1, 0, 1, 0)                                         \
    DECL_CLEAR(EXT, U16, w, 1, 0, 0, 0)                                         \
    DECL_CLEAR(EXT, U16, w, 0, 1, 0, 0)                                         \
    DECL_CLEAR(EXT, U16, w, 0, 0, 1, 0)                                         \
                                                                                \
static const SwsOpTable ops16##EXT = {                                          \
    .cpu_flags = AV_CPU_FLAG_##FLAG,                                            \
    .block_w = SIZE,                                                            \
    .block_h = 1,                                                               \
    .entries = {                                                                \
        REF_COMMON_PATTERNS(convert_U8_U16##EXT),                               \
        REF_COMMON_PATTERNS(convert_U16_U8##EXT),                               \
        REF_COMMON_PATTERNS(expand_U8_U16##EXT),                                \
        REF_COMMON_PATTERNS(lshift16##EXT),                                     \
        REF_COMMON_PATTERNS(rshift16##EXT),                                     \
        op_clear_alpha0_U16##EXT,                                               \
        op_clear_alpha1_U16##EXT,                                               \
        op_clear_alpha3_U16##EXT,                                               \
        REF_PATTERN(clearw##EXT, 1, 1, 1, 0),                                   \
        REF_PATTERN(clearw##EXT, 0, 1, 1, 1),                                   \
        REF_PATTERN(clearw##EXT, 0, 0, 1, 1),                                   \
        REF_PATTERN(clearw##EXT, 1, 0, 0, 1),                                   \
        REF_PATTERN(clearw##EXT, 1, 1, 0, 0),                                   \
        REF_PATTERN(clearw##EXT, 0, 1, 0, 1),                                   \
        REF_PATTERN(clearw##EXT, 1, 0, 1, 0),                                   \
        REF_PATTERN(clearw##EXT, 1, 0, 0, 0),                                   \
        REF_PATTERN(clearw##EXT, 0, 1, 0, 0),                                   \
        REF_PATTERN(clearw##EXT, 0, 0, 1, 0),                                   \
        {{0}}                                                                   \
    },                                                                          \
};

#define DECL_FUNCS_32(SIZE, EXT, FLAG)                                          \
    DECL_CONVERT(EXT,  U8, U32)                                                 \
    DECL_CONVERT(EXT, U32,  U8)                                                 \
    DECL_CONVERT(EXT, U16, U32)                                                 \
    DECL_CONVERT(EXT, U32, U16)                                                 \
    DECL_CONVERT(EXT,  U8, F32)                                                 \
    DECL_CONVERT(EXT, F32,  U8)                                                 \
    DECL_CONVERT(EXT, U16, F32)                                                 \
    DECL_CONVERT(EXT, F32, U16)                                                 \
    DECL_EXPAND(EXT,   U8, U32)                                                 \
    DECL_MIN_MAX(EXT)                                                           \
    DECL_SCALE(EXT)                                                             \
    DECL_DITHER(EXT, 0)                                                         \
    DECL_DITHER(EXT, 1)                                                         \
    DECL_DITHER(EXT, 2)                                                         \
    DECL_DITHER(EXT, 3)                                                         \
    DECL_DITHER(EXT, 4)                                                         \
    DECL_LINEAR(EXT, luma,      SWS_MASK_LUMA)                                  \
    DECL_LINEAR(EXT, alpha,     SWS_MASK_ALPHA)                                 \
    DECL_LINEAR(EXT, lumalpha,  SWS_MASK_LUMA | SWS_MASK_ALPHA)                 \
    DECL_LINEAR(EXT, dot3,      0b111)                                          \
    DECL_LINEAR(EXT, row0,      SWS_MASK_ROW(0))                                \
    DECL_LINEAR(EXT, row0a,     SWS_MASK_ROW(0) | SWS_MASK_ALPHA)               \
    DECL_LINEAR(EXT, diag3,     SWS_MASK_DIAG3)                                 \
    DECL_LINEAR(EXT, diag4,     SWS_MASK_DIAG4)                                 \
    DECL_LINEAR(EXT, diagoff3,  SWS_MASK_DIAG3 | SWS_MASK_OFF3)                 \
    DECL_LINEAR(EXT, matrix3,   SWS_MASK_MAT3)                                  \
    DECL_LINEAR(EXT, affine3,   SWS_MASK_MAT3 | SWS_MASK_OFF3)                  \
    DECL_LINEAR(EXT, affine3a,  SWS_MASK_MAT3 | SWS_MASK_OFF3 | SWS_MASK_ALPHA) \
    DECL_LINEAR(EXT, matrix4,   SWS_MASK_MAT4)                                  \
    DECL_LINEAR(EXT, affine4,   SWS_MASK_MAT4 | SWS_MASK_OFF4)                  \
                                                                                \
static const SwsOpTable ops32##EXT = {                                          \
    .cpu_flags = AV_CPU_FLAG_##FLAG,                                            \
    .block_w = SIZE,                                                            \
    .block_h = 1,                                                               \
    .entries = {                                                                \
        REF_COMMON_PATTERNS(convert_U8_U32##EXT),                               \
        REF_COMMON_PATTERNS(convert_U32_U8##EXT),                               \
        REF_COMMON_PATTERNS(convert_U16_U32##EXT),                              \
        REF_COMMON_PATTERNS(convert_U32_U16##EXT),                              \
        REF_COMMON_PATTERNS(convert_U8_F32##EXT),                               \
        REF_COMMON_PATTERNS(convert_F32_U8##EXT),                               \
        REF_COMMON_PATTERNS(convert_U16_F32##EXT),                              \
        REF_COMMON_PATTERNS(convert_F32_U16##EXT),                              \
        REF_COMMON_PATTERNS(expand_U8_U32##EXT),                                \
        REF_COMMON_PATTERNS(min##EXT),                                          \
        REF_COMMON_PATTERNS(max##EXT),                                          \
        REF_COMMON_PATTERNS(scale##EXT),                                        \
        REF_COMMON_PATTERNS(dither0##EXT),                                      \
        REF_COMMON_PATTERNS(dither1##EXT),                                      \
        REF_COMMON_PATTERNS(dither2##EXT),                                      \
        REF_COMMON_PATTERNS(dither3##EXT),                                      \
        REF_COMMON_PATTERNS(dither4##EXT),                                      \
        op_luma##EXT,                                                           \
        op_alpha##EXT,                                                          \
        op_lumalpha##EXT,                                                       \
        op_dot3##EXT,                                                           \
        op_row0##EXT,                                                           \
        op_row0a##EXT,                                                          \
        op_diag3##EXT,                                                          \
        op_diag4##EXT,                                                          \
        op_diagoff3##EXT,                                                       \
        op_matrix3##EXT,                                                        \
        op_affine3##EXT,                                                        \
        op_affine3a##EXT,                                                       \
        op_matrix4##EXT,                                                        \
        op_affine4##EXT,                                                        \
        {{0}}                                                                   \
    },                                                                          \
};

DECL_FUNCS_8(16, _m1_ssse3, SSSE3)
DECL_FUNCS_8(32, _m2_ssse3, SSSE3)
DECL_FUNCS_8(32, _m1_avx2,  AVX2)
DECL_FUNCS_8(64, _m2_avx2,  AVX2)

DECL_FUNCS_16(16, _m1_avx2, AVX2)
DECL_FUNCS_16(32, _m2_avx2, AVX2)

DECL_FUNCS_32(16, _avx2,    AVX2)

static av_const int get_mmsize(void)
{
    const int cpu_flags = av_get_cpu_flags();
    if (cpu_flags & AV_CPU_FLAG_AVX2)
        return 32;
    else if (cpu_flags & AV_CPU_FLAG_SSSE3)
        return 16;
    else
        return AVERROR(ENOTSUP);
}

/**
 * Returns true if the operation's implementation only depends on the block
 * size, and not the underlying pixel type
 */
static bool op_is_type_invariant(const SwsOp *op)
{
    switch (op->op) {
    case SWS_OP_READ:
    case SWS_OP_WRITE:
        return !op->rw.packed && !op->rw.frac;
    case SWS_OP_SHUFFLE:
    case SWS_OP_SWIZZLE:
        return true;
    case SWS_OP_CLEAR:
        /* clear-to-zero is type invariant */
        for (int i = 0; i < 4; i++) {
            if (op->c.q4[i].num != 0)
                return false;
        }
        return true;
    }

    return false;
}

static int compile(SwsContext *ctx, SwsOpList *ops, SwsOpChain *chain)
{
    int ret;

    static const SwsOpTable *const tables[] = {
        &ops8_m1_ssse3,
        &ops8_m1_avx2,
        &ops8_m2_avx2,
        &ops16_m1_avx2,
        &ops16_m2_avx2,
        &ops32_avx2,
    };

    /* Use at most two full vregs during the widest precision section */
    chain->block_w = 2 * get_mmsize() / ff_sws_op_list_max_size(ops);
    chain->block_h = 1;

    do {
        int block_w = chain->block_w, block_h = chain->block_h;
        SwsOp *op = &ops->ops[0];

        if (op_is_type_invariant(op)) {
            const int size = ff_sws_pixel_type_size(op->type);
            if (op->op == SWS_OP_SHUFFLE) {
                /* We lose information about the shuffle size, so pre-fill the
                 * entire array here */
                const int mask = size - 1;
                for (int i = size; i < 8; i++)
                    op->shuffle.index[i] = (i & ~mask) + op->shuffle.index[i & mask];
            }

            block_w *= size;
            op->type = SWS_PIXEL_U8;
        }

        ret = ff_sws_op_compile_tables(tables, FF_ARRAY_ELEMS(tables), ops,
                                       block_w, block_h, chain);
    } while (ret == AVERROR(EAGAIN));
    return ret;
}

SwsOpBackend backend_x86 = {
    .name       = "x86",
    .compile    = compile,
};
