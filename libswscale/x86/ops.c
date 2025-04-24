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

#include <libavutil/avassert.h>
#include <libavutil/bswap.h>

#include "../ops_internal.h"

#define DECL_FUNC(NAME) \
    void ff_##NAME(const SwsOpExec *, const SwsOpImpl *);

#define DECL_ENTRY(TYPE, NAME, ...)                                             \
    static const SwsOpEntry op_##NAME = {                                       \
        .op.type = SWS_PIXEL_##TYPE,                                            \
        __VA_ARGS__                                                             \
    }

#define DECL_ASM(TYPE, NAME, ...)                                               \
    DECL_FUNC(NAME)                                                             \
    DECL_ENTRY(TYPE, NAME,                                                      \
        .func = ff_##NAME,                                                      \
        __VA_ARGS__)

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

#define DECL_RW(EXT, TYPE, NAME, OP, ELEMS, PACKED, FRAC)                       \
    DECL_ASM(TYPE, NAME##ELEMS##EXT,                                            \
        .op.op = SWS_OP_##OP,                                                   \
        .op.rw = { .elems = ELEMS, .packed = PACKED, .frac = FRAC },            \
    );

#define DECL_PACKED_RW(EXT, DEPTH)                                              \
    DECL_RW(EXT, U##DEPTH, read##DEPTH##_packed,  READ,  2, true,  0)           \
    DECL_RW(EXT, U##DEPTH, read##DEPTH##_packed,  READ,  3, true,  0)           \
    DECL_RW(EXT, U##DEPTH, read##DEPTH##_packed,  READ,  4, true,  0)           \
    DECL_RW(EXT, U##DEPTH, write##DEPTH##_packed, WRITE, 2, true,  0)           \
    DECL_RW(EXT, U##DEPTH, write##DEPTH##_packed, WRITE, 3, true,  0)           \
    DECL_RW(EXT, U##DEPTH, write##DEPTH##_packed, WRITE, 4, true,  0)           \

static int setup_swap_bytes(const SwsOp *op, SwsOpPriv *out)
{
    const int mask = ff_sws_pixel_type_size(op->type) - 1;
    for (int i = 0; i < 16; i++)
        out->u8[i] = (i & ~mask) | (mask - (i & mask));
    return 0;
}

#define DECL_SWAP_BYTES(EXT, TYPE, X, Y, Z, W)                                  \
    DECL_PATTERN(TYPE, swap_bytes_##TYPE##EXT, X, Y, Z, W,                      \
        .func = ff_p##X##Y##Z##W##_shuffle##EXT,                                \
        .op.op = SWS_OP_SWAP_BYTES,                                             \
        .setup = setup_swap_bytes,                                              \
    );

#define DECL_CLEAR_ALPHA(EXT, IDX)                                              \
    DECL_ASM(U8, clear_alpha##IDX##EXT,                                         \
        .op.op = SWS_OP_CLEAR,                                                  \
        .op.c.q4[IDX] = { .num = -1, .den = 1 },                                \
        .op.comps.unused[IDX] = true,                                           \
    );                                                                          \

#define DECL_CLEAR_ZERO(EXT, IDX)                                               \
    DECL_ASM(U8, clear_zero##IDX##EXT,                                          \
        .op.op = SWS_OP_CLEAR,                                                  \
        .op.c.q4[IDX] = { .num = 0, .den = 1 },                                 \
        .op.comps.unused[IDX] = true,                                           \
    );

static int setup_clear(const SwsOp *op, SwsOpPriv *out)
{
    for (int i = 0; i < 4; i++)
        out->u32[i] = (uint32_t) op->c.q4[i].num;
    return 0;
}

#define DECL_CLEAR(EXT, X, Y, Z, W)                                             \
    DECL_PATTERN(U8, clear##EXT, X, Y, Z, W,                                    \
        .op.op = SWS_OP_CLEAR,                                                  \
        .setup = setup_clear,                                                   \
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
    DECL_RW(EXT, U8, read_planar,   READ,  1, false, 0)                         \
    DECL_RW(EXT, U8, read_planar,   READ,  2, false, 0)                         \
    DECL_RW(EXT, U8, read_planar,   READ,  3, false, 0)                         \
    DECL_RW(EXT, U8, read_planar,   READ,  4, false, 0)                         \
    DECL_RW(EXT, U8, write_planar,  WRITE, 1, false, 0)                         \
    DECL_RW(EXT, U8, write_planar,  WRITE, 2, false, 0)                         \
    DECL_RW(EXT, U8, write_planar,  WRITE, 3, false, 0)                         \
    DECL_RW(EXT, U8, write_planar,  WRITE, 4, false, 0)                         \
    DECL_RW(EXT, U8, read_nibbles,  READ,  1, false, 1)                         \
    DECL_RW(EXT, U8, read_bits,     READ,  1, false, 3)                         \
    DECL_RW(EXT, U8, write_bits,    WRITE, 1, false, 3)                         \
    DECL_PACKED_RW(EXT, 8)                                                      \
    DECL_FUNC(p1000_shuffle##EXT)                                               \
    DECL_FUNC(p1001_shuffle##EXT)                                               \
    DECL_FUNC(p1110_shuffle##EXT)                                               \
    DECL_FUNC(p1111_shuffle##EXT)                                               \
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
    DECL_CLEAR_ALPHA(EXT, 0)                                                    \
    DECL_CLEAR_ALPHA(EXT, 1)                                                    \
    DECL_CLEAR_ALPHA(EXT, 3)                                                    \
    DECL_CLEAR_ZERO(EXT, 0)                                                     \
    DECL_CLEAR_ZERO(EXT, 1)                                                     \
    DECL_CLEAR_ZERO(EXT, 3)                                                     \
    DECL_CLEAR(EXT, 1, 1, 1, 0)                                                 \
    DECL_CLEAR(EXT, 0, 1, 1, 1)                                                 \
    DECL_CLEAR(EXT, 0, 0, 1, 1)                                                 \
    DECL_CLEAR(EXT, 1, 0, 0, 1)                                                 \
    DECL_CLEAR(EXT, 1, 1, 0, 0)                                                 \
    DECL_CLEAR(EXT, 0, 1, 0, 1)                                                 \
    DECL_CLEAR(EXT, 1, 0, 1, 0)                                                 \
    DECL_CLEAR(EXT, 1, 0, 0, 0)                                                 \
    DECL_CLEAR(EXT, 0, 1, 0, 0)                                                 \
    DECL_CLEAR(EXT, 0, 0, 1, 0)                                                 \
                                                                                \
static const SwsOpTable ops8##EXT = {                                           \
    .cpu_flags = AV_CPU_FLAG_##FLAG,                                            \
    .block_size = SIZE,                                                         \
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
        op_write8_packed3##EXT,                                                 \
        op_write8_packed4##EXT,                                                 \
        op_read_nibbles1##EXT,                                                  \
        op_read_bits1##EXT,                                                     \
        op_write_bits1##EXT,                                                    \
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
        op_clear_alpha0##EXT,                                                   \
        op_clear_alpha1##EXT,                                                   \
        op_clear_alpha3##EXT,                                                   \
        op_clear_zero0##EXT,                                                    \
        op_clear_zero1##EXT,                                                    \
        op_clear_zero3##EXT,                                                    \
        REF_PATTERN(clear##EXT, 1, 1, 1, 0),                                    \
        REF_PATTERN(clear##EXT, 0, 1, 1, 1),                                    \
        REF_PATTERN(clear##EXT, 0, 0, 1, 1),                                    \
        REF_PATTERN(clear##EXT, 1, 0, 0, 1),                                    \
        REF_PATTERN(clear##EXT, 1, 1, 0, 0),                                    \
        REF_PATTERN(clear##EXT, 0, 1, 0, 1),                                    \
        REF_PATTERN(clear##EXT, 1, 0, 1, 0),                                    \
        REF_PATTERN(clear##EXT, 1, 0, 0, 0),                                    \
        REF_PATTERN(clear##EXT, 0, 1, 0, 0),                                    \
        REF_PATTERN(clear##EXT, 0, 0, 1, 0),                                    \
        {{0}}                                                                   \
    },                                                                          \
};

#define DECL_FUNCS_16(SIZE, EXT, FLAG)                                          \
    DECL_PACKED_RW(EXT, 16)                                                     \
    DECL_SWAP_BYTES(EXT, U16, 1, 0, 0, 0)                                       \
    DECL_SWAP_BYTES(EXT, U16, 1, 0, 0, 1)                                       \
    DECL_SWAP_BYTES(EXT, U16, 1, 1, 1, 0)                                       \
    DECL_SWAP_BYTES(EXT, U16, 1, 1, 1, 1)                                       \
    DECL_SHIFT16(EXT)                                                           \
    DECL_CONVERT(EXT,  U8, U16)                                                 \
    DECL_CONVERT(EXT, U16,  U8)                                                 \
    DECL_EXPAND(EXT,   U8, U16)                                                 \
                                                                                \
static const SwsOpTable ops16##EXT = {                                          \
    .cpu_flags = AV_CPU_FLAG_##FLAG,                                            \
    .block_size = SIZE,                                                         \
    .entries = {                                                                \
        op_read16_packed2##EXT,                                                 \
        op_read16_packed3##EXT,                                                 \
        op_read16_packed4##EXT,                                                 \
        op_write16_packed2##EXT,                                                \
        op_write16_packed3##EXT,                                                \
        op_write16_packed4##EXT,                                                \
        REF_COMMON_PATTERNS(swap_bytes_U16##EXT),                               \
        REF_COMMON_PATTERNS(convert_U8_U16##EXT),                               \
        REF_COMMON_PATTERNS(convert_U16_U8##EXT),                               \
        REF_COMMON_PATTERNS(expand_U8_U16##EXT),                                \
        REF_COMMON_PATTERNS(lshift16##EXT),                                     \
        REF_COMMON_PATTERNS(rshift16##EXT),                                     \
        {{0}}                                                                   \
    },                                                                          \
};

#define DECL_FUNCS_32(SIZE, EXT, FLAG)                                          \
    DECL_PACKED_RW(_m2##EXT, 32)                                                \
    DECL_SWAP_BYTES(_m2##EXT, U32, 1, 0, 0, 0)                                  \
    DECL_SWAP_BYTES(_m2##EXT, U32, 1, 0, 0, 1)                                  \
    DECL_SWAP_BYTES(_m2##EXT, U32, 1, 1, 1, 0)                                  \
    DECL_SWAP_BYTES(_m2##EXT, U32, 1, 1, 1, 1)                                  \
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
    .block_size = SIZE,                                                         \
    .entries = {                                                                \
        op_read32_packed2_m2##EXT,                                              \
        op_read32_packed3_m2##EXT,                                              \
        op_read32_packed4_m2##EXT,                                              \
        op_write32_packed2_m2##EXT,                                             \
        op_write32_packed3_m2##EXT,                                             \
        op_write32_packed4_m2##EXT,                                             \
        REF_COMMON_PATTERNS(swap_bytes_U32_m2##EXT),                            \
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

DECL_FUNCS_8(16, _m1_sse4, SSE4)
DECL_FUNCS_8(32, _m1_avx2, AVX2)
DECL_FUNCS_8(32, _m2_sse4, SSE4)
DECL_FUNCS_8(64, _m2_avx2, AVX2)

DECL_FUNCS_16(16, _m1_avx2, AVX2)
DECL_FUNCS_16(32, _m2_avx2, AVX2)

DECL_FUNCS_32(16, _avx2, AVX2)

static av_const int get_mmsize(const int cpu_flags)
{
    if (cpu_flags & AV_CPU_FLAG_AVX2)
        return 32;
    else if (cpu_flags & AV_CPU_FLAG_SSE4)
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
    case SWS_OP_SWIZZLE:
    case SWS_OP_CLEAR:
        return true;
    }

    return false;
}

/* Tries to reduce a series of operations to an in-place shuffle mask.
 * Returns the block size, 0 or a negative error code. */
static int solve_shuffle(const SwsOpList *ops, SwsOpChain *chain, int mmsize)
{
    const SwsOp read = ops->ops[0];
    const int read_size = ff_sws_pixel_type_size(read.type);
    uint32_t mask[4] = {0};

    if (!ops->num_ops || read.op != SWS_OP_READ)
        return AVERROR(EINVAL);
    if (read.rw.frac || (!read.rw.packed && read.rw.elems > 1))
        return AVERROR(ENOTSUP);

    for (int i = 0; i < read.rw.elems; i++)
        mask[i] = 0x01010101 * i * read_size + 0x03020100;

    for (int opidx = 1; opidx < ops->num_ops; opidx++) {
        const SwsOp *op = &ops->ops[opidx];
        switch (op->op) {
        case SWS_OP_SWIZZLE: {
            uint32_t orig[4] = { mask[0], mask[1], mask[2], mask[3] };
            for (int i = 0; i < 4; i++)
                mask[i] = orig[op->swizzle.in[i]];
            break;
        }

        case SWS_OP_SWAP_BYTES:
            for (int i = 0; i < 4; i++) {
                switch (ff_sws_pixel_type_size(op->type)) {
                case 2: mask[i] = av_bswap16(mask[i]); break;
                case 4: mask[i] = av_bswap32(mask[i]); break;
                }
            }
            break;

        case SWS_OP_CLEAR:
            for (int i = 0; i < 4; i++) {
                if (!op->c.q4[i].den)
                    continue;
                if (op->c.q4[i].num != 0)
                    return AVERROR(ENOTSUP);
                mask[i] = 0x80808080ul; /* pshufb implicit clear to zero */
            }
            break;

        case SWS_OP_CONVERT: {
            if (!op->convert.expand)
                return AVERROR(ENOTSUP);
            for (int i = 0; i < 4; i++) {
                switch (ff_sws_pixel_type_size(op->type)) {
                case 1: mask[i] = 0x01010101 * (mask[i] & 0xFF);   break;
                case 2: mask[i] = 0x00010001 * (mask[i] & 0xFFFF); break;
                }
            }
            break;
        }

        case SWS_OP_WRITE: {
            if (op->rw.frac || !op->rw.packed)
                return AVERROR(ENOTSUP);

            /* Initialize to no-op */
            SwsOpPriv priv;
            SwsFunc func = NULL;
            for (int i = 0; i < 16; i++)
                priv.u8[i] = 0x80;

            const int write_size  = ff_sws_pixel_type_size(op->type);
            const int read_chunk  = read.rw.elems * read_size;
            const int write_chunk = op->rw.elems * write_size;
            const int groups_per_lane = 16 / FFMAX(read_chunk, write_chunk);
            for (int n = 0; n < groups_per_lane; n++) {
                const int base_in  = n * read_chunk;
                const int base_out = n * write_chunk;
                for (int i = 0; i < op->rw.elems; i++) {
                    const int offset = base_out + i * write_size;
                    for (int b = 0; b < write_size; b++)
                        priv.u8[offset + b] = base_in + (mask[i] >> (b * 8));
                }
            }

            const int in_per_lane  = groups_per_lane * read_chunk;
            const int out_per_lane = groups_per_lane * write_chunk;
            if (in_per_lane < 16 || out_per_lane < 16)
                mmsize = 16; /* avoid cross-lane shuffle */

            const int num_lanes = mmsize / 16;
            const int in_total  = num_lanes * in_per_lane;
            const int out_total = num_lanes * out_per_lane;

#define ASSIGN_SHUFFLE_FUNC(IN, OUT, EXT)                                       \
do {                                                                            \
    DECL_FUNC(packed_shuffle##IN##_##OUT##_##EXT);                              \
    if (in_total == IN && out_total == OUT)                                     \
        func = ff_packed_shuffle##IN##_##OUT##_##EXT;                           \
} while (0)

            ASSIGN_SHUFFLE_FUNC( 5, 15, sse4);
            ASSIGN_SHUFFLE_FUNC( 4, 16, sse4);
            ASSIGN_SHUFFLE_FUNC( 2, 12, sse4);
            ASSIGN_SHUFFLE_FUNC(10, 15, sse4);
            ASSIGN_SHUFFLE_FUNC( 8, 16, sse4);
            ASSIGN_SHUFFLE_FUNC( 4, 12, sse4);
            ASSIGN_SHUFFLE_FUNC(15, 15, sse4);
            ASSIGN_SHUFFLE_FUNC(12, 16, sse4);
            ASSIGN_SHUFFLE_FUNC( 6, 12, sse4);
            ASSIGN_SHUFFLE_FUNC(16, 12, sse4);
            ASSIGN_SHUFFLE_FUNC(16, 16, sse4);
            ASSIGN_SHUFFLE_FUNC( 8, 12, sse4);
            ASSIGN_SHUFFLE_FUNC(12, 12, sse4);
            ASSIGN_SHUFFLE_FUNC(32, 32, avx2);
            if (!func)
                return AVERROR(ENOTSUP);

            const int regs = 4 * 4; /* four reps of 4 regs per iter */
            const int tail = in_total <= 4 ? 4 : in_total <= 8 ? 8 : mmsize;

            chain->block_size  = regs * groups_per_lane * num_lanes;
            chain->read_bytes  = (regs - 1) * in_total  + tail;
            chain->write_bytes = (regs - 1) * out_total + mmsize;
            return ff_sws_op_chain_append(chain, func, NULL, priv);
        }

        default:
            return AVERROR(ENOTSUP);
        }
    }

    return AVERROR(EINVAL);
}

/* Normalize clear values into 32-bit integer constants */
static void normalize_clear(SwsOp *op)
{
    static_assert(sizeof(uint32_t) == sizeof(int), "int size mismatch");
    SwsOpPriv priv;
    union {
        uint32_t u32;
        int i;
    } c;

    ff_sws_setup_q4(op, &priv);
    for (int i = 0; i < 4; i++) {
        if (!op->c.q4[i].den)
            continue;
        switch (ff_sws_pixel_type_size(op->type)) {
        case 1: c.u32 = 0x1010101 * priv.u8[i]; break;
        case 2: c.u32 = priv.u16[i] << 16 | priv.u16[i]; break;
        case 4: c.u32 = priv.u32[i]; break;
        }

        op->c.q4[i].num = c.i;
        op->c.q4[i].den = 1;
    }
}

static int compile(SwsContext *ctx, SwsOpList *ops, SwsOpChain *chain)
{
    const int cpu_flags = av_get_cpu_flags();
    const int mmsize = get_mmsize(cpu_flags);
    av_assert1(ops->num_ops > 0);
    const SwsOp read = ops->ops[0];
    const SwsOp write = ops->ops[ops->num_ops - 1];
    int ret;

    if (mmsize < 0)
        return mmsize;

    static const SwsOpTable *const tables[] = {
        &ops8_m1_sse4,
        &ops8_m1_avx2,
        &ops8_m2_sse4,
        &ops8_m2_avx2,
        &ops16_m1_avx2,
        &ops16_m2_avx2,
        &ops32_avx2,
    };

    /* Special fast path for in-place packed shuffle */
    ret = solve_shuffle(ops, chain, mmsize);
    if (ret != AVERROR(ENOTSUP))
        return ret;

    /* Use at most two full vregs during the widest precision section */
    chain->block_size = 2 * mmsize / ff_sws_op_list_max_size(ops);

    /* 3-component reads/writes process one extra garbage word */
    if (read.rw.packed && read.rw.elems == 3) {
        const int pixel_size = 3 * ff_sws_pixel_type_size(read.type);
        chain->read_bytes = chain->block_size * pixel_size + 4;
    }

    if (write.rw.packed && write.rw.elems == 3) {
        const int pixel_size = 3 * ff_sws_pixel_type_size(write.type);
        chain->write_bytes = chain->block_size * pixel_size + 4;
    }

    do {
        int block_size = chain->block_size;
        SwsOp *op = &ops->ops[0];

        if (op_is_type_invariant(op)) {
            if (op->op == SWS_OP_CLEAR)
                normalize_clear(op);
            block_size *= ff_sws_pixel_type_size(op->type);
            op->type = SWS_PIXEL_U8;
        }

        ret = ff_sws_op_compile_tables(tables, FF_ARRAY_ELEMS(tables), ops,
                                       block_size, chain);
    } while (ret == AVERROR(EAGAIN));
    return ret;
}

SwsOpBackend backend_x86 = {
    .name       = "x86",
    .compile    = compile,
};
