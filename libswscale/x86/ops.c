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

#define DECL_CLEAR_ALPHA(EXT, NAME, IDX)                                        \
    DECL_ASM(U8, clear_alpha##IDX##EXT,                                         \
        .op.op = SWS_OP_CLEAR,                                                  \
        .op.clear.value[IDX] = { .num = 0xFF, .den = 1 },                       \
        .op.comps.unused[IDX] = true,                                           \
    );

#define DECL_SWIZZLE(EXT, X, Y, Z, W)                                           \
    DECL_ASM(U8, swizzle_##X##Y##Z##W##EXT,                                     \
        .op.op = SWS_OP_SWIZZLE,                                                \
        .op.swizzle = SWS_SWIZZLE( X, Y, Z, W ),                                \
    );

#define DECL_FUNCS_8(EXT)                                                       \
    DECL_RW(EXT, read_planar, READ, 1, false)                                   \
    DECL_RW(EXT, read_planar, READ, 2, false)                                   \
    DECL_RW(EXT, read_planar, READ, 3, false)                                   \
    DECL_RW(EXT, read_planar, READ, 4, false)                                   \
    DECL_RW(EXT, write_planar, WRITE, 1, false)                                 \
    DECL_RW(EXT, write_planar, WRITE, 2, false)                                 \
    DECL_RW(EXT, write_planar, WRITE, 3, false)                                 \
    DECL_RW(EXT, write_planar, WRITE, 4, false)                                 \
    DECL_RW(EXT, read8_packed, READ, 2, true)                                   \
    DECL_SWIZZLE(EXT, 3, 0, 1, 2)                                               \
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
    DECL_SWIZZLE(EXT, 0, 2, 1, 3)                                               \
    DECL_SWIZZLE(EXT, 0, 2, 3, 1)                                               \
    DECL_SWIZZLE(EXT, 0, 3, 1, 2)                                               \
    DECL_SWIZZLE(EXT, 3, 1, 2, 0)                                               \
    DECL_SWIZZLE(EXT, 0, 3, 2, 1)                                               \
    DECL_SWIZZLE(EXT, 0, 0, 0, 3)                                               \
    DECL_SWIZZLE(EXT, 3, 0, 0, 0)                                               \
    DECL_SWIZZLE(EXT, 0, 0, 0, 1)                                               \
    DECL_SWIZZLE(EXT, 1, 0, 0, 0)                                               \
    DECL_CLEAR_ALPHA(EXT, clear_alpha, 0)                                       \
    DECL_CLEAR_ALPHA(EXT, clear_alpha, 1)                                       \
    DECL_CLEAR_ALPHA(EXT, clear_alpha, 3)

#define DECL_CONVERT(EXT, FROM, TO)                                             \
    DECL_COMMON_PATTERNS(U##FROM, convert_##FROM##to##TO##EXT,                  \
        .op.op = SWS_OP_CONVERT,                                                \
        .op.convert.to = SWS_PIXEL_U##TO,                                       \
    );

static int setup_shift(const SwsOp *op, SwsOpPriv *out)
{
    out->u16[0] = op->shift.amount;
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

#define DECL_FUNCS_16(EXT)                                                      \
    DECL_CONVERT(EXT,  8, 16)                                                   \
    DECL_CONVERT(EXT, 16,  8)                                                   \
    DECL_SHIFT16(EXT)

#define REF_OPS_U8(EXT)      \
    op_read_planar1##EXT,    \
    op_read_planar2##EXT,    \
    op_read_planar3##EXT,    \
    op_read_planar4##EXT,    \
    op_write_planar1##EXT,   \
    op_write_planar2##EXT,   \
    op_write_planar3##EXT,   \
    op_write_planar4##EXT,   \
    op_read8_packed2##EXT,   \
    op_swizzle_3012##EXT,    \
    op_swizzle_0003##EXT,    \
    op_swizzle_0001##EXT,    \
    op_swizzle_3000##EXT,    \
    op_swizzle_1000##EXT,    \
    op_clear_alpha0##EXT,    \
    op_clear_alpha1##EXT,    \
    op_clear_alpha3##EXT,

#define REF_OPS_U16(EXT)                        \
    REF_COMMON_PATTERNS(convert_8to16##EXT),    \
    REF_COMMON_PATTERNS(convert_16to8##EXT),    \
    REF_COMMON_PATTERNS(lshift16##EXT),         \
    REF_COMMON_PATTERNS(rshift16##EXT),

DECL_FUNCS_8(_m1_sse2)
DECL_FUNCS_8(_m1_avx2)
DECL_FUNCS_8(_m2_avx2)

DECL_FUNCS_16(_m1_avx2)
DECL_FUNCS_16(_m2_avx2)

static const SwsOpTable ops_u8_m1_sse2 = {
    .cpu_flags = AV_CPU_FLAG_SSE2,
    .block_w = 16,
    .block_h = 1,
    .entries = {
        REF_OPS_U8(_m1_sse2)
        {{0}}
    },
};

static const SwsOpTable ops_u8_m1_avx2 = {
    .cpu_flags = AV_CPU_FLAG_AVX2,
    .block_w = 32,
    .block_h = 1,
    .entries = {
        REF_OPS_U8(_m1_avx2)
        {{0}}
    },
};

static const SwsOpTable ops_u8_m2_avx2 = {
    .cpu_flags = AV_CPU_FLAG_AVX2,
    .block_w = 64,
    .block_h = 1,
    .entries = {
        REF_OPS_U8(_m2_avx2)
        {{0}}
    },
};

static const SwsOpTable ops_u16_m1_avx2 = {
    .cpu_flags = AV_CPU_FLAG_AVX2,
    .block_w = 16,
    .block_h = 1,
    .entries = {
        REF_OPS_U16(_m1_avx2)
        {{0}}
    },
};

static const SwsOpTable ops_u16_m2_avx2 = {
    .cpu_flags = AV_CPU_FLAG_AVX2,
    .block_w = 32,
    .block_h = 1,
    .entries = {
        REF_OPS_U16(_m2_avx2)
        {{0}}
    },
};

static av_const int get_mmsize(void)
{
    const int cpu_flags = av_get_cpu_flags();
    if (cpu_flags & AV_CPU_FLAG_AVX2)
        return 32;
    else if (cpu_flags & AV_CPU_FLAG_SSE2)
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
        return true;
    }

    return false;
}

/**
 * Returns true if the operation is a full range clamp followed by a conversion
 */
static bool op_is_saturating_clamp(const SwsOp *op, const SwsOp *next)
{
    if (op->op == SWS_OP_CLAMP && next && next->op == SWS_OP_CONVERT) {
        const int bits = 8 * ff_sws_pixel_type_size(next->convert.to);
        const unsigned value_range = (1 << bits) - 1;

        for (int i = 0; i < 4; i++) {
            const AVRational max = next->clamp.max[i];
            if (max.den == 0)
                continue;
            else if (max.den != 1 || max.num < value_range)
                return false;
        }

        return true;
    } else {
        return false;
    }
}

static int compile(SwsOpList *ops, SwsOpChain *chain)
{
    int ret;

    static const SwsOpTable *const tables[] = {
        &ops_u8_m1_sse2,
        &ops_u8_m1_avx2,
        &ops_u8_m2_avx2,
        &ops_u16_m1_avx2,
        &ops_u16_m2_avx2,
    };

    /* Use at most two full vregs during the widest precision section */
    chain->block_w = 2 * get_mmsize() / ff_sws_op_list_max_size(ops);
    chain->block_h = 1;

    do {
        int block_w = chain->block_w, block_h = chain->block_h;
        SwsOp *op = &ops->ops[0];
        const SwsOp *next = ops->num_ops > 1 ? &ops->ops[1] : NULL;

        if (op_is_type_invariant(op)) {
            block_w *= ff_sws_pixel_type_size(op->type);
            op->type = SWS_PIXEL_U8;
        }

        if (op_is_saturating_clamp(op, next))
            ff_sws_op_list_remove_at(ops, 0, 1);

        ret = ff_sws_op_compile_tables(tables, FF_ARRAY_ELEMS(tables), ops,
                                       block_w, block_h, chain);
    } while (ret == AVERROR(EAGAIN));
    return ret;
}

SwsOpBackend backend_x86 = {
    .name       = "x86",
    .compile    = compile,
};
