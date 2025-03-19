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
#include "libavutil/bswap.h"

#include "ops_backend.h"

#ifndef BIT_DEPTH
#  define BIT_DEPTH 8
#endif

#if BIT_DEPTH == 32
#  define PIXEL_TYPE SWS_PIXEL_U32
#  define PIXEL_MAX  0xFFFFFFFFu
#  define SWAP_BYTES av_bswap32
#  define pixel_t    uint32_t
#elif BIT_DEPTH == 16
#  define PIXEL_TYPE SWS_PIXEL_U16
#  define PIXEL_MAX  0xFFFFu
#  define SWAP_BYTES av_bswap16
#  define pixel_t    uint16_t
#elif BIT_DEPTH == 8
#  define PIXEL_TYPE SWS_PIXEL_U8
#  define PIXEL_MAX  0xFFu
#  define pixel_t    uint8_t
#else
#  error Invalid BIT_DEPTH
#endif

#define IS_FLOAT 0
#define FMT_CHAR u
#include "ops_tmpl_common.c"

DECL_READ(read_planar, const int pixels, const int elems)
{
    pixel_t x[SWS_CHUNK_SIZE], y[SWS_CHUNK_SIZE],
            z[SWS_CHUNK_SIZE], w[SWS_CHUNK_SIZE];

    SWS_LOOP
    for (int i = 0; i < pixels; i++) {
        x[i] = in0[i];
        if (elems > 1)
            y[i] = in1[i];
        if (elems > 2)
            z[i] = in2[i];
        if (elems > 3)
            w[i] = in3[i];
    }

    CONTINUE(pixel_t *, x, y, z, w);
}

DECL_READ(read_packed, const int pixels, const int elems)
{
    pixel_t x[SWS_CHUNK_SIZE], y[SWS_CHUNK_SIZE],
            z[SWS_CHUNK_SIZE], w[SWS_CHUNK_SIZE];

    SWS_LOOP
    for (int i = 0; i < pixels; i++) {
        x[i] = in0[elems * i + 0];
        if (elems > 1)
            y[i] = in0[elems * i + 1];
        if (elems > 2)
            z[i] = in0[elems * i + 2];
        if (elems > 3)
            w[i] = in0[elems * i + 3];
    }

    CONTINUE(pixel_t *, x, y, z, w);
}

DECL_WRITE(write_planar, const int pixels, const int elems)
{
    SWS_LOOP
    for (int i = 0; i < pixels; i++) {
        out0[i] = x[i];
        if (elems > 1)
            out1[i] = y[i];
        if (elems > 2)
            out2[i] = z[i];
        if (elems > 3)
            out3[i] = w[i];
    }
}

DECL_WRITE(write_packed, const int pixels, const int elems)
{
    SWS_LOOP
    for (int i = 0; i < pixels; i++) {
        out0[elems * i + 0] = x[i];
        if (elems > 1)
            out0[elems * i + 1] = y[i];
        if (elems > 2)
            out0[elems * i + 2] = z[i];
        if (elems > 3)
            out0[elems * i + 3] = w[i];
    }
}

#define WRAP_READ(FUNC, ELEMS, FRAC, PLANAR)                                    \
DECL_IMPL_READONLY(FUNC##ELEMS)                                                 \
{                                                                               \
    const int pixel_stride = sizeof(pixel_t) * (PLANAR ? 1 : ELEMS);            \
    const int align = SWS_CHUNK_SIZE * pixel_stride >> FRAC;                    \
    CALL_READONLY(FUNC, SWS_ASSUME_ALIGNED(exec->in.data[0], align),            \
                        SWS_ASSUME_ALIGNED(exec->in.data[1], align),            \
                        SWS_ASSUME_ALIGNED(exec->in.data[2], align),            \
                        SWS_ASSUME_ALIGNED(exec->in.data[3], align),            \
                        SWS_CHUNK_SIZE, ELEMS);                                 \
}                                                                               \
                                                                                \
DECL_IMPL_READONLY(FUNC##ELEMS##_n)                                             \
{                                                                               \
    const pixel_t *restrict in0 = (const pixel_t *) exec->in.data[0];           \
    const pixel_t *restrict in1 = (const pixel_t *) exec->in.data[1];           \
    const pixel_t *restrict in2 = (const pixel_t *) exec->in.data[2];           \
    const pixel_t *restrict in3 = (const pixel_t *) exec->in.data[3];           \
    const int pixels = exec->block_w;                                           \
    av_assert2(exec->block_h == 1);                                             \
    SWS_ASSUME(pixels <= SWS_CHUNK_SIZE);                                       \
    CALL_READONLY(FUNC, in0, in1, in2, in3, pixels, ELEMS);                     \
}                                                                               \
                                                                                \
DECL_ENTRY(FUNC##ELEMS,                                                         \
    .func_n = (SwsOpFunc) fn(FUNC##ELEMS##_n),                                  \
    .op.op = SWS_OP_READ,                                                       \
    .op.rw = {                                                                  \
        .elems  = ELEMS,                                                        \
        .planar = PLANAR,                                                       \
        .frac   = FRAC,                                                         \
    },                                                                          \
);

WRAP_READ(read_packed, 1, 0, false)
WRAP_READ(read_packed, 2, 0, false)
WRAP_READ(read_packed, 3, 0, false)
WRAP_READ(read_packed, 4, 0, false)
WRAP_READ(read_planar, 2, 0, true)
WRAP_READ(read_planar, 3, 0, true)
WRAP_READ(read_planar, 4, 0, true)

#define WRAP_WRITE(FUNC, ELEMS, FRAC, PLANAR)                                   \
DECL_IMPL(FUNC##ELEMS)                                                          \
{                                                                               \
    const int pixel_stride = sizeof(pixel_t) * (PLANAR ? 1 : ELEMS);            \
    const int align = SWS_CHUNK_SIZE * pixel_stride >> FRAC;                    \
    CALL(FUNC, SWS_ASSUME_ALIGNED(exec->out.data[0], align),                    \
               SWS_ASSUME_ALIGNED(exec->out.data[1], align),                    \
               SWS_ASSUME_ALIGNED(exec->out.data[2], align),                    \
               SWS_ASSUME_ALIGNED(exec->out.data[3], align),                    \
               SWS_CHUNK_SIZE, ELEMS);                                          \
}                                                                               \
                                                                                \
DECL_IMPL(FUNC##ELEMS##_n)                                                      \
{                                                                               \
    pixel_t *restrict out0 = (pixel_t *) exec->out.data[0];                     \
    pixel_t *restrict out1 = (pixel_t *) exec->out.data[1];                     \
    pixel_t *restrict out2 = (pixel_t *) exec->out.data[2];                     \
    pixel_t *restrict out3 = (pixel_t *) exec->out.data[3];                     \
    const int pixels = exec->block_w;                                           \
    av_assert2(exec->block_h == 1);                                             \
    SWS_ASSUME(pixels <= SWS_CHUNK_SIZE);                                       \
    CALL(FUNC, out0, out1, out2, out3, pixels, ELEMS);                          \
}                                                                               \
                                                                                \
DECL_ENTRY(FUNC##ELEMS,                                                         \
    .func_n = (SwsOpFunc) fn(FUNC##ELEMS##_n),                                  \
    .op.op = SWS_OP_WRITE,                                                      \
    .op.rw = {                                                                  \
        .elems  = ELEMS,                                                        \
        .planar = PLANAR,                                                       \
        .frac   = FRAC,                                                         \
    },                                                                          \
);

WRAP_WRITE(write_packed, 1, 0, false)
WRAP_WRITE(write_packed, 2, 0, false)
WRAP_WRITE(write_packed, 3, 0, false)
WRAP_WRITE(write_packed, 4, 0, false)
WRAP_WRITE(write_planar, 2, 0, true)
WRAP_WRITE(write_planar, 3, 0, true)
WRAP_WRITE(write_planar, 4, 0, true)

DECL_FUNC(clear_const, const uint8_t mask, const pixel_t value)
{
    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        if (mask & 1)
            x[i] = value;
        if (mask & 2)
            y[i] = value;
        if (mask & 4)
            z[i] = value;
        if (mask & 8)
            w[i] = value;
    }

    CONTINUE(pixel_t *, x, y, z, w);
}

#if BIT_DEPTH == 8
DECL_READ(read_nibbles, const int pixels, const int elems)
{
    pixel_t x[SWS_CHUNK_SIZE], y[SWS_CHUNK_SIZE],
            z[SWS_CHUNK_SIZE], w[SWS_CHUNK_SIZE];

    SWS_LOOP
    for (int i = 0; i < pixels; i += 2) {
        const pixel_t val = ((const pixel_t *) in0)[i >> 1];
        x[i + 0] = val >> 8;  /* high nibble */
        x[i + 1] = val & 0xF; /* low nibble */
    }

    CONTINUE(pixel_t *, x, y, z, w);
}

DECL_READ(read_bits, const int pixels, const int elems)
{
    pixel_t x[SWS_CHUNK_SIZE], y[SWS_CHUNK_SIZE],
            z[SWS_CHUNK_SIZE], w[SWS_CHUNK_SIZE];

    SWS_LOOP
    for (int i = 0; i < pixels; i += 8) {
        const pixel_t val = ((const pixel_t *) in0)[i >> 3];
        x[i + 0] = (val >> 7) & 1;
        x[i + 1] = (val >> 6) & 1;
        x[i + 2] = (val >> 5) & 1;
        x[i + 3] = (val >> 4) & 1;
        x[i + 4] = (val >> 3) & 1;
        x[i + 5] = (val >> 2) & 1;
        x[i + 6] = (val >> 1) & 1;
        x[i + 7] = (val >> 0) & 1;
    }

    CONTINUE(pixel_t *, x, y, z, w);
}

WRAP_READ(read_nibbles, 1, 1, false)
WRAP_READ(read_bits,    1, 3, false)

DECL_WRITE(write_nibbles, const int pixels, const int elems)
{
    SWS_LOOP
    for (int i = 0; i < pixels; i += 2)
        out0[i >> 1] = x[i] << 8 | x[i + 1];
}

DECL_WRITE(write_bits, const int pixels, const int elems)
{
    SWS_LOOP
    for (int i = pixels; i < FFALIGN(pixels, 8); i++)
        x[i] = 0; /* clear remaining bits in word */

    SWS_LOOP
    for (int i = 0; i < pixels; i += 8) {
        out0[i >> 3] = x[i + 0] << 7 |
                       x[i + 1] << 6 |
                       x[i + 2] << 5 |
                       x[i + 3] << 4 |
                       x[i + 4] << 3 |
                       x[i + 5] << 2 |
                       x[i + 6] << 1 |
                       x[i + 7];
    }
}

WRAP_WRITE(write_nibbles, 1, 1, false)
WRAP_WRITE(write_bits,    1, 3, false)
#endif /* BIT_DEPTH == 8 */

#ifdef SWAP_BYTES
DECL_IMPL(swap_bytes)
{
    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        x[i] = SWAP_BYTES(x[i]);
        y[i] = SWAP_BYTES(y[i]);
        z[i] = SWAP_BYTES(z[i]);
        w[i] = SWAP_BYTES(w[i]);
    }

    CONTINUE(pixel_t *, x, y, z, w);
}

DECL_ENTRY_SIMPLE(swap_bytes, .op = SWS_OP_SWAP_BYTES);
#endif /* SWAP_BYTES */

#if BIT_DEPTH == 8
DECL_IMPL(expand16)
{
    uint16_t xx[SWS_CHUNK_SIZE], yy[SWS_CHUNK_SIZE],
             zz[SWS_CHUNK_SIZE], ww[SWS_CHUNK_SIZE];

    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        xx[i] = x[i] << 8 | x[i];
        yy[i] = y[i] << 8 | y[i];
        zz[i] = z[i] << 8 | z[i];
        ww[i] = w[i] << 8 | w[i];
    }

    CONTINUE(uint16_t *, xx, yy, zz, ww);
}

DECL_ENTRY_SIMPLE(expand16,
    .op = SWS_OP_CONVERT,
    .convert.to = SWS_PIXEL_U16,
    .convert.expand = true,
);

DECL_IMPL(expand32)
{
    uint32_t xx[SWS_CHUNK_SIZE], yy[SWS_CHUNK_SIZE],
             zz[SWS_CHUNK_SIZE], ww[SWS_CHUNK_SIZE];

    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        xx[i] = x[i] << 24 | x[i] << 16 | x[i] << 8 | x[i];
        yy[i] = y[i] << 24 | y[i] << 16 | y[i] << 8 | y[i];
        zz[i] = z[i] << 24 | z[i] << 16 | z[i] << 8 | z[i];
        ww[i] = w[i] << 24 | w[i] << 16 | w[i] << 8 | w[i];
    }

    CONTINUE(uint32_t *, xx, yy, zz, ww);
}

DECL_ENTRY_SIMPLE(expand32,
    .op = SWS_OP_CONVERT,
    .convert.to = SWS_PIXEL_U32,
    .convert.expand = true,
);
#endif

#define WRAP_PACK_UNPACK(PACK_TYPE, PACK_PIXEL, X, Y, Z, W)                     \
inline DECL_IMPL(pack_##X##Y##Z##W)                                             \
{                                                                               \
    PACK_PIXEL xx[SWS_CHUNK_SIZE], yy[SWS_CHUNK_SIZE],                          \
               zz[SWS_CHUNK_SIZE], ww[SWS_CHUNK_SIZE];                          \
                                                                                \
    SWS_LOOP                                                                    \
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {                                  \
        xx[i] = x[i] << (Y+Z+W);                                                \
        if (Y)                                                                  \
            xx[i] |= y[i] << (Z+W);                                             \
        if (Z)                                                                  \
            xx[i] |= z[i] << W;                                                 \
        if (W)                                                                  \
            xx[i] |= w[i];                                                      \
    }                                                                           \
                                                                                \
    CONTINUE(PACK_PIXEL *, xx, yy, zz, ww);                                     \
}                                                                               \
                                                                                \
DECL_ENTRY_SIMPLE(pack_##X##Y##Z##W,                                            \
    .op = SWS_OP_PACK,                                                          \
    .pack.type = PACK_TYPE,                                                     \
    .pack.pattern = { X, Y, Z, W },                                             \
    .comps.unused = { !X, !Y, !Z, !W },                                         \
);                                                                              \
                                                                                \
inline static SWS_FUNC void                                                     \
fn(unpack_##X##Y##Z##W)(const SwsOpExec *restrict exec,                         \
                        const SwsOpImpl *restrict impl,                         \
                        PACK_PIXEL *restrict x, PACK_PIXEL *restrict y,         \
                        PACK_PIXEL *restrict z, PACK_PIXEL *restrict w)         \
{                                                                               \
    pixel_t xx[SWS_CHUNK_SIZE], yy[SWS_CHUNK_SIZE],                             \
            zz[SWS_CHUNK_SIZE], ww[SWS_CHUNK_SIZE];                             \
                                                                                \
    SWS_LOOP                                                                    \
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {                                  \
        const unsigned val = x[i];                                              \
        xx[i] = val >> (Y+Z+W);                                                 \
        if (Y)                                                                  \
            yy[i] = (val >> (Z+W)) & ((1 << Y) - 1);                            \
        if (Z)                                                                  \
            zz[i] = (val >> W) & ((1 << Z) - 1);                                \
        if (W)                                                                  \
            ww[i] = val & ((1 << W) - 1);                                       \
    }                                                                           \
                                                                                \
    CONTINUE(pixel_t *, xx, yy, zz, ww);                                        \
}                                                                               \
                                                                                \
DECL_ENTRY_SIMPLE(unpack_##X##Y##Z##W,                                          \
    .op = SWS_OP_UNPACK,                                                        \
    .pack.type = PACK_TYPE,                                                     \
    .pack.pattern = { X, Y, Z, W },                                             \
    .comps.flags = {                                                            \
        X ? 0 : SWS_COMP_GARBAGE, Y ? 0 : SWS_COMP_GARBAGE,                     \
        Z ? 0 : SWS_COMP_GARBAGE, W ? 0 : SWS_COMP_GARBAGE,                     \
    },                                                                          \
);

WRAP_PACK_UNPACK(SWS_PIXEL_U8,  uint8_t,   3,  3,  2,  0)
WRAP_PACK_UNPACK(SWS_PIXEL_U8,  uint8_t,   2,  3,  3,  0)
WRAP_PACK_UNPACK(SWS_PIXEL_U8,  uint8_t,   1,  2,  1,  0)
WRAP_PACK_UNPACK(SWS_PIXEL_U16, uint16_t,  5,  6,  5,  0)
WRAP_PACK_UNPACK(SWS_PIXEL_U16, uint16_t,  5,  5,  5,  0)
WRAP_PACK_UNPACK(SWS_PIXEL_U16, uint16_t,  4,  4,  4,  0)
WRAP_PACK_UNPACK(SWS_PIXEL_U32, uint32_t,  2, 10, 10, 10)
WRAP_PACK_UNPACK(SWS_PIXEL_U32, uint32_t, 10, 10, 10,  2)

#if BIT_DEPTH != 8
DECL_FUNC(lshift, const int amount)
{
    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        x[i] <<= amount;
        y[i] <<= amount;
        z[i] <<= amount;
        w[i] <<= amount;
    }

    CONTINUE(pixel_t *, x, y, z, w);
}

DECL_FUNC(rshift, const int amount)
{
    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        x[i] >>= amount;
        y[i] >>= amount;
        z[i] >>= amount;
        w[i] >>= amount;
    }

    CONTINUE(pixel_t *, x, y, z, w);
}

#define WRAP_SHIFT(N)                                                           \
DECL_IMPL(lshift_##N)                                                           \
{                                                                               \
    fn(lshift)(exec, impl, x, y, z, w, N);                                      \
}                                                                               \
                                                                                \
DECL_IMPL(rshift_##N)                                                           \
{                                                                               \
    fn(rshift)(exec, impl, x, y, z, w, N);                                      \
}                                                                               \
                                                                                \
DECL_ENTRY_SIMPLE(lshift_##N,                                                   \
    .op = SWS_OP_LSHIFT,                                                        \
    .shift.amount = N,                                                          \
);                                                                              \
                                                                                \
DECL_ENTRY_SIMPLE(rshift_##N,                                                   \
    .op = SWS_OP_RSHIFT,                                                        \
    .shift.amount = N,                                                          \
);

WRAP_SHIFT(1)
WRAP_SHIFT(2)
WRAP_SHIFT(3)
WRAP_SHIFT(4)
WRAP_SHIFT(5)
WRAP_SHIFT(6)
WRAP_SHIFT(7)
WRAP_SHIFT(8)
#endif /* BIT_DEPTH != 8 */

DECL_IMPL(convert_float)
{
    float xx[SWS_CHUNK_SIZE], yy[SWS_CHUNK_SIZE],
          zz[SWS_CHUNK_SIZE], ww[SWS_CHUNK_SIZE];

    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        xx[i] = x[i];
        yy[i] = y[i];
        zz[i] = z[i];
        ww[i] = w[i];
    }

    CONTINUE(float *, xx, yy, zz, ww);
}

DECL_ENTRY(convert_float,
    .op.op = SWS_OP_CONVERT,
    .op.convert.to = SWS_PIXEL_F32,
);

/**
 * Swizzle by directly swapping the order of arguments to the continuation.
 * Note that this is only safe to do if no arguments are duplicated.
 */
#define DECL_SWIZZLE(X, Y, Z, W)                                                \
static SWS_FUNC void                                                            \
fn(swizzle_##X##Y##Z##W)(const SwsOpExec *restrict exec,                        \
                         const SwsOpImpl *restrict impl,                        \
                         pixel_t *restrict c0, pixel_t *restrict c1,            \
                         pixel_t *restrict c2, pixel_t *restrict c3)            \
{                                                                               \
    CONTINUE(pixel_t *, c##X, c##Y, c##Z, c##W);                                \
}                                                                               \
                                                                                \
DECL_ENTRY_SIMPLE(swizzle_##X##Y##Z##W,                                         \
    .op = SWS_OP_SWIZZLE,                                                       \
    .swizzle = SWS_SWIZZLE(X, Y, Z, W),                                         \
);

DECL_SWIZZLE(0, 1, 2, 3)
DECL_SWIZZLE(3, 0, 1, 2)
DECL_SWIZZLE(2, 1, 0, 3)
DECL_SWIZZLE(3, 2, 1, 0)
DECL_SWIZZLE(3, 1, 0, 2)
DECL_SWIZZLE(3, 2, 0, 1)
DECL_SWIZZLE(1, 2, 0, 3)
DECL_SWIZZLE(1, 0, 2, 3)
DECL_SWIZZLE(2, 0, 1, 3)
DECL_SWIZZLE(2, 3, 1, 0)
DECL_SWIZZLE(2, 1, 3, 0)
DECL_SWIZZLE(1, 2, 3, 0)
DECL_SWIZZLE(0, 2, 1, 3)
DECL_SWIZZLE(0, 2, 3, 1)
DECL_SWIZZLE(0, 3, 1, 2)
DECL_SWIZZLE(3, 1, 2, 0)
DECL_SWIZZLE(0, 3, 2, 1)

/* Broadcast luma -> rgb (only used for y(a) -> rgb(a)) */
#define DECL_EXPAND_LUMA(X, W, T0, T1)                                          \
static SWS_FUNC void                                                            \
fn(expand_luma_##X##W)(const SwsOpExec *restrict exec,                          \
                       const SwsOpImpl *restrict impl,                          \
                       pixel_t *restrict c0, pixel_t *restrict c1,              \
                       pixel_t *restrict c2, pixel_t *restrict c3)              \
{                                                                               \
    SWS_LOOP                                                                    \
    for (int i = 0; i < SWS_CHUNK_SIZE; i++)                                    \
        T0[i] = T1[i] = c0[i];                                                  \
                                                                                \
    CONTINUE(pixel_t *, c##X, T0, T1, c##W);                                    \
}                                                                               \
                                                                                \
DECL_ENTRY_SIMPLE(expand_luma_##X##W,                                           \
    .op = SWS_OP_SWIZZLE,                                                       \
    .swizzle = SWS_SWIZZLE(X, 0, 0, W),                                         \
);

DECL_EXPAND_LUMA(0, 3, c1, c2)
DECL_EXPAND_LUMA(3, 0, c1, c2)
DECL_EXPAND_LUMA(1, 0, c2, c3)
DECL_EXPAND_LUMA(0, 1, c2, c3)

static const SwsOpTable fn(op_table_int) = {
    .block_w = SWS_CHUNK_SIZE,
    .block_h = 1,
    .entries = {
        fn(op_read_packed1),
        fn(op_read_packed2),
        fn(op_read_packed3),
        fn(op_read_packed4),
        fn(op_read_planar2),
        fn(op_read_planar3),
        fn(op_read_planar4),

        fn(op_write_packed1),
        fn(op_write_packed2),
        fn(op_write_packed3),
        fn(op_write_packed4),
        fn(op_write_planar2),
        fn(op_write_planar3),
        fn(op_write_planar4),

#if BIT_DEPTH == 8
        fn(op_read_bits1),
        fn(op_read_nibbles1),
        fn(op_write_bits1),
        fn(op_write_nibbles1),

        fn(op_pack_1210),
        fn(op_pack_2330),
        fn(op_pack_3320),
        fn(op_pack_4440),
        fn(op_pack_5550),
        fn(op_pack_5650),

        fn(op_unpack_1210),
        fn(op_unpack_2330),
        fn(op_unpack_3320),
        fn(op_unpack_4440),
        fn(op_unpack_5550),
        fn(op_unpack_5650),

        fn(op_expand16),
        fn(op_expand32),
#elif BIT_DEPTH == 16
        fn(op_pack_2101010),
        fn(op_pack_1010102),
        fn(op_unpack_2101010),
        fn(op_unpack_1010102),
#endif

#ifdef SWAP_BYTES
        fn(op_swap_bytes),
#endif

        fn(op_scale),
        fn(op_convert_float),

        fn(op_clear_1110),
        fn(op_clear_0111),
        fn(op_clear_0011),
        fn(op_clear_1001),
        fn(op_clear_1100),
        fn(op_clear_0101),
        fn(op_clear_1010),
        fn(op_clear_1000),
        fn(op_clear_0100),
        fn(op_clear_0010),

        fn(op_swizzle_0123),
        fn(op_swizzle_3012),
        fn(op_swizzle_2103),
        fn(op_swizzle_3210),
        fn(op_swizzle_3102),
        fn(op_swizzle_3201),
        fn(op_swizzle_1203),
        fn(op_swizzle_1023),
        fn(op_swizzle_2013),
        fn(op_swizzle_2310),
        fn(op_swizzle_2130),
        fn(op_swizzle_1230),
        fn(op_swizzle_0213),
        fn(op_swizzle_0231),
        fn(op_swizzle_0312),
        fn(op_swizzle_3120),
        fn(op_swizzle_0321),

        fn(op_expand_luma_03),
        fn(op_expand_luma_30),
        fn(op_expand_luma_10),
        fn(op_expand_luma_01),

#if BIT_DEPTH != 8
        fn(op_lshift_1),
        fn(op_lshift_2),
        fn(op_lshift_3),
        fn(op_lshift_4),
        fn(op_lshift_5),
        fn(op_lshift_6),
        fn(op_lshift_7),
        fn(op_lshift_8),

        fn(op_rshift_1),
        fn(op_rshift_2),
        fn(op_rshift_3),
        fn(op_rshift_4),
        fn(op_rshift_5),
        fn(op_rshift_6),
        fn(op_rshift_7),
        fn(op_rshift_8),

        fn(op_convert_uint8),
#endif /* BIT_DEPTH != 8 */

#if BIT_DEPTH != 16
        fn(op_convert_uint16),
#endif
#if BIT_DEPTH != 32
        fn(op_convert_uint32),
#endif

        {{0}}
    },
};

#undef PIXEL_TYPE
#undef PIXEL_MAX
#undef SWAP_BYTES
#undef pixel_t

#undef FMT_CHAR
#undef IS_FLOAT
