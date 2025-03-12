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

#include "libavutil/intreadwrite.h"
#include "libavutil/bswap.h"

#include "ops_internal.h"
#include "ops_tmpl_common.h"

/* Extend the border of a partially read chunk */
static av_always_inline void
pad_input(tmp_t *restrict out, const int pixels, const int elems)
{
    const pixel_t edge_x = out->x.px[pixels - 1];
    const pixel_t edge_y = out->y.px[pixels - 1];
    const pixel_t edge_z = out->z.px[pixels - 1];
    const pixel_t edge_w = out->w.px[pixels - 1];

    SWS_LOOP
    for (int i = pixels; i < SWS_CHUNK_SIZE; i++) {
        out->x.px[i] = edge_x;
        if (elems > 1)
            out->y.px[i] = edge_y;
        if (elems > 2)
            out->z.px[i] = edge_z;
        if (elems > 3)
            out->w.px[i] = edge_w;
    }
}

static av_always_inline void
read_packed(const plane_t *restrict in0, const plane_t *restrict in1,
            const plane_t *restrict in2, const plane_t *restrict in3,
            tmp_t *restrict out, const int pixels, const int elems)
{
    SWS_LOOP
    for (int i = 0; i < pixels; i++) {
        out->x.px[i] = ((const pixel_t *) in0)[elems * i + 0];
        if (elems > 1)
            out->y.px[i] = ((const pixel_t *) in0)[elems * i + 1];
        if (elems > 2)
            out->z.px[i] = ((const pixel_t *) in0)[elems * i + 2];
        if (elems > 3)
            out->w.px[i] = ((const pixel_t *) in0)[elems * i + 3];
    }

    pad_input(out, pixels, elems);
}

static av_always_inline void
read_planar(const plane_t *restrict in0, const plane_t *restrict in1,
            const plane_t *restrict in2, const plane_t *restrict in3,
            tmp_t *restrict out, const int pixels, const int elems)
{
    SWS_LOOP
    for (int i = 0; i < pixels; i++) {
        out->x.px[i] = ((const pixel_t *) in0)[i];
        if (elems > 1)
            out->y.px[i] = ((const pixel_t *) in1)[i];
        if (elems > 2)
            out->z.px[i] = ((const pixel_t *) in2)[i];
        if (elems > 3)
            out->w.px[i] = ((const pixel_t *) in3)[i];
    }

    pad_input(out, pixels, elems);
}

#define WRAP_READ(FUNC, ELEMS, FRAC, PLANAR)                                    \
static SWS_FUNC void FUNC##ELEMS(tmp_t *out, int pixels,                        \
                                 const plane_t *in0, const plane_t *in1,        \
                                 const plane_t *in2, const plane_t *in3,        \
                                 const void *priv)                              \
{                                                                               \
    FUNC(SWS_ASSUME_ALIGNED(in0), SWS_ASSUME_ALIGNED(in1),                      \
         SWS_ASSUME_ALIGNED(in2), SWS_ASSUME_ALIGNED(in3),                      \
         SWS_ASSUME_ALIGNED(out), SWS_CHUNK_SIZE, ELEMS);                       \
}                                                                               \
                                                                                \
static SWS_FUNC void FUNC##ELEMS##_n(tmp_t *out, int pixels,                    \
                                     const plane_t *in0, const plane_t *in1,    \
                                     const plane_t *in2, const plane_t *in3,    \
                                     const void *priv)                          \
{                                                                               \
    SWS_ASSUME(pixels <= SWS_CHUNK_SIZE);                                       \
    FUNC(in0, in1, in2, in3, out, pixels, ELEMS);                               \
}                                                                               \
                                                                                \
static const SwsOpEntry op_##FUNC##ELEMS = {                                    \
    .num_ops = 1,                                                               \
    .ops = (const SwsOp[]) {{                                                   \
        .type = PIXEL_TYPE,                                                     \
        .op   = SWS_OP_READ,                                                    \
        .rw = {                                                                 \
            .elems  = ELEMS,                                                    \
            .planar = PLANAR,                                                   \
            .frac   = FRAC,                                                     \
        },                                                                      \
    }},                                                                         \
    .read   = FUNC##ELEMS,                                                      \
    .read_n = FUNC##ELEMS##_n,                                                  \
};

WRAP_READ(read_packed, 1, 0, false)
WRAP_READ(read_packed, 2, 0, false)
WRAP_READ(read_packed, 3, 0, false)
WRAP_READ(read_packed, 4, 0, false)
/* No such thing as 1-component planar */
WRAP_READ(read_planar, 2, 0, true)
WRAP_READ(read_planar, 3, 0, true)
WRAP_READ(read_planar, 4, 0, true)

static av_always_inline void
write_packed(plane_t *restrict out0, plane_t *restrict out1,
             plane_t *restrict out2, plane_t *restrict out3,
             const tmp_t *restrict in, const int pixels, const int elems)
{
    SWS_LOOP
    for (int i = 0; i < pixels; i++) {
        ((pixel_t *) out0)[elems * i + 0] = in->x.px[i];
        if (elems > 1)
            ((pixel_t *) out0)[elems * i + 1] = in->y.px[i];
        if (elems > 2)
            ((pixel_t *) out0)[elems * i + 2] = in->z.px[i];
        if (elems > 3)
            ((pixel_t *) out0)[elems * i + 3] = in->w.px[i];
    }
}

static av_always_inline void
write_planar(plane_t *restrict out0, plane_t *restrict out1,
             plane_t *restrict out2, plane_t *restrict out3,
             const tmp_t *restrict in, const int pixels, const int elems)
{
    SWS_LOOP
    for (int i = 0; i < pixels; i++) {
        ((pixel_t *) out0)[i] = in->x.px[i];
        if (elems > 1)
            ((pixel_t *) out1)[i] = in->y.px[i];
        if (elems > 2)
            ((pixel_t *) out2)[i] = in->z.px[i];
        if (elems > 3)
            ((pixel_t *) out3)[i] = in->w.px[i];
    }
}

#define WRAP_WRITE(FUNC, ELEMS, FRAC, PLANAR)                                   \
static SWS_FUNC void FUNC##ELEMS(const tmp_t *in, int pixels,                   \
                                 plane_t *out0, plane_t *out1,                  \
                                 plane_t *out2, plane_t *out3,                  \
                                 const void *priv)                              \
{                                                                               \
    FUNC(SWS_ASSUME_ALIGNED(out0), SWS_ASSUME_ALIGNED(out1),                    \
         SWS_ASSUME_ALIGNED(out2), SWS_ASSUME_ALIGNED(out3),                    \
         in, SWS_CHUNK_SIZE, ELEMS);                                            \
}                                                                               \
                                                                                \
static SWS_FUNC void FUNC##ELEMS##_n(const tmp_t *in, int pixels,               \
                                     plane_t *out0, plane_t *out1,              \
                                     plane_t *out2, plane_t *out3,              \
                                     const void *priv)                          \
{                                                                               \
    SWS_ASSUME(pixels <= SWS_CHUNK_SIZE);                                       \
    FUNC(out0, out1, out2, out3, in, pixels, ELEMS);                            \
}                                                                               \
                                                                                \
static const SwsOpEntry op_##FUNC##ELEMS = {                                    \
    .num_ops = 1,                                                               \
    .ops = (const SwsOp[]) {{                                                   \
        .type     = PIXEL_TYPE,                                                 \
        .op       = SWS_OP_WRITE,                                               \
        .rw = {                                                                 \
            .elems  = ELEMS,                                                    \
            .planar = PLANAR,                                                   \
            .frac   = FRAC,                                                     \
        },                                                                      \
    }},                                                                         \
    .write   = FUNC##ELEMS,                                                     \
    .write_n = FUNC##ELEMS##_n,                                                 \
};

WRAP_WRITE(write_packed, 1, 0, false)
WRAP_WRITE(write_packed, 2, 0, false)
WRAP_WRITE(write_packed, 3, 0, false)
WRAP_WRITE(write_packed, 4, 0, false)
WRAP_WRITE(write_planar, 2, 0, true)
WRAP_WRITE(write_planar, 3, 0, true)
WRAP_WRITE(write_planar, 4, 0, true)

#if BIT_DEPTH == 8
static av_always_inline void
read_nibbles(const plane_t *restrict in0, const plane_t *restrict in1,
             const plane_t *restrict in2, const plane_t *restrict in3,
             tmp_t *restrict out, const int pixels, const int elems)
{
    SWS_LOOP
    for (int i = 0; i < pixels; i += 2) {
        const pixel_t val = ((const pixel_t *) in0)[i >> 1];
        out->x.px[i + 0] = val >> 8;  /* high nibble */
        out->x.px[i + 1] = val & 0xF; /* low nibble */
    }

    /* This also fixes any extra elements written by the loop when `pixels`
     * is not a clean multiple of 2 */
    pad_input(out, pixels, 1);
}

static av_always_inline void
read_bits(const plane_t *restrict in0, const plane_t *restrict in1,
          const plane_t *restrict in2, const plane_t *restrict in3,
          tmp_t *restrict out, const int pixels, const int elems)
{
    SWS_LOOP
    for (int i = 0; i < pixels; i += 8) {
        const pixel_t val = ((const pixel_t *) in0)[i >> 3];
        out->x.px[i + 0] = (val >> 7) & 1;
        out->x.px[i + 1] = (val >> 6) & 1;
        out->x.px[i + 2] = (val >> 5) & 1;
        out->x.px[i + 3] = (val >> 4) & 1;
        out->x.px[i + 4] = (val >> 3) & 1;
        out->x.px[i + 5] = (val >> 2) & 1;
        out->x.px[i + 6] = (val >> 1) & 1;
        out->x.px[i + 7] = (val >> 0) & 1;
    }

    pad_input(out, pixels, 1);
}

WRAP_READ(read_nibbles, 1, 1, false)
WRAP_READ(read_bits,    1, 3, false)

static av_always_inline void
write_nibbles(plane_t *restrict out0, plane_t *restrict out1,
              plane_t *restrict out2, plane_t *restrict out3,
              const tmp_t *restrict in, const int pixels, const int frac)
{
    tmp_t tmp = *in;
    if (pixels & 1)
        tmp.x.px[pixels] = 0;

    SWS_LOOP
    for (int i = 0; i < pixels; i += 2) {
        ((pixel_t *) out0)[i >> 1] = tmp.x.px[i] << 8 | tmp.x.px[i + 1];
    }
}

static av_always_inline void
write_bits(plane_t *restrict out0, plane_t *restrict out1,
           plane_t *restrict out2, plane_t *restrict out3,
           const tmp_t *restrict in, const int pixels, const int frac)
{
    tmp_t tmp = *in;
    SWS_LOOP
    for (int i = pixels; i < FFALIGN(pixels, 8); i++)
        tmp.x.px[i] = 0; /* clear remaining bits in word */

    SWS_LOOP
    for (int i = 0; i < pixels; i += 8) {
        ((pixel_t *) out0)[i >> 3] = tmp.x.px[i + 0] << 7 |
                                     tmp.x.px[i + 1] << 6 |
                                     tmp.x.px[i + 2] << 5 |
                                     tmp.x.px[i + 3] << 4 |
                                     tmp.x.px[i + 4] << 3 |
                                     tmp.x.px[i + 5] << 2 |
                                     tmp.x.px[i + 6] << 1 |
                                     tmp.x.px[i + 7];
    }
}

WRAP_WRITE(write_nibbles, 1, 1, false)
WRAP_WRITE(write_bits,    1, 3, false)
#endif /* BIT_DEPTH == 8 */

static av_always_inline void swizzle(tmp_t *inout, const SwsSwizzleOp swizzle)
{
    const chunk_t in[4] = { inout->x, inout->y, inout->z, inout->w };

    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        inout->x.px[i] = in[swizzle.x].px[i];
        inout->y.px[i] = in[swizzle.y].px[i];
        inout->z.px[i] = in[swizzle.z].px[i];
        inout->w.px[i] = in[swizzle.w].px[i];
    }
}

#define WRAP_SWIZZLE(X, Y, Z, W)                                                \
static SWS_FUNC void                                                            \
swizzle_##X##Y##Z##W(tmp_t *inout, int y, const void *priv)                     \
{                                                                               \
    swizzle(SWS_ASSUME_ALIGNED(inout), SWS_SWIZZLE(X, Y, Z, W));                \
}                                                                               \
                                                                                \
static const SwsOpEntry op_swizzle_##X##Y##Z##W = {                             \
    .num_ops = 1,                                                               \
    .ops = (const SwsOp[]) {{                                                   \
        .type    = PIXEL_TYPE,                                                  \
        .op      = SWS_OP_SWIZZLE,                                              \
        .swizzle = SWS_SWIZZLE(X, Y, Z, W),                                     \
    }},                                                                         \
    .op = swizzle_##X##Y##Z##W,                                                 \
};

WRAP_SWIZZLE(0, 1, 2, 3)
WRAP_SWIZZLE(3, 0, 1, 2)
WRAP_SWIZZLE(2, 1, 0, 3)
WRAP_SWIZZLE(3, 2, 1, 0)
WRAP_SWIZZLE(3, 1, 0, 2)
WRAP_SWIZZLE(3, 2, 0, 1)
WRAP_SWIZZLE(1, 2, 0, 3)
WRAP_SWIZZLE(1, 0, 2, 3)
WRAP_SWIZZLE(2, 0, 1, 3)
WRAP_SWIZZLE(2, 3, 1, 0)
WRAP_SWIZZLE(2, 1, 3, 0)
WRAP_SWIZZLE(1, 2, 3, 0)
WRAP_SWIZZLE(0, 2, 1, 3)
WRAP_SWIZZLE(0, 2, 3, 1)
WRAP_SWIZZLE(0, 3, 1, 2)
WRAP_SWIZZLE(3, 1, 2, 0)
WRAP_SWIZZLE(0, 3, 2, 1)

/* Broadcasting swizzles (only used for gray -> rgb(a)) */
WRAP_SWIZZLE(0, 0, 0, 3)
WRAP_SWIZZLE(0, 0, 0, 0)
WRAP_SWIZZLE(1, 0, 0, 0)
WRAP_SWIZZLE(0, 0, 0, 1)

/* Fast path for directly swizzling packed bytes */
static av_always_inline void
packed_swizzle(plane_t *restrict out, const plane_t *restrict in,
               const int pixels, const int elems,
               const SwsSwizzleOp swizzle)
{
    const pixel_t *inp = (const pixel_t *) in;
    pixel_t *outp = (pixel_t *) out;

    SWS_LOOP
    for (int i = 0; i < pixels; i++) {
        outp[elems * i + 0] = inp[elems * i + swizzle.x];
        if (elems > 1)
            outp[elems * i + 1] = inp[elems * i + swizzle.y];
        if (elems > 2)
            outp[elems * i + 2] = inp[elems * i + swizzle.z];
        if (elems > 3)
            outp[elems * i + 3] = inp[elems * i + swizzle.w];
    }
}

#define WRAP_PACKED_SWIZZLE(N, X, Y, Z, W)                                      \
static SWS_FUNC void                                                            \
packed##N##_swizzle_##X##Y##Z##W(int pixels,                                    \
                                 plane_t *out0, const plane_t *in0,             \
                                 plane_t *out1, const plane_t *in1,             \
                                 plane_t *out2, const plane_t *in2,             \
                                 plane_t *out3, const plane_t *in3,             \
                                 const void *priv)                              \
{                                                                               \
    packed_swizzle(out0, in0, pixels, N, SWS_SWIZZLE(X, Y, Z, W));              \
}                                                                               \
                                                                                \
static const SwsOpEntry op_packed##N##_swizzle_##X##Y##Z##W = {                 \
    .num_ops = 3,                                                               \
    .ops = (const SwsOp[]) {{                                                   \
        .type     = PIXEL_TYPE,                                                 \
        .op       = SWS_OP_READ,                                                \
        .rw.elems = N,                                                          \
    }, {                                                                        \
        .type     = PIXEL_TYPE,                                                 \
        .op       = SWS_OP_SWIZZLE,                                             \
        .swizzle  = SWS_SWIZZLE(X, Y, Z, W),                                    \
    }, {                                                                        \
        .type     = PIXEL_TYPE,                                                 \
        .op       = SWS_OP_WRITE,                                               \
        .rw.elems = N,                                                          \
    }},                                                                         \
    .read_write = packed##N##_swizzle_##X##Y##Z##W,                             \
};

WRAP_PACKED_SWIZZLE(3, 2, 1, 0, 3) /* rgb24 <-> bgr24 */

WRAP_PACKED_SWIZZLE(4, 3, 0, 1, 2) /* rgba   -> argb */
WRAP_PACKED_SWIZZLE(4, 1, 2, 3, 0) /* argb   -> rgba */
WRAP_PACKED_SWIZZLE(4, 2, 1, 0, 3) /* rgba  <-> bgra */
WRAP_PACKED_SWIZZLE(4, 3, 2, 1, 0) /* rgba  <-> abgr */
WRAP_PACKED_SWIZZLE(4, 1, 2, 0, 3) /* rgba   -> gbra */
WRAP_PACKED_SWIZZLE(4, 2, 0, 1, 3) /* gbra   -> rgba */

/* Fast path for fused read/write + conversion to float */
static av_always_inline void
to_float(tmp_t *restrict out, const tmp_t *restrict in, const int elems)
{
    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        out->x.f32[i] = in->x.px[i];
        if (elems > 1)
            out->y.f32[i] = in->y.px[i];
        if (elems > 2)
            out->z.f32[i] = in->z.px[i];
        if (elems > 3)
            out->w.f32[i] = in->w.px[i];
    }
}

static av_always_inline void
from_float(tmp_t *restrict out, const tmp_t *restrict in, const int elems)
{
    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        out->x.px[i] = in->x.f32[i];
        if (elems > 1)
            out->y.px[i] = in->y.f32[i];
        if (elems > 2)
            out->z.px[i] = in->z.f32[i];
        if (elems > 3)
            out->w.px[i] = in->w.f32[i];
    }
}

#define WRAP_READ_FLOAT(FUNC, ELEMS, PLANAR)                                    \
static SWS_FUNC void FUNC##ELEMS##f(tmp_t *out, int pixels,                     \
                                    const plane_t *in0, const plane_t *in1,     \
                                    const plane_t *in2, const plane_t *in3,     \
                                    const void *priv)                           \
{                                                                               \
    tmp_t tmp;                                                                  \
    FUNC(SWS_ASSUME_ALIGNED(in0), SWS_ASSUME_ALIGNED(in1),                      \
         SWS_ASSUME_ALIGNED(in2), SWS_ASSUME_ALIGNED(in3),                      \
         &tmp, SWS_CHUNK_SIZE, ELEMS);                                          \
    to_float(out, &tmp, ELEMS);                                                 \
}                                                                               \
                                                                                \
static SWS_FUNC void FUNC##ELEMS##f_n(tmp_t *out, int pixels,                   \
                                      const plane_t *in0, const plane_t *in1,   \
                                      const plane_t *in2, const plane_t *in3,   \
                                      const void *priv)                         \
{                                                                               \
    tmp_t tmp;                                                                  \
    SWS_ASSUME(pixels <= SWS_CHUNK_SIZE);                                       \
    FUNC(in0, in1, in2, in3, &tmp, pixels, ELEMS);                              \
    to_float(out, &tmp, ELEMS);                                                 \
}                                                                               \
                                                                                \
static const SwsOpEntry op_##FUNC##ELEMS##f = {                                 \
    .num_ops = 2,                                                               \
    .ops = (const SwsOp[]) {{                                                   \
        .type = PIXEL_TYPE,                                                     \
        .op   = SWS_OP_READ,                                                    \
        .rw = {                                                                 \
            .elems  = ELEMS,                                                    \
            .planar = PLANAR,                                                   \
        },                                                                      \
    }, {                                                                        \
        .type = PIXEL_TYPE,                                                     \
        .op   = SWS_OP_CONVERT,                                                 \
        .convert.to = SWS_PIXEL_F32,                                            \
        .comps.unused = { ELEMS < 1, ELEMS < 2, ELEMS < 3, ELEMS < 4 },         \
    }},                                                                         \
    .read   = FUNC##ELEMS##f,                                                   \
    .read_n = FUNC##ELEMS##f_n,                                                 \
};

WRAP_READ_FLOAT(read_packed, 1, false)
WRAP_READ_FLOAT(read_packed, 2, false)
WRAP_READ_FLOAT(read_packed, 3, false)
WRAP_READ_FLOAT(read_packed, 4, false)
WRAP_READ_FLOAT(read_planar, 2, true)
WRAP_READ_FLOAT(read_planar, 3, true)
WRAP_READ_FLOAT(read_planar, 4, true)

#ifdef SWAP_BYTES
static SWS_FUNC void swap_bytes(tmp_t *restrict inout, int y, const void *priv)
{
    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        inout->x.px[i] = SWAP_BYTES(inout->x.px[i]);
        inout->y.px[i] = SWAP_BYTES(inout->y.px[i]);
        inout->z.px[i] = SWAP_BYTES(inout->z.px[i]);
        inout->w.px[i] = SWAP_BYTES(inout->w.px[i]);
    }
}

static const SwsOpEntry op_swap_bytes = {
    .num_ops = 1,                                                               \
    .ops = (const SwsOp[]) {{
        .type = PIXEL_TYPE,
        .op   = SWS_OP_SWAP_BYTES,
    }},
    .op  = swap_bytes,
};

/* Fast path for in-place byte swapping as a single pass */
static av_always_inline void
swap_packed(const int pixels, const int elems,
            plane_t *restrict out0, const plane_t *restrict in0,
            plane_t *restrict out1, const plane_t *restrict in1,
            plane_t *restrict out2, const plane_t *restrict in2,
            plane_t *restrict out3, const plane_t *restrict in3)
{
    SWS_LOOP
    for (int i = 0; i < elems * pixels; i++)
        ((pixel_t *) out0)[i] = SWAP_BYTES(((const pixel_t *) in0)[i]);
}

static av_always_inline void
swap_planar(const int pixels, const int elems,
            plane_t *restrict out0, const plane_t *restrict in0,
            plane_t *restrict out1, const plane_t *restrict in1,
            plane_t *restrict out2, const plane_t *restrict in2,
            plane_t *restrict out3, const plane_t *restrict in3)
{
    SWS_LOOP
    for (int i = 0; i < pixels; i++) {
        ((pixel_t *) out0)[i] = SWAP_BYTES(((const pixel_t *) in0)[i]);
        if (elems > 1)
            ((pixel_t *) out1)[i] = SWAP_BYTES(((const pixel_t *) in1)[i]);
        if (elems > 2)
            ((pixel_t *) out2)[i] = SWAP_BYTES(((const pixel_t *) in2)[i]);
        if (elems > 3)
            ((pixel_t *) out3)[i] = SWAP_BYTES(((const pixel_t *) in3)[i]);
    }
}

#define WRAP_SWAP(NAME, ELEMS, PLANAR)                                          \
static SWS_FUNC void                                                            \
swap_##NAME##ELEMS(int pixels,                                                  \
                   plane_t *out0, const plane_t *in0,                           \
                   plane_t *out1, const plane_t *in1,                           \
                   plane_t *out2, const plane_t *in2,                           \
                   plane_t *out3, const plane_t *in3,                           \
                   const void *priv)                                            \
{                                                                               \
    swap_##NAME(pixels, ELEMS, out0, in0, out1, in1, out2, in2, out3, in3);     \
}                                                                               \
                                                                                \
static const SwsOpEntry op_swap_##NAME##ELEMS = {                               \
    .num_ops = 3,                                                               \
    .ops = (const SwsOp[]) {{                                                   \
        .type     = PIXEL_TYPE,                                                 \
        .op       = SWS_OP_READ,                                                \
        .rw = {                                                                 \
            .elems  = ELEMS,                                                    \
            .planar = PLANAR,                                                   \
        },                                                                      \
    }, {                                                                        \
        .type     = PIXEL_TYPE,                                                 \
        .op       = SWS_OP_SWAP_BYTES,                                          \
    }, {                                                                        \
        .type     = PIXEL_TYPE,                                                 \
        .op       = SWS_OP_WRITE,                                               \
        .rw = {                                                                 \
            .elems  = ELEMS,                                                    \
            .planar = PLANAR,                                                   \
        },                                                                      \
    }},                                                                         \
    .read_write = swap_##NAME##ELEMS,                                           \
};

WRAP_SWAP(packed, 1, false)
WRAP_SWAP(packed, 2, false)
WRAP_SWAP(packed, 3, false)
WRAP_SWAP(packed, 4, false)
WRAP_SWAP(planar, 1, true)
WRAP_SWAP(planar, 2, true)
WRAP_SWAP(planar, 3, true)
WRAP_SWAP(planar, 4, true)
#endif /* SWAP_BYTES */

#if BIT_DEPTH != 8
static av_always_inline void
lshift(tmp_t *restrict inout, const uint8_t amount,
       const bool x, const bool y, const bool z, const bool w)
{
    SWS_ASSUME(amount < BIT_DEPTH);

    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        if (x)
            inout->x.px[i] <<= amount;
        if (y)
            inout->y.px[i] <<= amount;
        if (z)
            inout->z.px[i] <<= amount;
        if (w)
            inout->w.px[i] <<= amount;
    }
}

static av_always_inline void
rshift(tmp_t *restrict inout, const uint8_t amount,
       const bool x, const bool y, const bool z, const bool w)
{
    SWS_ASSUME(amount < BIT_DEPTH);

    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        if (x)
            inout->x.px[i] >>= amount;
        if (y)
            inout->y.px[i] >>= amount;
        if (z)
            inout->z.px[i] >>= amount;
        if (w)
            inout->w.px[i] >>= amount;
    }
}

#define WRAP_SHIFT(N, X, Y, Z, W)                                               \
static SWS_FUNC void                                                            \
lshift##N##_##X##Y##Y##W(tmp_t *inout, int y, const void *priv)                 \
{                                                                               \
    lshift(inout, N, X, Y, Z, W);                                               \
}                                                                               \
                                                                                \
static SWS_FUNC void                                                            \
rshift##N##_##X##Y##Y##W(tmp_t *inout, int y, const void *priv)                 \
{                                                                               \
    rshift(inout, N, X, Y, Z, W);                                               \
}                                                                               \
                                                                                \
static const SwsOpEntry op_lshift##N##_##X##Y##Z##W = {                         \
    .num_ops = 1,                                                               \
    .ops = (const SwsOp[]) {{                                                   \
        .type = PIXEL_TYPE,                                                     \
        .op   = SWS_OP_LSHIFT,                                                  \
        .shift.amount = N,                                                      \
        .comps.unused = { !X, !Y, !Z, !W },                                     \
    }},                                                                         \
    .op = lshift##N##_##X##Y##Z##W,                                             \
};                                                                              \
                                                                                \
static const SwsOpEntry op_rshift##N##_##X##Y##Z##W = {                         \
    .num_ops = 1,                                                               \
    .ops = (const SwsOp[]) {{                                                   \
        .type = PIXEL_TYPE,                                                     \
        .op   = SWS_OP_RSHIFT,                                                  \
        .shift.amount = N,                                                      \
        .comps.unused = { !X, !Y, !Z, !W },                                     \
    }},                                                                         \
    .op = rshift##N##_##X##Y##Z##W,                                             \
};

WRAP_SHIFT(1, 1, 1, 1, 0)
WRAP_SHIFT(2, 1, 1, 1, 0)
WRAP_SHIFT(3, 1, 1, 1, 0)
WRAP_SHIFT(4, 1, 1, 1, 0)
WRAP_SHIFT(5, 1, 1, 1, 0)
WRAP_SHIFT(6, 1, 1, 1, 0)
WRAP_SHIFT(7, 1, 1, 1, 0)
WRAP_SHIFT(8, 1, 1, 1, 0)
#endif /* BIT_DEPTH != 8 */

static av_always_inline void unpack(tmp_t *restrict inout, const SwsPackOp pack)
{
    const chunk_t in = inout->x;
    const int shift2 = pack.pattern[3];
    const int shift1 = pack.pattern[2] + shift2;
    const int shift0 = pack.pattern[1] + shift1;
    unsigned val;

    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        switch (pack.type) {
        case SWS_PIXEL_U8:  val = in.u8[i];  break;
        case SWS_PIXEL_U16: val = in.u16[i]; break;
        case SWS_PIXEL_U32: val = in.u32[i]; break;
        default: val = 0;
        }

        inout->x.px[i] = val >> shift0;
        if (pack.pattern[1])
            inout->y.px[i] = (val >> shift1) & ((1 << pack.pattern[1]) - 1);
        if (pack.pattern[2])
            inout->z.px[i] = (val >> shift2) & ((1 << pack.pattern[2]) - 1);
        if (pack.pattern[3])
            inout->w.px[i] = val & ((1 << pack.pattern[3]) - 1);
    }
}

static av_always_inline void pack(tmp_t *restrict inout, const SwsPackOp pack)
{
    const tmp_t in = *inout;
    const int shift2 = pack.pattern[3];
    const int shift1 = pack.pattern[2] + shift2;
    const int shift0 = pack.pattern[1] + shift1;

    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        unsigned val = in.x.px[i] << shift0;
        if (pack.pattern[1])
            val |= in.y.px[i] << shift1;
        if (pack.pattern[2])
            val |= in.z.px[i] << shift2;
        if (pack.pattern[3])
            val |= in.w.px[i];

        switch (pack.type) {
        case SWS_PIXEL_U8:  inout->x.u8[i]  = val; break;
        case SWS_PIXEL_U16: inout->x.u16[i] = val; break;
        case SWS_PIXEL_U32: inout->x.u32[i] = val; break;
        default: break;
        }
    }
}

#define WRAP_PACK(NAME, OP, SIZE, X, Y, Z, W)                                   \
static SWS_FUNC void NAME##X##Y##Z##W(tmp_t *inout, int y, const void *priv)    \
{                                                                               \
    NAME(inout, (SwsPackOp) {                                                   \
        .type    = SWS_PIXEL_U##SIZE,                                           \
        .pattern = { X, Y, Z, W },                                              \
    });                                                                         \
}                                                                               \
                                                                                \
static const SwsOpEntry op_##NAME##X##Y##Z##W = {                               \
    .num_ops = 1,                                                               \
    .ops = (const SwsOp[]) {{                                                   \
        .type = PIXEL_TYPE,                                                     \
        .op   = SWS_OP_##OP,                                                    \
        .pack = {                                                               \
            .type    = SWS_PIXEL_U##SIZE,                                       \
            .pattern = { X, Y, Z, W },                                          \
        },                                                                      \
    }},                                                                         \
    .op = NAME##X##Y##Z##W,                                                     \
};

#define WRAP_PACK_UNPACK(SIZE, X, Y, Z, W)                                      \
    WRAP_PACK(pack,   PACK,   SIZE, X, Y, Z, W)                                 \
    WRAP_PACK(unpack, UNPACK, SIZE, X, Y, Z, W)

WRAP_PACK_UNPACK(32, 2, 10, 10, 10)
WRAP_PACK_UNPACK(32, 10, 10, 10, 2)
WRAP_PACK_UNPACK(16, 5, 6, 5, 0)
WRAP_PACK_UNPACK(16, 5, 5, 5, 0)
WRAP_PACK_UNPACK(16, 4, 4, 4, 0)
WRAP_PACK_UNPACK(8,  3, 3, 2, 0)
WRAP_PACK_UNPACK(8,  2, 3, 3, 0)
WRAP_PACK_UNPACK(8,  1, 2, 1, 0)

static av_always_inline pixel_t expand8(uint8_t x)
{
    switch (BIT_DEPTH) {
    case 8:  return x;
    case 16: return x << 8  | x;
    case 32: return x << 24 | x << 16 | x << 8 | x;
    }
}

static SWS_FUNC void
expand8_pattern(tmp_t *restrict inout, bool x, bool y, bool z, bool w)
{
    const tmp_t in = *inout;

    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        if (x)
            inout->x.px[i] = expand8(in.x.u8[i]);
        if (y)
            inout->y.px[i] = expand8(in.y.u8[i]);
        if (z)
            inout->z.px[i] = expand8(in.z.u8[i]);
        if (w)
            inout->w.px[i] = expand8(in.w.u8[i]);
    }
}

#define WRAP_EXPAND8(X, Y, Z, W)                                                \
static SWS_FUNC void                                                            \
expand8_##X##Y##Z##W(tmp_t *restrict inout, int y, const void *restrict priv)   \
{                                                                               \
    expand8_pattern(inout, X, Y, Z, W);                                         \
}                                                                               \
                                                                                \
static const SwsOpEntry op_expand8_##X##Y##Z##W = {                             \
    .num_ops = 1,                                                               \
    .ops = (const SwsOp[]) {{                                                   \
        .type = SWS_PIXEL_U8,                                                   \
        .op   = SWS_OP_CONVERT,                                                 \
        .convert = {                                                            \
            .to = PIXEL_TYPE,                                                   \
            .expand = true,                                                     \
        },                                                                      \
        .comps.unused = { !X, !Y, !Z, !W },                                     \
    }},                                                                         \
    .op = expand8_##X##Y##Z##W,                                                 \
};

WRAP_EXPAND8(1, 0, 0, 0)
WRAP_EXPAND8(1, 0, 0, 1)
WRAP_EXPAND8(1, 1, 1, 0)
WRAP_EXPAND8(1, 1, 1, 1)

/* Fast path for fused 8 -> N bit expansion */
static av_always_inline void
upshift(tmp_t *restrict inout, const uint8_t amount,
        const bool x, const bool y, const bool z, const bool w)
{
    const tmp_t in = *inout;
    SWS_ASSUME(amount < BIT_DEPTH);

    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        if (x)
            inout->x.px[i] = in.x.u8[i] << amount;
        if (y)
            inout->y.px[i] = in.y.u8[i] << amount;
        if (z)
            inout->z.px[i] = in.z.u8[i] << amount;
        if (w)
            inout->w.px[i] = in.w.u8[i] << amount;
    }
}

#define WRAP_UPSHIFT8(N, X, Y, Z, W)                                            \
static SWS_FUNC void                                                            \
upshift##N##_##X##Y##Z##W(tmp_t *restrict inout, int y, const void *priv)       \
{                                                                               \
    upshift(inout, N, X, Y, Z, W);                                              \
}                                                                               \
                                                                                \
static const SwsOpEntry op_upshift##N##_##X##Y##Z##W = {                        \
    .num_ops = 2,                                                               \
    .ops = (const SwsOp[]) {{                                                   \
        .type = SWS_PIXEL_U8,                                                   \
        .op   = SWS_OP_CONVERT,                                                 \
        .convert.to = PIXEL_TYPE,                                               \
        .comps.unused = { !X, !Y, !Z, !W },                                     \
    }, {                                                                        \
        .type = PIXEL_TYPE,                                                     \
        .op   = SWS_OP_LSHIFT,                                                  \
        .shift.amount = N,                                                      \
        .comps.unused = { !X, !Y, !Z, !W },                                     \
    }},                                                                         \
    .op = upshift##N##_##X##Y##Z##W,                                            \
};

WRAP_UPSHIFT8(1, 1, 1, 1, 0)
WRAP_UPSHIFT8(2, 1, 1, 1, 0)
WRAP_UPSHIFT8(4, 1, 1, 1, 0)
WRAP_UPSHIFT8(6, 1, 1, 1, 0)
WRAP_UPSHIFT8(8, 1, 1, 1, 0)

static const SwsOpEntry int_tmpl_entries[] = {
    OPS_COMMON

    op_read_packed1,
    op_read_packed2,
    op_read_packed3,
    op_read_packed4,
    op_read_planar2,
    op_read_planar3,
    op_read_planar4,
    op_write_packed1,
    op_write_packed2,
    op_write_packed3,
    op_write_packed4,
    op_write_planar2,
    op_write_planar3,
    op_write_planar4,

#if BIT_DEPTH == 8
    op_read_nibbles1,
    op_read_bits1,
    op_write_nibbles1,
    op_write_bits1,
    op_unpack5650,
    op_unpack5550,
    op_unpack4440,
    op_unpack3320,
    op_unpack2330,
    op_unpack1210,
    op_pack5650,
    op_pack5550,
    op_pack4440,
    op_pack3320,
    op_pack2330,
    op_pack1210,
#elif BIT_DEPTH == 16
    op_unpack2101010,
    op_unpack1010102,
    op_pack2101010,
    op_pack1010102,
#endif
#ifdef SWAP_BYTES
    op_swap_bytes,
#endif

    op_swizzle_0123,
    op_swizzle_3012,
    op_swizzle_2103,
    op_swizzle_3210,
    op_swizzle_3102,
    op_swizzle_3201,
    op_swizzle_1203,
    op_swizzle_1023,
    op_swizzle_2013,
    op_swizzle_2310,
    op_swizzle_2130,
    op_swizzle_1230,
    op_swizzle_0213,
    op_swizzle_0231,
    op_swizzle_0312,
    op_swizzle_3120,
    op_swizzle_0321,
    op_swizzle_0003,
    op_swizzle_0000,
    op_swizzle_1000,
    op_swizzle_0001,

#if BIT_DEPTH != 8
    op_from8_1000,
    op_from8_1001,
    op_from8_1110,
    op_from8_1111,
    op_to8_1000,
    op_to8_1001,
    op_to8_1110,
    op_to8_1111,
    op_expand8_1111,

    op_lshift1_1110,
    op_lshift2_1110,
    op_lshift3_1110,
    op_lshift4_1110,
    op_lshift5_1110,
    op_lshift6_1110,
    op_lshift7_1110,
    op_lshift8_1110,
    op_rshift1_1110,
    op_rshift2_1110,
    op_rshift3_1110,
    op_rshift4_1110,
    op_rshift5_1110,
    op_rshift6_1110,
    op_rshift7_1110,
    op_rshift8_1110,
#endif

#if BIT_DEPTH != 16
    op_from16_1000,
    op_from16_1001,
    op_from16_1110,
    op_from16_1111,

    op_to16_1000,
    op_to16_1001,
    op_to16_1110,
    op_to16_1111,
#endif

#if BIT_DEPTH != 32
    op_from32_1000,
    op_from32_1001,
    op_from32_1110,
    op_from32_1111,

    op_to32_1000,
    op_to32_1001,
    op_to32_1110,
    op_to32_1111,
#endif

/* Optional fast paths */
#if !CONFIG_SMALL
    op_packed3_swizzle_2103,
    op_packed4_swizzle_3012,
    op_packed4_swizzle_1230,
    op_packed4_swizzle_2103,
    op_packed4_swizzle_3210,
    op_packed4_swizzle_1203,
    op_packed4_swizzle_2013,
    op_read_packed1f,
    op_read_packed2f,
    op_read_packed3f,
    op_read_packed4f,
    op_read_planar2f,
    op_read_planar3f,
    op_read_planar4f,
# ifdef SWAP_BYTES
    op_swap_packed1,
    op_swap_packed2,
    op_swap_packed3,
    op_swap_packed4,
    op_swap_planar1,
    op_swap_planar2,
    op_swap_planar3,
    op_swap_planar4,
# endif
# if BIT_DEPTH > 8
    op_upshift1_1110,
    op_upshift2_1110,
    op_upshift4_1110,
    op_upshift6_1110,
    op_upshift8_1110,
    op_expand8_1000,
    op_expand8_1001,
    op_expand8_1110,
# endif
#endif /* !CONFIG_SMALL */
};

const SwsOpTable bitfn(ff_sws_op_tmpl_int_table, BIT_DEPTH, SUFFIX) = {
    .cpu_flags   = CPU_FLAGS,
    .score       = SCORE,
    .entries     = int_tmpl_entries,
    .num_entries = FF_ARRAY_ELEMS(int_tmpl_entries),
};
