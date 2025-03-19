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

#include "ops_backend.h"

#ifndef BIT_DEPTH
#  error Should only be included from ops_tmpl_*.c!
#endif

#define WRAP_CONVERT_UINT(N)                                                    \
DECL_IMPL(convert_uint##N)                                                      \
{                                                                               \
    uint##N##_t xx[SWS_CHUNK_SIZE], yy[SWS_CHUNK_SIZE],                         \
                zz[SWS_CHUNK_SIZE], ww[SWS_CHUNK_SIZE];                         \
                                                                                \
    SWS_LOOP                                                                    \
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {                                  \
        xx[i] = x[i];                                                           \
        yy[i] = y[i];                                                           \
        zz[i] = z[i];                                                           \
        ww[i] = w[i];                                                           \
    }                                                                           \
                                                                                \
    CONTINUE(uint##N##_t *, xx, yy, zz, ww);                                    \
}                                                                               \
                                                                                \
DECL_ENTRY_SIMPLE(convert_uint##N,                                              \
    .op = SWS_OP_CONVERT,                                                       \
    .convert.to = SWS_PIXEL_U##N,                                               \
);

#if BIT_DEPTH != 8
WRAP_CONVERT_UINT(8)
#endif

#if BIT_DEPTH != 16
WRAP_CONVERT_UINT(16)
#endif

#if BIT_DEPTH != 32 || defined(IS_FLOAT)
WRAP_CONVERT_UINT(32)
#endif

typedef struct {
    pixel_t x, y, z, w;
} fn(ClearCoefs);

DECL_SETUP(clear)
{
    fn(ClearCoefs) c = {
        .x = av_q2pixel(op->clear.value[0]),
        .y = av_q2pixel(op->clear.value[1]),
        .z = av_q2pixel(op->clear.value[2]),
        .w = av_q2pixel(op->clear.value[3]),
    };

    return SETUP_MEMDUP(c);
}

DECL_FUNC_PATTERN(clear)
{
    const fn(ClearCoefs) *restrict c = impl->priv;

    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        if (!X)
            x[i] = c->x;
        if (!Y)
            y[i] = c->y;
        if (!Z)
            z[i] = c->z;
        if (!W)
            w[i] = c->w;
    }

    CONTINUE(pixel_t *, x, y, z, w);
}

#define WRAP_CLEAR_PATTERN(X, Y, Z, W)                                          \
WRAP_PATTERN(clear, X, Y, Z, W,                                                 \
    .op.op = SWS_OP_CLEAR,                                                      \
    .setup = fn(setup_clear),                                                   \
    .free  = av_free,                                                           \
);

WRAP_CLEAR_PATTERN(1, 1, 1, 0) /* rgba alpha */
WRAP_CLEAR_PATTERN(0, 1, 1, 1) /* argb alpha */

WRAP_CLEAR_PATTERN(0, 0, 1, 1); /* vuya chroma */
WRAP_CLEAR_PATTERN(1, 0, 0, 1); /* yuva chroma */
WRAP_CLEAR_PATTERN(1, 1, 0, 0); /* ayuv chroma */
WRAP_CLEAR_PATTERN(0, 1, 0, 1); /* uyva chroma */
WRAP_CLEAR_PATTERN(1, 0, 1, 0); /* xvyu chroma */

WRAP_CLEAR_PATTERN(1, 0, 0, 0) /* gray -> yuva */
WRAP_CLEAR_PATTERN(0, 1, 0, 0) /* gray -> ayuv */
WRAP_CLEAR_PATTERN(0, 0, 1, 0) /* gray -> vuya */

static_assert(sizeof(pixel_t) <= sizeof(uintptr_t), "scale coef too large");
DECL_SETUP(scale)
{
    union {
        const void *ptr;
        pixel_t scale;
    } c = { .scale = av_q2pixel(op->scale.factor) };

    *out_priv = c.ptr;
    return 0;
}

DECL_IMPL(scale)
{
    union {
        const void *ptr;
        pixel_t scale;
    } c = { .ptr = impl->priv };

    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        x[i] *= c.scale;
        y[i] *= c.scale;
        z[i] *= c.scale;
        w[i] *= c.scale;
    }

    CONTINUE(pixel_t *, x, y, z, w);
}

DECL_ENTRY(scale,
    .op.op = SWS_OP_SCALE,
    .setup = fn(setup_scale),
);
