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
DECL_ENTRY(convert_uint##N,                                                     \
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

DECL_FUNC(clear, const bool X, const bool Y, const bool Z, const bool W)
{
    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        if (!X)
            x[i] = impl->priv.px[0];
        if (!Y)
            y[i] = impl->priv.px[1];
        if (!Z)
            z[i] = impl->priv.px[2];
        if (!W)
            w[i] = impl->priv.px[3];
    }

    CONTINUE(pixel_t *, x, y, z, w);
}

#define WRAP_CLEAR(X, Y, Z, W)                                                  \
DECL_IMPL(clear##_##X##Y##Z##W)                                                 \
{                                                                               \
    CALL(clear, X, Y, Z, W);                                                    \
}                                                                               \
                                                                                \
DECL_ENTRY_EX(clear##_##X##Y##Z##W,                                             \
    .setup = ff_sws_setup_q4,                                                   \
    .flexible = true,                                                           \
    .op.op = SWS_OP_CLEAR,                                                      \
    .op.comps.unused = { !X, !Y, !Z, !W },                                      \
);

WRAP_CLEAR(1, 1, 1, 0) /* rgba alpha */
WRAP_CLEAR(0, 1, 1, 1) /* argb alpha */

WRAP_CLEAR(0, 0, 1, 1) /* vuya chroma */
WRAP_CLEAR(1, 0, 0, 1) /* yuva chroma */
WRAP_CLEAR(1, 1, 0, 0) /* ayuv chroma */
WRAP_CLEAR(0, 1, 0, 1) /* uyva chroma */
WRAP_CLEAR(1, 0, 1, 0) /* xvyu chroma */

WRAP_CLEAR(1, 0, 0, 0) /* gray -> yuva */
WRAP_CLEAR(0, 1, 0, 0) /* gray -> ayuv */
WRAP_CLEAR(0, 0, 1, 0) /* gray -> vuya */

DECL_IMPL(min)
{
    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        x[i] = FFMIN(x[i], impl->priv.px[0]);
        y[i] = FFMIN(y[i], impl->priv.px[1]);
        z[i] = FFMIN(z[i], impl->priv.px[2]);
        w[i] = FFMIN(w[i], impl->priv.px[3]);
    }

    CONTINUE(pixel_t *, x, y, z, w);
}

DECL_IMPL(max)
{
    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        x[i] = FFMAX(x[i], impl->priv.px[0]);
        y[i] = FFMAX(y[i], impl->priv.px[1]);
        z[i] = FFMAX(z[i], impl->priv.px[2]);
        w[i] = FFMAX(w[i], impl->priv.px[3]);
    }

    CONTINUE(pixel_t *, x, y, z, w);
}

DECL_ENTRY_EX(min, .op.op = SWS_OP_MIN, .setup = ff_sws_setup_q4, .flexible = true);
DECL_ENTRY_EX(max, .op.op = SWS_OP_MAX, .setup = ff_sws_setup_q4, .flexible = true);

DECL_IMPL(scale)
{
    const pixel_t scale = impl->priv.px[0];

    SWS_LOOP
    for (int i = 0; i < SWS_CHUNK_SIZE; i++) {
        x[i] *= scale;
        y[i] *= scale;
        z[i] *= scale;
        w[i] *= scale;
    }

    CONTINUE(pixel_t *, x, y, z, w);
}

DECL_ENTRY_EX(scale, .op.op = SWS_OP_SCALE, .setup = ff_sws_setup_q, .flexible = true);
