/*
 * Copyright (C) 2024 Niklas Haas
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

#ifndef SWSCALE_CMS_H
#define SWSCALE_CMS_H

#include <stdint.h>

#include "libavutil/csp.h"
#include "libavutil/pixfmt.h"

#include "csp.h"
#include "gamut_mapping.h"
#include "utils.h"

enum {
    /* Input LUT size. This is only calculated once. */
    INPUT_LUT_BITS = 4,
    INPUT_LUT_SIZE = (1 << INPUT_LUT_BITS) + 1,

    /* Tone mapping LUT size. This is regenerated possibly per frame. */
    TONE_LUT_BITS = 8,
    TONE_LUT_SIZE = 1 << TONE_LUT_BITS,

    /* Gamut mapping / output LUT size. This is only calculated once. */
    GAMUT_LUT_BITS_I = 6,
    GAMUT_LUT_BITS_C = 7,

    GAMUT_LUT_SIZE_I = (1 << GAMUT_LUT_BITS_I) + 1,
    GAMUT_LUT_SIZE_C = (1 << GAMUT_LUT_BITS_C) + 1,
};

typedef struct SwsCms {
    /* Static parameters, call sws_lut3d_update() after changing */
    SwsFormat src, dst; /* Only color metadata and pixfmt are used */
    SwsIntent intent;

    /* Input 3DLUT (RGB -> IPT) */
    v3u16_t input_lut[INPUT_LUT_SIZE][INPUT_LUT_SIZE][INPUT_LUT_SIZE];

    /* Tone mapping LUT (I -> I) */
    uint16_t tone_lut[TONE_LUT_SIZE];

    /* Gamut mapping / output 3DLUT (IPT -> RGB) */
    v3u16_t gamut_lut[GAMUT_LUT_SIZE_C][GAMUT_LUT_SIZE_C][GAMUT_LUT_SIZE_I];
    SwsGamutMap gamut_map;
} SwsCms;

SwsCms *sws_cms_alloc(void);
void sws_cms_free(SwsCms **cms);

/**
 * Test to see if a given format is supported by the CMS input/output code.
 */
bool sws_cms_test_fmt(SwsFormat fmt, int output);

/**
 * Pick the best compatible pixfmt for a given SwsFormat.
 */
enum AVPixelFormat sws_cms_pick_pixfmt(SwsFormat fmt, int output);

/**
 * Recalculate the CMS state with new settings.
 *
 * Returns a negative error code, 1 if the CMS was successfully updated, or 0
 * if generation was skipped because the resulting CMS would be a no-op. In
 * the last case, the user may not call sws_cms_apply*().
 */
int sws_cms_update(SwsCms *cms);

/**
 * Applies a color transformation to a plane. The format must match the format
 * used during sws_lut3d_update().
 */
void sws_cms_apply(const SwsCms *cms, const uint8_t *in, int in_stride,
                   uint8_t *out, int out_stride, int w, int h);

#endif /* SWSCALE_CMS_H */
