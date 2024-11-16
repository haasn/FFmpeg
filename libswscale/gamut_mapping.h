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

#ifndef SWSCALE_GAMUT_MAPPING_H
#define SWSCALE_GAMUT_MAPPING_H

#include <stdbool.h>

#include "libavutil/csp.h"

#include "csp.h"

/* (Relative) chromaticity protection zone for perceptual mapping [0,1] */
#define PERCEPTUAL_DEADZONE 0.30f

/* Strength of the perceptual saturation mapping component [0,1] */
#define PERCEPTUAL_STRENGTH 0.80f

/* Knee point to use for perceptual soft clipping [0,1] */
#define SOFTCLIP_KNEE 0.70f

/* I vs C curve gamma to use for colorimetric clipping [0,10] */
#define COLORIMETRIC_GAMMA 1.80f

typedef enum SwsIntent {
    SWS_INTENT_PERCEPTUAL = 0,
    SWS_INTENT_RELATIVE_COLORIMETRIC = 1,
    SWS_INTENT_SATURATION = 2,
    SWS_INTENT_ABSOLUTE_COLORIMETRIC = 3,
} SwsIntent;

typedef struct SwsGamutMap {
    /**
     * The desired input/output primaries. This affects the subjective color
     * volume in which the desired mapping shall take place.
     */
    AVColorPrimariesDesc src;
    AVColorPrimariesDesc dst;

    /**
     * Minimum/maximum luminance (nits) of the target display. Note that the
     * same value applies to both the input and output, since it's assumed that
     * tone mapping has already happened by this stage. This effectively defines
     * the legal gamut boundary in RGB space.
     */
    float min_luma;
    float max_luma;

    /**
     * Output transfer characteristic of the target display. Will be applied to
     * the output colors before storing them to the 3DLUT.
     */
    av_csp_eotf_function eotf_inv;

    /* Desired ICC rendering intent */
    SwsIntent intent;
} SwsGamutMap;

bool sws_gamut_map_equal(const SwsGamutMap *a, const SwsGamutMap *b);

/**
 * Returns true if the given gamut mapping configuration effectively represents
 * a no-op configuration. Gamut mapping can be skipped in this case.
 */
bool sws_gamut_map_noop(const SwsGamutMap *params);

/**
 * Generate a gamut-mapping LUT for a given configuration. LUT samples are
 * stored as RGB values in the configured output space, but the LUT itself is
 * indexed by IPTPQc4, spanning the range [min_pq, max_pq] × [-0.5, 0.5]².
 *
 * Returns 0 or a negative error code.
 */
int sws_gamut_map_generate(v3u16_t *lut, int size_I, int size_C,
                           const SwsGamutMap *params);

#endif // SWSCALE_GAMUT_MAPPING_H
