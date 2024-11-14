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

#ifndef SWSCALE_CSP_H
#define SWSCALE_CSP_H

#include <stdint.h>
#include <stdbool.h>
#include <math.h>

#include "libavutil/attributes.h"
#include "libavutil/common.h"
#include "libavutil/csp.h"
#include "libavutil/pixfmt.h"

/* Shared constants and helpers for colorspace mapping */

#define fmixf(a, b, x) ((b) * (x) + (a) * (1 - (x)))

static inline float smoothstepf(float edge0, float edge1, float x)
{
    if (edge0 == edge1)
        return x >= edge0;
    x = (x - edge0) / (edge1 - edge0);
    x = av_clipf(x, 0.0f, 1.0f);
    return x * x * (3.0f - 2.0f * x);
}

/* 3x3 matrix math */
typedef struct SwsMatrix3x3 {
    float m[3][3];
} SwsMatrix3x3;

void ff_sws_matrix3x3_mul(SwsMatrix3x3 *a, const SwsMatrix3x3 *b);
void ff_sws_matrix3x3_invert(SwsMatrix3x3 *mat);
void ff_sws_matrix3x3_apply(const SwsMatrix3x3 *mat, float vec[3]);

SwsMatrix3x3 ff_sws_get_adaptation_matrix(AVCIExy from, AVCIExy to);
SwsMatrix3x3 ff_sws_ipt_rgb2lms(const AVColorPrimariesDesc *prim);
SwsMatrix3x3 ff_sws_ipt_lms2rgb(const AVColorPrimariesDesc *prim);
SwsMatrix3x3 ff_sws_rgb2xyz(const AVColorPrimariesDesc *prim);
SwsMatrix3x3 ff_sws_xyz2rgb(const AVColorPrimariesDesc *prim);

/* Integer math definitions / helpers */
typedef struct v3u8_t {
    uint8_t x, y, z;
} v3u8_t;

typedef struct v3u16_t {
    uint16_t x, y, z;
} v3u16_t;

/* Fast perceptual quantizer */
static const float PQ_M1 = 2610./4096 * 1./4,
                   PQ_M2 = 2523./4096 * 128,
                   PQ_C1 = 3424./4096,
                   PQ_C2 = 2413./4096 * 32,
                   PQ_C3 = 2392./4096 * 32;

enum { PQ_LUT_SIZE = 1024 };
extern const float pq_eotf_lut[PQ_LUT_SIZE+1];

static inline float pq_eotf(float x)
{
    float idxf  = av_clipf(x, 0.0f, 1.0f) * (PQ_LUT_SIZE - 1);
    int ipart   = floorf(idxf);
    float fpart = idxf - ipart;
    return fmixf(pq_eotf_lut[ipart], pq_eotf_lut[ipart + 1], fpart);
}

static inline float pq_oetf(float x)
{
    x = powf(fmaxf(x * 1e-4f, 0.0f), PQ_M1);
    x = (PQ_C1 + PQ_C2 * x) / (1.0f + PQ_C3 * x);
    return powf(x, PQ_M2);
}

/* For some minimal type safety, and code cleanliness */
typedef struct RGB {
    float R, G, B; /* nits */
} RGB;

typedef struct IPT {
    float I, P, T;
} IPT;

typedef struct ICh {
    float I, C, h;
} ICh;

static av_always_inline ICh ipt2ich(IPT c)
{
    return (ICh) {
        .I = c.I,
        .C = sqrtf(c.P * c.P + c.T * c.T),
        .h = atan2f(c.T, c.P),
    };
}

static av_always_inline IPT ich2ipt(ICh c)
{
    return (IPT) {
        .I = c.I,
        .P = c.C * cosf(c.h),
        .T = c.C * sinf(c.h),
    };
}

static av_always_inline IPT rgb2ipt(RGB c, const SwsMatrix3x3 *rgb2lms)
{
    const float L = rgb2lms->m[0][0] * c.R +
                    rgb2lms->m[0][1] * c.G +
                    rgb2lms->m[0][2] * c.B;
    const float M = rgb2lms->m[1][0] * c.R +
                    rgb2lms->m[1][1] * c.G +
                    rgb2lms->m[1][2] * c.B;
    const float S = rgb2lms->m[2][0] * c.R +
                    rgb2lms->m[2][1] * c.G +
                    rgb2lms->m[2][2] * c.B;
    const float Lp = pq_oetf(L);
    const float Mp = pq_oetf(M);
    const float Sp = pq_oetf(S);
    return (IPT) {
        .I = 0.4000f * Lp + 0.4000f * Mp + 0.2000f * Sp,
        .P = 4.4550f * Lp - 4.8510f * Mp + 0.3960f * Sp,
        .T = 0.8056f * Lp + 0.3572f * Mp - 1.1628f * Sp,
    };
}

static av_always_inline RGB ipt2rgb(IPT c, const SwsMatrix3x3 *lms2rgb)
{
    const float Lp = c.I + 0.0975689f * c.P + 0.205226f * c.T;
    const float Mp = c.I - 0.1138760f * c.P + 0.133217f * c.T;
    const float Sp = c.I + 0.0326151f * c.P - 0.676887f * c.T;
    const float L = pq_eotf(Lp);
    const float M = pq_eotf(Mp);
    const float S = pq_eotf(Sp);
    return (RGB) {
        .R = lms2rgb->m[0][0] * L +
             lms2rgb->m[0][1] * M +
             lms2rgb->m[0][2] * S,
        .G = lms2rgb->m[1][0] * L +
             lms2rgb->m[1][1] * M +
             lms2rgb->m[1][2] * S,
        .B = lms2rgb->m[2][0] * L +
             lms2rgb->m[2][1] * M +
             lms2rgb->m[2][2] * S,
    };
}

/* Misc colorspace math / helpers */

bool ff_csp_desc_equal(const AVColorPrimariesDesc *a, const AVColorPrimariesDesc *b);

/**
 * Returns true if 'b' is entirely contained in 'a'. Useful for figuring out if
 * colorimetric clipping will occur or not.
 */
bool ff_prim_superset(const AVPrimaryCoefficients *a, const AVPrimaryCoefficients *b);

#endif /* SWSCALE_CSP_H */
