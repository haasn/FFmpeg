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

#include <math.h>
#include <string.h>

#include "libavutil/attributes.h"
#include "libavutil/avassert.h"
#include "libavutil/common.h"
#include "libavutil/csp.h"
#include "libavutil/mem.h"

#include "cms.h"
#include "csp.h"

SwsCms *sws_cms_alloc(void)
{
    SwsCms *cms = av_malloc(sizeof(*cms));
    if (!cms)
        return NULL;

    memset(&cms->src, 0, sizeof(cms->src));
    memset(&cms->dst, 0, sizeof(cms->dst));
    memset(&cms->gamut_map, 0, sizeof(cms->gamut_map));
    cms->intent = SWS_INTENT_RELATIVE_COLORIMETRIC;
    return cms;
}

void sws_cms_free(SwsCms **pcms)
{
    av_freep(pcms);
}

bool sws_cms_test_fmt(SwsFormat fmt, int output)
{
    return fmt.format == AV_PIX_FMT_RGBA64;
}

enum AVPixelFormat sws_cms_pick_pixfmt(SwsFormat fmt, int output)
{
    return AV_PIX_FMT_RGBA64;
}

static av_always_inline v3u16_t lerp(v3u16_t a, v3u16_t b, int x, int shift)
{
    const int xi = (1 << shift) - x;
    return (v3u16_t) {
        (a.x * xi + b.x * x) >> shift,
        (a.y * xi + b.y * x) >> shift,
        (a.z * xi + b.z * x) >> shift,
    };
}

/**
 * v0 and v1 are 'black' and 'white'
 * v1 and v2 are closest RGB/CMY vertices
 * x >= y >= z are relative weights
 */
static av_always_inline
v3u16_t barycentric(int shift, int x, int y, int z,
                    v3u16_t v0, v3u16_t v1, v3u16_t v2, v3u16_t v3)
{
    const int a = (1 << shift) - x;
    const int b = x - y;
    const int c = y - z;
    const int d = z;
    av_assert2(x >= y);
    av_assert2(y >= z);

    return (v3u16_t) {
        (a * v0.x + b * v1.x + c * v2.x + d * v3.x) >> shift,
        (a * v0.y + b * v1.y + c * v2.y + d * v3.y) >> shift,
        (a * v0.z + b * v1.z + c * v2.z + d * v3.z) >> shift,
    };
}

static av_always_inline v3u16_t lookup_input16(const SwsCms *cms, v3u16_t rgb)
{
    const int shift = 16 - INPUT_LUT_BITS;
    const int Rx = rgb.x >> shift;
    const int Gx = rgb.y >> shift;
    const int Bx = rgb.z >> shift;
    const int Rf = rgb.x & ((1 << shift) - 1);
    const int Gf = rgb.y & ((1 << shift) - 1);
    const int Bf = rgb.z & ((1 << shift) - 1);
    const int Rn = FFMIN(Rx + 1, INPUT_LUT_SIZE - 1);
    const int Gn = FFMIN(Gx + 1, INPUT_LUT_SIZE - 1);
    const int Bn = FFMIN(Bx + 1, INPUT_LUT_SIZE - 1);

    /* Tetrahedral interpolation */
    const v3u16_t c000 = cms->input_lut[Bx][Gx][Rx];
    const v3u16_t c111 = cms->input_lut[Bn][Gn][Rn];
    if (Rf > Gf) {
        if (Gf > Bf) {
            const v3u16_t c100 = cms->input_lut[Bx][Gx][Rn];
            const v3u16_t c110 = cms->input_lut[Bx][Gn][Rn];
            return barycentric(shift, Rf, Gf, Bf, c000, c100, c110, c111);
        } else if (Rf > Bf) {
            const v3u16_t c100 = cms->input_lut[Bx][Gx][Rn];
            const v3u16_t c101 = cms->input_lut[Bn][Gx][Rn];
            return barycentric(shift, Rf, Bf, Gf, c000, c100, c101, c111);
        } else {
            const v3u16_t c001 = cms->input_lut[Bn][Gx][Rx];
            const v3u16_t c101 = cms->input_lut[Bn][Gx][Rn];
            return barycentric(shift, Bf, Rf, Gf, c000, c001, c101, c111);
        }
    } else {
        if (Bf > Gf) {
            const v3u16_t c001 = cms->input_lut[Bn][Gx][Rx];
            const v3u16_t c011 = cms->input_lut[Bn][Gn][Rx];
            return barycentric(shift, Bf, Gf, Rf, c000, c001, c011, c111);
        } else if (Bf > Rf) {
            const v3u16_t c010 = cms->input_lut[Bx][Gn][Rx];
            const v3u16_t c011 = cms->input_lut[Bn][Gn][Rx];
            return barycentric(shift, Gf, Bf, Rf, c000, c010, c011, c111);
        } else {
            const v3u16_t c010 = cms->input_lut[Bx][Gn][Rx];
            const v3u16_t c110 = cms->input_lut[Bx][Gn][Rn];
            return barycentric(shift, Gf, Rf, Bf, c000, c010, c110, c111);
        }
    }
}

static av_always_inline v3u16_t lookup_output(const SwsCms *cms, v3u16_t ich)
{
    const int Ishift = 16 - GAMUT_LUT_BITS_I;
    const int Cshift = 16 - GAMUT_LUT_BITS_C;
    const int Ix = ich.x >> Ishift;
    const int Px = ich.y >> Cshift;
    const int Tx = ich.z >> Cshift;
    const int If = ich.x & ((1 << Ishift) - 1);
    const int Pf = ich.y & ((1 << Cshift) - 1);
    const int Tf = ich.z & ((1 << Cshift) - 1);
    const int In = FFMIN(Ix + 1, GAMUT_LUT_SIZE_I - 1);
    const int Pn = FFMIN(Px + 1, GAMUT_LUT_SIZE_C - 1);
    const int Tn = FFMIN(Tx + 1, GAMUT_LUT_SIZE_C - 1);

    /* Trilinear interpolation */
    const v3u16_t c000 = cms->gamut_lut[Tx][Px][Ix];
    const v3u16_t c001 = cms->gamut_lut[Tx][Px][In];
    const v3u16_t c010 = cms->gamut_lut[Tx][Pn][Ix];
    const v3u16_t c011 = cms->gamut_lut[Tx][Pn][In];
    const v3u16_t c100 = cms->gamut_lut[Tn][Px][Ix];
    const v3u16_t c101 = cms->gamut_lut[Tn][Px][In];
    const v3u16_t c110 = cms->gamut_lut[Tn][Pn][Ix];
    const v3u16_t c111 = cms->gamut_lut[Tn][Pn][In];
    const v3u16_t c00  = lerp(c000, c100, Tf, Cshift);
    const v3u16_t c10  = lerp(c010, c110, Tf, Cshift);
    const v3u16_t c01  = lerp(c001, c101, Tf, Cshift);
    const v3u16_t c11  = lerp(c011, c111, Tf, Cshift);
    const v3u16_t c0   = lerp(c00,  c10,  Pf, Cshift);
    const v3u16_t c1   = lerp(c01,  c11,  Pf, Cshift);
    const v3u16_t c    = lerp(c0,   c1,   If, Ishift);
    return c;
}

/* Approximation of gamut hull at a given intensity level */
static const float hull(float I)
{
    return ((I - 6.0f) * I + 9.0f) * I;
}

int sws_cms_update(SwsCms *cms)
{
    av_csp_eotf_function eotf;
    SwsMatrix3x3 rgb2lms;
    int noop, ret;

    /* TODO */
    const float src_nits = 10000;
    const float dst_nits = 203;
    const float src_pq   = pq_oetf(src_nits);
    const float dst_pq   = pq_oetf(dst_nits);

    const SwsGamutMap gamut_map = {
        .src      = *av_csp_primaries_desc_from_id(cms->src.prim),
        .dst      = *av_csp_primaries_desc_from_id(cms->dst.prim),
        .eotf_inv = av_csp_itu_eotf_inv(cms->dst.trc),
        .intent   = cms->intent,
        .max_luma = dst_nits,
        .min_luma = 0.0f,
    };

    /* TODO: add proper tone mapping, for now just naively convert/clip */
    noop = cms->src.trc == cms->dst.trc;

    /* Saturation gamut mapping is always a no-op, so we can safely skip it */
    noop &= cms->src.prim == cms->dst.prim || cms->intent == SWS_INTENT_SATURATION;
    if (noop)
        return 0;

    if (!sws_cms_test_fmt(cms->src, 0) || !sws_cms_test_fmt(cms->dst, 1))
        return AVERROR(EINVAL);

    if (!sws_gamut_map_equal(&gamut_map, &cms->gamut_map)) {
        /* Update gamut mapping 3DLUT */
        ret = sws_gamut_map_generate(&cms->gamut_lut[0][0][0], GAMUT_LUT_SIZE_I,
                                     GAMUT_LUT_SIZE_C, &gamut_map);
        if (ret < 0)
            return ret;
        cms->gamut_map = gamut_map;
    }

    /* Generate input LUT */
    eotf    = av_csp_itu_eotf(cms->src.trc);
    rgb2lms = ff_sws_ipt_rgb2lms(&gamut_map.src);

    for (int b = 0; b < INPUT_LUT_SIZE; b++) {
        for (int g = 0; g < INPUT_LUT_SIZE; g++) {
            for (int r = 0; r < INPUT_LUT_SIZE; r++) {
                const AVColor cp = {{
                    (double) r / (INPUT_LUT_SIZE - 1),
                    (double) g / (INPUT_LUT_SIZE - 1),
                    (double) b / (INPUT_LUT_SIZE - 1),
                }};

                AVColor c = eotf(src_nits, 0, cp);
                RGB rgb = { c.c[0], c.c[1], c.c[2] };
                IPT ipt = rgb2ipt(rgb, &rgb2lms);

                /* FIXME: implement proper tone mapping */
                const float Inew = ipt.I / src_pq * dst_pq;
                const float desat = fminf(ipt.I / Inew, hull(Inew) / hull(ipt.I));
                ipt.I = Inew;
                ipt.P = desat * ipt.P;
                ipt.T = desat * ipt.T;

                /* rescale to [0, 1]^3 relative to source range */
                const float Ix = ipt.I / dst_pq; // FIXME: separate TM step
                const float Px = ipt.P + 0.5f;
                const float Tx = ipt.T + 0.5f;

                cms->input_lut[b][g][r] = (v3u16_t) {
                    av_clip_uint16(Ix * (UINT16_MAX - 1) + 0.5f),
                    av_clip_uint16(Px * (UINT16_MAX - 1) + 0.5f),
                    av_clip_uint16(Tx * (UINT16_MAX - 1) + 0.5f),
                };
            }
        }
    }

    return 1;
}

void sws_cms_apply(const SwsCms *cms, const uint8_t *in, int in_stride,
                   uint8_t *out, int out_stride, int w, int h)
{
    while (h--) {
        const uint16_t *in16 = (const uint16_t *) in;
        uint16_t *out16 = (uint16_t *) out;

        for (int x = 0; x < w; x++) {
            v3u16_t rgb = { in16[0], in16[1], in16[2] };
            v3u16_t ipt = lookup_input16(cms, rgb);
            rgb = lookup_output(cms, ipt);
            out16[0] = rgb.x;
            out16[1] = rgb.y;
            out16[2] = rgb.z;
            out16[3] = in16[3];
            in16  += 4;
            out16 += 4;
        }

        in  += in_stride;
        out += out_stride;
    }
}
