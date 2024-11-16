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

#include "libavutil/avassert.h"
#include "libavutil/csp.h"
#include "libavutil/slicethread.h"

#include "gamut_mapping.h"
#include "csp.h"

static bool cie_xy_equal(const AVCIExy *a, const AVCIExy *b)
{
    return !av_cmp_q(a->x, b->x) && !av_cmp_q(a->y, b->y);
}

static bool color_desc_equal(const AVColorPrimariesDesc *a, const AVColorPrimariesDesc *b)
{
    return cie_xy_equal(&a->wp,     &b->wp)     &&
           cie_xy_equal(&a->prim.r, &b->prim.r) &&
           cie_xy_equal(&a->prim.g, &b->prim.g) &&
           cie_xy_equal(&a->prim.b, &b->prim.b);
}

bool sws_gamut_map_equal(const SwsGamutMap *a, const SwsGamutMap *b)
{
    return color_desc_equal(&a->src, &b->src) &&
           color_desc_equal(&a->dst, &b->dst) &&
           a->min_luma  == b->min_luma &&
           a->max_luma  == b->max_luma &&
           a->intent    == b->intent &&
           a->eotf_inv  == b->eotf_inv;
}

bool sws_gamut_map_noop(const SwsGamutMap *par)
{
    switch (par->intent) {
    case SWS_INTENT_RELATIVE_COLORIMETRIC:
    case SWS_INTENT_ABSOLUTE_COLORIMETRIC:
        /* Clipping methods can only shrink the gamut */
        return ff_prim_superset(&par->dst.prim, &par->src.prim) &&
               cie_xy_equal(&par->src.wp, &par->dst.wp);
    case SWS_INTENT_SATURATION:
    case SWS_INTENT_PERCEPTUAL:
        /* Bidirectional/perceptual mapping methods can both shrink and expand */
        return ff_csp_desc_equal(&par->dst, &par->src);
    default:
        av_assert0(!"Invalid gamut mapping intent?");
        return true;
    }
}

/* Helper struct containing pre-computed cached values describing a gamut */
typedef struct Gamut {
    SwsMatrix3x3 lms2rgb;
    SwsMatrix3x3 rgb2lms;
    float min_nits, max_nits;
    float min_pq, max_pq;
    ICh peak;
} Gamut;

typedef struct GamutCtx {
    /* Loop body parameters */
    Gamut src;
    Gamut dst;
    /* Invocation parameters */
    SwsGamutMap params;
    v3u16_t *out;
    int size_I;
    int size_C;
    int slice_size;
} GamutCtx;

static inline v3u16_t output(GamutCtx ctx, RGB rgb)
{
    const float Lw   = ctx.dst.max_nits;
    const float Lb   = ctx.dst.min_nits;
    const AVColor c  = av_color(rgb.R, rgb.G, rgb.B);
    const AVColor cp = ctx.params.eotf_inv(Lw, Lb, c);
    return (v3u16_t) {
        av_clip_uint16(cp.c[0] * (UINT16_MAX - 1) + 0.5f),
        av_clip_uint16(cp.c[1] * (UINT16_MAX - 1) + 0.5f),
        av_clip_uint16(cp.c[2] * (UINT16_MAX - 1) + 0.5f),
    };
}

static inline bool ingamut(IPT c, Gamut gamut)
{
    const float min_rgb = gamut.min_nits - 1e-4f;
    const float max_rgb = gamut.max_nits + 1e-2f;
    const float Lp = c.I + 0.0975689f * c.P + 0.205226f * c.T;
    const float Mp = c.I - 0.1138760f * c.P + 0.133217f * c.T;
    const float Sp = c.I + 0.0326151f * c.P - 0.676887f * c.T;
    if (Lp < gamut.min_pq || Lp > gamut.max_pq ||
        Mp < gamut.min_pq || Mp > gamut.max_pq ||
        Sp < gamut.min_pq || Sp > gamut.max_pq)
    {
        /* Values outside legal LMS range */
        return false;
    } else {
        const float L = pq_eotf(Lp);
        const float M = pq_eotf(Mp);
        const float S = pq_eotf(Sp);
        RGB rgb = {
            .R = gamut.lms2rgb.m[0][0] * L +
                 gamut.lms2rgb.m[0][1] * M +
                 gamut.lms2rgb.m[0][2] * S,
            .G = gamut.lms2rgb.m[1][0] * L +
                 gamut.lms2rgb.m[1][1] * M +
                 gamut.lms2rgb.m[1][2] * S,
            .B = gamut.lms2rgb.m[2][0] * L +
                 gamut.lms2rgb.m[2][1] * M +
                 gamut.lms2rgb.m[2][2] * S,
        };
        return rgb.R >= min_rgb && rgb.R <= max_rgb &&
               rgb.G >= min_rgb && rgb.G <= max_rgb &&
               rgb.B >= min_rgb && rgb.B <= max_rgb;
    }
}

static const float maxDelta = 5e-5f;

// Find gamut intersection using specified bounds
static inline ICh
desat_bounded(float I, float h, float Cmin, float Cmax, Gamut gamut)
{
    if (I <= gamut.min_pq)
        return (ICh) { .I = gamut.min_pq, .C = 0, .h = h };
    else if (I >= gamut.max_pq)
        return (ICh) { .I = gamut.max_pq, .C = 0, .h = h };
    else {
        const float maxDI = I * maxDelta;
        ICh res = { .I = I, .C = (Cmin + Cmax) / 2, .h = h };
        do {
            if (ingamut(ich2ipt(res), gamut)) {
                Cmin = res.C;
            } else {
                Cmax = res.C;
            }
            res.C = (Cmin + Cmax) / 2;
        } while (Cmax - Cmin > maxDI);

        return res;
    }
}

// Finds maximally saturated in-gamut color (for given hue)
static inline ICh saturate(float hue, Gamut gamut)
{
    static const float invphi = 0.6180339887498948f;
    static const float invphi2 = 0.38196601125010515f;

    ICh lo = { .I = gamut.min_pq, .h = hue };
    ICh hi = { .I = gamut.max_pq, .h = hue };
    float de = hi.I - lo.I;
    ICh a = { .I = lo.I + invphi2 * de };
    ICh b = { .I = lo.I + invphi  * de };
    a = desat_bounded(a.I, hue, 0.0f, 0.5f, gamut);
    b = desat_bounded(b.I, hue, 0.0f, 0.5f, gamut);

    while (de > maxDelta) {
        de *= invphi;
        if (a.C > b.C) {
            hi = b;
            b = a;
            a.I = lo.I + invphi2 * de;
            a = desat_bounded(a.I, hue, lo.C - maxDelta, 0.5f, gamut);
        } else {
            lo = a;
            a = b;
            b.I = lo.I + invphi * de;
            b = desat_bounded(b.I, hue, hi.C - maxDelta, 0.5f, gamut);
        }
    }

    return a.C > b.C ? a : b;
}

static float softclip(float value, float source, float target)
{
    const float j = SOFTCLIP_KNEE;
    float peak, x, a, b, scale;
    if (!target)
        return 0.0f;

    peak = source / target;
    x = fminf(value / target, peak);
    if (x <= j || peak <= 1.0)
        return value;

    /* Apply simple mobius function */
    a = -j*j * (peak - 1.0f) / (j*j - 2.0f * j + peak);
    b = (j*j - 2.0f * j * peak + peak) / fmaxf(1e-6f, peak - 1.0f);
    scale = (b*b + 2.0f * b*j + j*j) / (b - a);

    return scale * (x + a) / (x + b) * target;
}

static IPT input(GamutCtx ctx, int Ix, float P, float T)
{
    const float Imin = ctx.src.min_pq, Imax = ctx.src.max_pq;
    const float Ixf  = (float) Ix / (ctx.size_I - 1);
    return (IPT) { fmixf(Imin, Imax, Ixf), P, T };
}

static void perceptual(GamutCtx ctx, v3u16_t *out, float P, float T)
{
    for (int Ix = 0; Ix < ctx.size_I; Ix++) {
        IPT ipt = input(ctx, Ix, P, T);
        ICh ich = ipt2ich(ipt);
        IPT mapped = rgb2ipt(ipt2rgb(ipt, &ctx.src.lms2rgb), &ctx.dst.rgb2lms);
        RGB rgb;
        float maxRGB;

        // Protect in gamut region
        const float maxC = fmaxf(ctx.src.peak.C, ctx.dst.peak.C);
        float k = smoothstepf(PERCEPTUAL_DEADZONE, 1.0f, ich.C / maxC);
        k *= PERCEPTUAL_STRENGTH;
        ipt.I = fmixf(ipt.I, mapped.I, k);
        ipt.P = fmixf(ipt.P, mapped.P, k);
        ipt.T = fmixf(ipt.T, mapped.T, k);

        rgb = ipt2rgb(ipt, &ctx.dst.lms2rgb);
        maxRGB = fmaxf(rgb.R, fmaxf(rgb.G, rgb.B));
        rgb.R = fmaxf(softclip(rgb.R, maxRGB, ctx.dst.max_nits), ctx.dst.min_nits);
        rgb.G = fmaxf(softclip(rgb.G, maxRGB, ctx.dst.max_nits), ctx.dst.min_nits);
        rgb.B = fmaxf(softclip(rgb.B, maxRGB, ctx.dst.max_nits), ctx.dst.min_nits);

        *out++ = output(ctx, rgb);
    }
}

/**
 * Something like fmixf(base, c, x) but follows an exponential curve, note
 * that this can be used to extend 'c' outwards for x > 1
 */
static inline ICh mix_exp(ICh c, float x, float gamma, float base)
{
    return (ICh) {
        .I = base + (c.I - base) * powf(x, gamma),
        .C = c.C * x,
        .h = c.h,
    };
}

/**
 * Drop gamma for colors approaching black and achromatic to avoid numerical
 * instabilities, and excessive brightness boosting of grain, while also
 * strongly boosting gamma for values exceeding the target peak
 */
static inline float scale_gamma(float gamma, ICh ich, Gamut gamut)
{
    const float Imin = gamut.min_pq;
    const float Irel = fmaxf((ich.I - Imin) / (gamut.peak.I - Imin), 0.0f);
    return gamma * powf(Irel, 3) * fminf(ich.C / gamut.peak.C, 1.0f);
}

/* Clip a color along the exponential curve given by `gamma` */
static inline IPT clip_gamma(IPT ipt, float gamma, Gamut gamut)
{
    float lo = 0.0f, hi = 1.0f, x = 0.5f;
    const float maxDI = fmaxf(ipt.I * maxDelta, 1e-7f);
    ICh ich;

    if (ipt.I <= gamut.min_pq)
        return (IPT) { .I = gamut.min_pq };
    if (ingamut(ipt, gamut))
        return ipt;

    ich = ipt2ich(ipt);
    if (!gamma)
        return ich2ipt(desat_bounded(ich.I, ich.h, 0.0f, ich.C, gamut));

    gamma = scale_gamma(gamma, ich, gamut);
    do {
        ICh test = mix_exp(ich, x, gamma, gamut.peak.I);
        if (ingamut(ich2ipt(test), gamut)) {
            lo = x;
        } else {
            hi = x;
        }
        x = (lo + hi) / 2.0f;
    } while (hi - lo > maxDI);

    return ich2ipt(mix_exp(ich, x, gamma, gamut.peak.I));
}


static void relative(GamutCtx ctx, v3u16_t *out, float P, float T)
{
    for (int Ix = 0; Ix < ctx.size_I; Ix++) {
        IPT ipt = input(ctx, Ix, P, T);
        ipt = clip_gamma(ipt, COLORIMETRIC_GAMMA, ctx.dst);
        *out++ = output(ctx, ipt2rgb(ipt, &ctx.dst.lms2rgb));
    }
}

static void absolute(GamutCtx ctx, v3u16_t *out, float P, float T)
{
    SwsMatrix3x3 m;

    /**
     * Note: This matrix maps from the *output* white point to the *input* wp,
     * because the IPT transform already implicitly includes a mapping from
     * the respective white point. As such, we need to actually reverse this
     * mapping to get the effects of an absolute transform.
     */
    m = ff_sws_get_adaptation_matrix(ctx.params.dst.wp, ctx.params.src.wp);

    for (int Ix = 0; Ix < ctx.size_I; Ix++) {
        IPT ipt = input(ctx, Ix, P, T);
        RGB rgb = ipt2rgb(ipt, &ctx.src.lms2rgb);
        float c[3] = { rgb.R, rgb.G, rgb.B };
        ff_sws_matrix3x3_apply(&m, c);
        rgb = (RGB) { c[0], c[1], c[2] };
        ipt = rgb2ipt(rgb, &ctx.dst.rgb2lms);
        ipt = clip_gamma(ipt, COLORIMETRIC_GAMMA, ctx.dst);
        *out++ = output(ctx, ipt2rgb(ipt, &ctx.dst.lms2rgb));
    }
}

static void saturation(GamutCtx ctx, v3u16_t *out, float P, float T)
{
    for (int Ix = 0; Ix < ctx.size_I; Ix++) {
        IPT ipt = input(ctx, Ix, P, T);
        RGB rgb = ipt2rgb(ipt, &ctx.src.lms2rgb);
        *out++ = output(ctx, rgb);
    }
}

static void noop(GamutCtx ctx, v3u16_t *out, float P, float T)
{
    for (int Ix = 0; Ix < ctx.size_I; Ix++) {
        IPT ipt = input(ctx, Ix, P, T);
        RGB rgb = ipt2rgb(ipt, &ctx.dst.lms2rgb);
        *out++ = output(ctx, rgb);
    }
}

static void generate_slice(void *priv, int jobnr, int threadnr, int nb_jobs,
                           int nb_threads)
{
    GamutCtx ctx = *(const GamutCtx *) priv;
    void (*generate)(GamutCtx ctx, v3u16_t *out, float P, float T) = NULL;

    const int Tstart = jobnr * ctx.slice_size;
    const int Tend   = FFMIN((jobnr + 1) * ctx.slice_size, ctx.size_C);
    v3u16_t *out = &ctx.out[Tstart * ctx.size_C * ctx.size_I];

    switch (ctx.params.intent) {
    case SWS_INTENT_PERCEPTUAL: generate = perceptual; break;
    case SWS_INTENT_SATURATION: generate = saturation; break;
    case SWS_INTENT_RELATIVE_COLORIMETRIC: generate = relative; break;
    case SWS_INTENT_ABSOLUTE_COLORIMETRIC: generate = absolute; break;
    }

    if (sws_gamut_map_noop(&ctx.params))
        generate = noop;

    for (int Tx = Tstart; Tx < Tend; Tx++) {
        const float T = (float) Tx / (ctx.size_C - 1) - 0.5f;
        for (int Px = 0; Px < ctx.size_C; Px++) {
            const float P = (float) Px / (ctx.size_C - 1) - 0.5f;
            if (generate != saturation && generate != noop) {
                /* Colorimetric methods need access to hue peaks, skip
                 * this for the trivial mappers as it is rather expensive
                 * to compute */
                const float hue = atan2f(T, P);
                ctx.src.peak = saturate(hue, ctx.src);
                ctx.dst.peak = saturate(hue, ctx.dst);
            }

            generate(ctx, out, P, T);
            out += ctx.size_I;
        }
    }
}

int sws_gamut_map_generate(v3u16_t *out, int size_I, int size_C,
                           const SwsGamutMap *params)
{
    AVSliceThread *slicethread;
    GamutCtx ctx;
    int ret, num_slices;

    ctx.params = *params;
    ctx.size_I = size_I;
    ctx.size_C = size_C;
    ctx.out    = out;

    ctx.src = ctx.dst = (Gamut) {
        .min_nits = params->min_luma,
        .max_nits = params->max_luma,
        .min_pq   = pq_oetf(params->min_luma),
        .max_pq   = pq_oetf(params->max_luma),
    };

    ctx.dst.rgb2lms = ff_sws_ipt_rgb2lms(&params->dst);
    ctx.dst.lms2rgb = ff_sws_ipt_lms2rgb(&params->dst);
    ctx.src.rgb2lms = ff_sws_ipt_rgb2lms(&params->src);
    ctx.src.lms2rgb = ff_sws_ipt_lms2rgb(&params->src);

    ret = avpriv_slicethread_create(&slicethread, &ctx, generate_slice, NULL, 0);
    if (ret < 0)
        return ret;

    ctx.slice_size = (size_C + ret - 1) / ret;
    num_slices = (size_C + ctx.slice_size - 1) / ctx.slice_size;
    avpriv_slicethread_execute(slicethread, num_slices, 0);
    avpriv_slicethread_free(&slicethread);
    return 0;
}
