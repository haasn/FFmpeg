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

#ifndef SWSCALE_AVSCALE_H
#define SWSCALE_AVSCALE_H

/**
 * @file
 * @ingroup libsws
 * Higher-level wrapper around libswscale + related libraries, which is
 * capable of handling more advanced colorspace transformations.
 */

#include "libavutil/frame.h"
#include "libavutil/log.h"

/**
 * Main external API structure. New fields cannot be added to the end with
 * minor version bumps. Removal, reordering and changes to existing fields
 * require a major version bump. sizeof(AVScaleContext) is not part of the ABI.
 */
typedef struct AVScaleContext {
    const AVClass *av_class;

    /**
     * Private context used for internal data.
     */
    struct AVScaleInternal *internal;

    /**
     * Private data of the user, can be used to carry app specific stuff.
     */
    void *opaque;

    /**
     * Bitmask of AV_SCALE_* flags.
     */
    int64_t flags;

    /**
     * How many threads to use for processing, or 0 for automatic selection.
     */
    int threads;

    /**
     * Quality preset, on a scale from 1 to 10, or 0 to disable in favor of
     * fixed, unchanging dafaults. See `enum AVScaleQuality`.
     */
    int preset;

    /**
     * Dither mode. If set to something other than AV_DITHER_AUTO, this will
     * override the dither mode implied by the `preset`.
     */
    int dither;

    /**
     * Scaling filter. If set to something other than AV_SCALE_AUTO, this will
     * override the filter implied by the `preset`.
     */
    int filter;

    /**
     * Filter used specifically for up/downsampling subsampled (chroma) planes.
     * If set to something other than AV_SCALE_AUTO, this will override the
     * filter implied by the `preset`.
     */
    int filter_sub;

    /**
     * Backwards compatibility field with libswscale. Anything set here
     * will override the corresponding options implied by the fields above.
     *
     * @deprecated use AVScaleContext.flags/filter/dither
     */
    attribute_deprecated
    int sws_flags;
} AVScaleContext;

/**
 * Allocate an AVScaleContext and set its fields to default values. The
 * resulting struct should be freed with avscale_free_context().
 */
AVScaleContext *avscale_alloc_context(void);

/**
 * Free the codec context and everything associated with it, and write NULL
 * to the provided pointer.
 */
void avscale_free_context(AVScaleContext **ctx);

/**
 * Get the AVClass for AVScaleContext. It can be used in combination with
 * AV_OPT_SEARCH_FAKE_OBJ for examining options.
 *
 * @see av_opt_find().
 */
const AVClass *avscale_get_class(void);

/******************************
 * Flags and quality settings *
 ******************************/

enum AVScaleFlags {
    /**
    * Force bit-exact output. This will prevent the use of platform-specific
    * optimizations that may lead to slight difference in rounding, in favor
    * of always maintaining exact bit output compatibility with the reference
    * C code.
    */
    AV_SCALE_BITEXACT = 1 << 0,

    /**
    * Return an error on underspecified conversions. Without this flag,
    * unspecified fields are defaulted to sensible values.
    */
    AV_SCALE_STRICT = 1 << 1,
};

/**
 * The exact interpretation of these quality presets are not part of the ABI,
 * except for AV_SCALE_NONE, which is guaranteed to remain stable across
 * versions (for constistent output).
 */
enum AVScaleQuality {
    AV_SCALE_NONE      = 0,  /* currently equivalent to AV_SCALE_MEDIUM */
    AV_SCALE_ULTRAFAST = 1,  /* no dither,      nearest+nearest         */
    AV_SCALE_SUPERFAST = 2,  /* no dither,      bilinear+nearest        */
    AV_SCALE_VERYFAST  = 3,  /* no dither,      bilinear+bilinear       */
    AV_SCALE_FASTER    = 4,  /* bayer dither,   bilinear+bilinear       */
    AV_SCALE_FAST      = 5,  /* bayer dither,   bicubic+bilinear        */
    AV_SCALE_MEDIUM    = 6,  /* bayer dither,   bicubic+bicubic         */
    AV_SCALE_SLOW      = 7,  /* bayer dither,   lanczos+bicubic         */
    AV_SCALE_SLOWER    = 8,  /* full dither,    lanczos+bicubic         */
    AV_SCALE_VERYSLOW  = 9,  /* full dither,    lanczos+lanczos         */
    AV_SCALE_PLACEBO   = 10, /* full dither,    lanczos+lanczos         */
};

enum AVDitherMode {
    AV_DITHER_AUTO = 0, /* auto-select from preset */
    AV_DITHER_NONE,     /* disable dithering */
    AV_DITHER_BAYER,    /* ordered dither matrix */
    AV_DITHER_FULL,     /* full error diffusion */
};

/**
 * Returns the default dither mode implied by a given quality preset.
 */
enum AVDitherMode avscale_default_dither(int preset);

enum AVScaleFilter {
    AV_SCALE_AUTO = 0,  /* auto-select from preset */
    AV_SCALE_NEAREST,   /* nearest neighbour */
    AV_SCALE_BILINEAR,  /* bilinear filtering */
    AV_SCALE_BICUBIC,   /* 2-tap cubic B-spline */
    AV_SCALE_GAUSSIAN,  /* gaussian approximation */
    AV_SCALE_LANCZOS,   /* 3-tap sinc/sinc */
};

/**
 * Returns the default scaling filters implied by a given quality preset.
 */
enum AVScaleFilter avscale_default_filter(int preset);
enum AVScaleFilter avscale_default_filter_sub(int preset);

/***************************
 * Supported frame formats *
 ***************************/

/**
 * Test if a given pixel format is supported.
 *
 * @param output  If 0, test if compatible with the source/input frame;
 *                otherwise, with the destination/output frame.
 * @param format  The format to check.
 *
 * @return A positive integer if supported, 0 otherwise.
 */
int avscale_test_format(enum AVPixelFormat format, int output);

/**
 * Test if a given color space is supported.
 *
 * @param output  If 0, test if compatible with the source/input frame;
 *                otherwise, with the destination/output frame.
 * @param colorspace The colorspace to check.
 *
 * @return A positive integer if supported, 0 otherwise.
 */
int avscale_test_colorspace(enum AVColorSpace colorspace, int output);

/**
 * Test if a given set of color primaries is supported.
 *
 * @param output  If 0, test if compatible with the source/input frame;
 *                otherwise, with the destination/output frame.
 * @param primaries The color primaries to check.
 *
 * @return A positive integer if supported, 0 otherwise.
 */
int avscale_test_primaries(enum AVColorPrimaries primaries, int output);

/**
 * Test if a given color transfer function is supported.
 *
 * @param output  If 0, test if compatible with the source/input frame;
 *                otherwise, with the destination/output frame.
 * @param trc     The color transfer function to check.
 *
 * @return A positive integer if supported, 0 otherwise.
 */
int avscale_test_transfer(enum AVColorTransferCharacteristic trc, int output);

/**
 * Helper function to run all avscale_test_* against a frame. Ignores irrelevant
 * properties, for example AVColorSpace is not checked for RGB frames.
 */
int avscale_test_frame(const AVFrame *frame, int output);

/********************
 * Main scaling API *
 ********************/

/**
 * Check if a given conversion is a noop. Returns a positive integer if
 * no operation needs to be performed, 0 otherwise.
 */
int avscale_is_noop(const AVFrame *dst, const AVFrame *src);

/**
 * Return the minimum slice alignment required for a given frame. This is
 * always a power of two, typically 1, 2 or 4 depending on the frame's
 * subsampling and interlacing.
 */
int avscale_slice_alignment(const AVFrame *frame);

/**
 * Scale source data from `src` and write the output to `dst`. This is
 * merely a convenience wrapper around `avscale_frame_slice(ctx, dst, src, 0,
 * src->height)`.
 *
 * @param ctx   The scaling context.
 * @param dst   The destination frame. See avscale_frame_slice().
 * @param src   The source frame. See avscale_frame_slice().
 * @return 0 on success, a negative AVERROR code on failure.
 */
int avscale_frame(AVScaleContext *ctx, AVFrame *dst, const AVFrame *src);

/**
 * Like `avscale_frame`, but operates only on the (source) range from `ystart`
 * to `height`.
 *
 * @param ctx   The scaling context.
 * @param dst   The destination frame. The data buffers may either be already
 *              allocated by the caller or left clear, in which case they will
 *              be allocated by the scaler. The latter may have performance
 *              advantages - e.g. in certain cases some (or all) output planes
 *              may be references to input planes, rather than copies.
 * @param src   The source frame. If the data buffers are set to NULL, then
 *              this function behaves identically to `avscale_frame_setup`.
 * @param slice_start   First row of slice, relative to `src`. Must be a
 *                      multiple of avscale_slice_alignment(src).
 * @param slice_height  Number of (source) rows in the slice. Must be a
 *                      multiple of avscale_slice_alignment(src).
 *
 * @return 0 on success, a negative AVERROR code on failure.
 */
int avscale_frame_slice(AVScaleContext *ctx, AVFrame *dst,
                        const AVFrame *src, int slice_start, int slice_height);

/**
 * Like `avscale_frame`, but without actually scaling. It will instead merely
 * initialize internal state that *would* be required to perform the operation,
 * as well as returning the correct error code for unsupported frame
 * combinations.
 *
 * @param ctx   The scaling context.
 * @param dst   The destination frame to consider.
 * @param src   The source frame to consider.
 * @return 0 on success, a negative AVERROR code on failure.
 */
int avscale_frame_setup(AVScaleContext *ctx, const AVFrame *dst,
                        const AVFrame *src);

#endif /* SWSCALE_AVSCALE_H */
