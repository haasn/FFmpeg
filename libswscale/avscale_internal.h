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

#ifndef SWSCALE_AVSCALE_INTERNAL_H
#define SWSCALE_AVSCALE_INTERNAL_H

#include <libavutil/internal.h>
#include <libavutil/pixdesc.h>

#include "avscale.h"

/* Tests only options that affect graph state */
static inline int avscale_opts_equal(const AVScaleContext *ctx1,
                                     const AVScaleContext *ctx2)
{
FF_DISABLE_DEPRECATION_WARNINGS
    return ctx1->flags      == ctx2->flags      &&
           ctx1->threads    == ctx2->threads    &&
           ctx1->quality    == ctx2->quality    &&
           ctx1->dither     == ctx2->dither     &&
           ctx1->filter     == ctx2->filter     &&
           ctx1->filter_sub == ctx2->filter_sub &&
           ctx1->sws_flags  == ctx2->sws_flags;
FF_ENABLE_DEPRECATION_WARNINGS
}

/* Represents a view into a single field of frame data */
typedef struct AVScaleField {
    uint8_t *data[4]; /* points to y=0 */
    int linesize[4];
} AVScaleField;

enum {
    FIELD_TOP, /* top/even rows, or progressive */
    FIELD_BOTTOM, /* bottom/odd rows */
};

AVScaleField avscale_get_field(const AVFrame *frame, int field);

/* Subset of AVFrame parameters that uniquely determine pixel representation */
typedef struct AVScaleFormat {
    int width, height;
    int interlaced;
    enum AVPixelFormat format;
    enum AVColorRange range;
    enum AVColorPrimaries prim;
    enum AVColorTransferCharacteristic trc;
    enum AVColorSpace csp;
    enum AVChromaLocation loc;
    const AVPixFmtDescriptor *desc; /* convenience */
} AVScaleFormat;

static inline int avscale_fmt_equal(const AVScaleFormat *fmt1,
                                    const AVScaleFormat *fmt2)
{
    return fmt1->width      == fmt2->width      &&
           fmt1->height     == fmt2->height     &&
           fmt1->interlaced == fmt2->interlaced &&
           fmt1->format     == fmt2->format     &&
           fmt1->range      == fmt2->range      &&
           fmt1->prim       == fmt2->prim       &&
           fmt1->trc        == fmt2->trc        &&
           fmt1->csp        == fmt2->csp        &&
           fmt1->loc        == fmt2->loc;
}

int avscale_test_fmt(const AVScaleFormat *fmt, int output);

#endif /* SWSCALE_AVSCALE_INTERNAL_H */
