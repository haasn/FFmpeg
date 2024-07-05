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

#ifndef SWSCALE_GRAPH_H
#define SWSCALE_GRAPH_H

#include "libavutil/slicethread.h"
#include "swscale_internal.h"
#include "swscale.h"

/* Explicit name for intermediate pixels, actual representation varies */
typedef char sws_pixel_t;

enum {
    /* TODO: remove / fine tune these */
    SWS_OP_SIZE     = 256,
    SWS_MAX_OPS     = 16,
    SWS_MAX_FILTERS = 16,
};

/* Atomic processing step, which does not scale or filter the pixel data. */
typedef struct SwsPixelOp {
    /**
     * data: packed pixel data, modified in-place
     * size: number of pixels, no larger than SWS_OP_SIZE / pixel size
     */
    void (*process)(sws_pixel_t *data, int size, void *priv);
    void *priv; /* freed by calling code */
} SwsPixelOp;

/* Filter step, which may read from source data and scale/filter it. */
typedef struct SwsFilterOp {
    int new_w, new_h;
    int slice_h; /* read/write will be called with this granularity */
    int num_slices;

    /* Output for intermediate data. Managed by calling code. */
    sws_pixel_t *tmp;
    size_t stride;

    /**
     * Produce one slice of filtered, packed pixel data, starting on line
     * `y` of the output image size (relative to `dst_h`). Optional for
     * pure output filters.
     */
    void (*read)(int y, sws_pixel_t *out, void *priv);

    /* Pixel operations to apply after the read callback has been called */
    SwsPixelOp post_ops[SWS_MAX_OPS];
    int num_post_ops;

    /**
     * Optional output callback to be called after postprocessing filters.
     * Intended for output passes writing to the final image.
     */
    void (*write)(int y, const sws_pixel_t *in, void *priv);

    void (*uninit)(void *priv); /* optional */
    void *priv; /* freed by calling code */
} SwsFilterOp;

/* Filter graph, which represents a 'baked' pixel format conversion */
typedef struct SwsGraph {
    SwsContext2 *ctx;
    AVSliceThread *slicethread;
    int num_threads; /* resolved at init() time */
    int incomplete;  /* set during init() if formats had to be inferred */

    /* Sorted sequence of filters to apply */
    SwsFilterOp filters[SWS_MAX_FILTERS];
    int num_filters;

    /* Overall image parameters */
    SwsFormat src, dst;
    int field;

    /* Temporary execution state inside sws_graph_run */
    struct SwsExec {
        const SwsField *src, *dst;
        const SwsFilterOp *filter; /* currently active filter */
    } exec;
} SwsGraph;

/**
 * Initialize the filter graph. Returns 0 or a negative error code.
 */
int sws_graph_init(SwsContext2 *ctx, SwsGraph *graph,
                   const SwsFormat *dst, const SwsFormat *src,
                   int field);

/**
 * Uninitialize any state associate with this filter graph.
 */
void sws_graph_uninit(SwsGraph *graph);

/**
 * Dispatch the filter graph on a single field. Internally threaded.
 */
void sws_graph_run(SwsGraph *graph, const SwsField *dst, const SwsField *src);

/**
 * For internal use by filters. (WIP)
 */

/* `line_h` should be a power of two */
SwsFilterOp *sws_add_filter(SwsGraph *graph, int new_w, int new_h,
                            int line_h, size_t priv_size);

SwsPixelOp *sws_add_post_op(SwsFilterOp *filter, size_t priv_size);

#endif /* SWSCALE_GRAPH_H */
