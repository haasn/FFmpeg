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
#include "swscale.h"
#include "utils.h"

/* Represents a view into a single field of frame data */
typedef struct SwsImg {
    uint8_t *data[4]; /* points to y=0 */
    int linesize[4];
} SwsImg;

typedef struct SwsPass  SwsPass;
typedef struct SwsGraph SwsGraph;

struct SwsPass {
    const SwsGraph *graph;
    uint8_t *buf; /* temporary buffer for this pass, freed automatically */

    int width, height; /* new output size */
    int pixel_bytes;   /* bytes per pixel */
    int slice_h;       /* filter granularity */
    int num_slices;

    /* Filter input/output. */
    SwsImg input;
    SwsImg output;

    /**
     * Called once from the main thread before running the filter. Optional.
     * `out` and `in` always point to the main image input/output, regardless
     * of `input` and `output` fields.
     */
    void (*setup)(const SwsImg *out, const SwsImg *in, const SwsPass *pass);

    /**
     * Output `slice_h` lines of filtered data. `src` and `dst` point to the
     * start of the image buffer for this pass.
     */
    void (*run)(const SwsImg *out, const SwsImg *in, int y, int h,
                const SwsPass *pass);

    void (*uninit)(const SwsPass *pass); /* optional */
    void *priv;
};

/* Filter graph, which represents a 'baked' pixel format conversion */
typedef struct SwsGraph {
    SwsContext *ctx;
    AVSliceThread *slicethread;
    int num_threads; /* resolved at init() time */
    int incomplete;  /* set during init() if formats had to be inferred */
    SwsContext *sws; /* wrapped legacy context */

    /* Sorted sequence of filter passes to apply */
    SwsPass **passes;
    int num_passes;

    /* Overall image parameters and flags */
    SwsContext opts;
    SwsFormat src, dst;
    int field;
    int noop; /* true if the graph is a no-op */

    /* Temporary execution state inside sws_graph_run */
    struct {
        const SwsPass *pass; /* current filter pass */
        SwsImg input;
        SwsImg output;
    } exec;
} SwsGraph;

/**
 * Allocate and initialize the filter graph. Returns 0 or a negative error.
 */
int sws_graph_create(SwsContext *ctx, const SwsFormat *dst, const SwsFormat *src,
                     int field, SwsGraph **out_graph);

/**
 * Uninitialize any state associate with this filter graph and free it.
 */
void sws_graph_free(SwsGraph **graph);

/**
 * Wrapper around sws_graph_create that does nothing if the format is
 * unchanged.
 */
int sws_graph_reinit(SwsContext *ctx, const SwsFormat *dst, const SwsFormat *src,
                     int field, SwsGraph **graph);

/**
 * Dispatch the filter graph on a single field. Internally threaded.
 */
void sws_graph_run(SwsGraph *graph, const SwsImg *out, const SwsImg *in);

#endif /* SWSCALE_GRAPH_H */
