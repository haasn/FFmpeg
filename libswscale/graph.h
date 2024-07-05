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

/**
 * Sentinel values to refer to the overall image input / output during
 * filter graph construction, as the true values are not known.
 */
extern const uint8_t sws_input_sentinel, sws_output_sentinel;
#define SWS_INPUT  ((SwsField) {{ (uint8_t *) &sws_input_sentinel }})
#define SWS_OUTPUT ((SwsField) {{ (uint8_t *) &sws_output_sentinel }})

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
    SwsField input;
    SwsField output;

    /**
     * Called once from the main thread before running the filter. Optional.
     * `out` and `in` always point to the main image input/output, regardless
     * of `input` and `output` fields.
     */
    void (*setup)(const SwsField *out, const SwsField *in,
                  const SwsPass *pass);

    /**
     * Output `slice_h` lines of filtered data. `src` and `dst` point to the
     * start of the image buffer for this pass.
     */
    void (*run)(const SwsField *out, const SwsField *in, int y,
                const SwsPass *pass);

    void (*uninit)(void *priv); /* optional */
    void *priv;
};

/* Filter graph, which represents a 'baked' pixel format conversion */
typedef struct SwsGraph {
    SwsContext2 *ctx;
    AVSliceThread *slicethread;
    int num_threads; /* resolved at init() time */
    int incomplete;  /* set during init() if formats had to be inferred */
    SwsContext *sws; /* wrapped legacy context */

    /* Sorted sequence of filter passes to apply */
    SwsPass **passes;
    int num_passes;

    /* Overall image parameters */
    SwsFormat src, dst;
    int field;

    /* Temporary execution state inside sws_graph_run */
    struct {
        const SwsPass *pass; /* current filter pass */
        SwsField input;
        SwsField output;
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
void sws_graph_run(SwsGraph *graph, const SwsField *out, const SwsField *in);

#endif /* SWSCALE_GRAPH_H */
