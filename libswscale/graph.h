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

#include "avscale_internal.h"
#include "swscale.h"

typedef struct AVScaleGraph {
    /* Generic swscale fallback to cover use cases not yet implemented */
    struct SwsContext *fallback;
} AVScaleGraph;

/**
 * Initialize the filter graph. Returns 0 or a negative error code.
 */
int avscale_graph_init(AVScaleContext *ctx, AVScaleGraph *graph,
                       const AVScaleFormat *dst, const AVScaleFormat *src,
                       int field);

/**
 * Uninitialize any state associate with this filter graph.
 */
void avscale_graph_uninit(AVScaleGraph *graph);

/**
 * Dispatch the filter graph on a single slice of data. This function is
 * internally threadad.
 */
void avscale_graph_run(AVScaleGraph *graph, const AVScaleField *dst,
                       const AVScaleField *src, int y, int h);

#endif /* SWSCALE_GRAPH_H */
