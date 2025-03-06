/**
 * Copyright (C) 2025 Niklas Haas
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

#ifndef SWSCALE_OPS_INTERNAL_H
#define SWSCALE_OPS_INTERNAL_H

#include "libavutil/mem.h"

#include "ops.h"

#ifdef __clang__
#  define SWS_FUNC av_noinline
#  define SWS_LOOP AV_PRAGMA(clang loop vectorize_width(SWS_CHUNK_SIZE))
#elif defined(__GNUC__)
#  define SWS_FUNC __attribute__((optimize("tree-vectorize"))) av_noinline
#  define SWS_LOOP AV_PRAGMA(GCC ivdep)
#endif

#if defined(__clang__)
#  define SWS_ASSUME(cond) __builtin_assume(cond)
#elif defined(__GNUC__)
#  define SWS_ASSUME(cond) { if (!(cond)) __builtin_unreachable(); }
#else
#  define SWS_ASSUME(cond) ((void) (cond))
#endif

#if defined(__clang__) || defined(__GNUC__)
#  define SWS_ASSUME_ALIGNED(ptr)  __builtin_assume_aligned(ptr, SWS_ALIGNMENT)
#else
#  define SWS_ASSUME_ALIGNED(ptr) ((void *) (ptr))
#endif

#define bitfn2(name, bits, suffix) name ## _ ## suffix ## _ ## bits ## bpc
#define bitfn(name, bits, suffix)  bitfn2(name, bits, suffix)

/* Operation tables */
typedef struct SwsOpEntry {
    /**
     * The set of reference operations for this entry.
     */
    const SwsOp *ops;
    int num_ops;

    /* The filter type is uniquely determined by ops[] */
    union {
        sws_read_write_t read_write;
        sws_read_t read;
        sws_write_t write;
        sws_filter_t filter;
        sws_generic_op_t op;
    };

    /* Separate case for variable size unaligned I/O, used to avoid
     * over-read/write as well as a fallback for unaligned inputs. */
    union {
        sws_read_t  read_n;
        sws_write_t write_n;
    };

    /**
     * Used to prepare data which will be passed back to the relevant
     * callbacks. Returns a pointer which will be av_free()'d by the caller,
     * or NULL on OOM. Optional.
     */
    void *(*setup)(const SwsOp *ops);
} SwsOpEntry;

typedef struct SwsOpTable {
    int cpu_flags; /* required CPU flags */
    int score; /* base score for this op table */

    const SwsOpEntry *entries;
    int num_entries;
} SwsOpTable;

#endif
