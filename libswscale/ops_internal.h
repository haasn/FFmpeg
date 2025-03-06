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

#include "libavutil/cpu.h"
#include "libavutil/mem.h"
#include "libavutil/mem_internal.h"

#include "ops.h"

/**
 * Global execution context for all operations.
 *
 * Note: This struct is hard-coded in assembly, so do not change the layout
 * without updating the corresponding assembly definitions.
 */
typedef struct __attribute__((packed)) SwsOpExec {
    const uint8_t *in[4];       /* Points to start of block input */
    uint8_t *out[4];            /* Points to start of block output */
    ptrdiff_t in_stride[4];     /* Separation in bytes between lines */
    ptrdiff_t out_stride[4];

    int32_t x, y;               /* Coordinates of the current block */
    int32_t w, h;               /* Overall dimensions being processed */
    int32_t slice_y, slice_h;   /* Start and height of current slice */
    int32_t block_w, block_h;   /* Configured processing block size */
} SwsOpExec;

typedef struct SwsOpImpl SwsOpImpl;
typedef void (*SwsFunc)(const SwsOpExec *exec, const SwsOpImpl *impl);

/**
 * Private data for operation implementations.
 */
typedef union SwsOpPriv {
    DECLARE_ALIGNED_16(char, data)[16];

    /* Common types */
    void *ptr;
    uint8_t   u8[16];
    uint16_t u16[8];
    uint32_t u32[4];
    float    f32[4];
} SwsOpPriv;

static_assert(sizeof(SwsOpPriv) == 16, "SwsOpPriv size mismatch");

/**
 * Per-implementation execution context.
 *
 * Note: This struct is hard-coded in assembly, so do not change the layout.
 */
struct SwsOpImpl {
    SwsFunc   cont; /* [offset =  0] Continuation for this operation. */
    SwsOpPriv priv; /* [offset = 16] Private data for this operation. */
};

static_assert(sizeof(SwsOpImpl) == 32,         "SwsOpImpl layout mismatch");
static_assert(offsetof(SwsOpImpl, priv) == 16, "SwsOpImpl layout mismatch");

/* Compiled chain of operations, which can be dispatched efficiently */
typedef struct SwsOpChain {
    int block_w, block_h; /* Block size for this chain */
    SwsFunc entry; /* First function to call */

    /* Chain of successive implementations */
#define SWS_MAX_OPS 16
    SwsOpImpl impl[SWS_MAX_OPS];
    void (*free[SWS_MAX_OPS])(void *);
    int num_impl;
} SwsOpChain;

void ff_sws_op_chain_uninit(SwsOpChain *chain);

/* Returns 0 on success, or a negative error code. */
int ff_sws_op_chain_append(SwsOpChain *chain, SwsFunc func,
                           void (*free)(void *), SwsOpPriv priv);

typedef struct SwsOpBackend {
    const char *name; /* Descriptive name for this backend */

    /**
     * Compile an operation list to an implementation chain. May modify `ops`
     * freely; the original list will be freed automatically by the caller.
     *
     * Returns 0 or a negative error code.
     */
    int (*compile)(SwsContext *ctx, SwsOpList *ops, SwsOpChain *chain);
} SwsOpBackend;

/* List of all backends, terminated by NULL */
extern const SwsOpBackend *const ff_sws_op_backends[];
extern const int ff_sws_num_op_backends; /* excludes terminating NULL */

/**
 * Attempt to compile a list of operations using a specific backend.
 *
 * Returns 0 on success, or a negative error code on failure.
 */
int ff_sws_ops_compile_backend(SwsContext *ctx, const SwsOpBackend *backend,
                               const SwsOpList *ops, SwsOpChain *chain);

/**
 * Compile a list of operations using the best available backend.
 *
 * Returns 0 on success, or a negative error code on failure.
 */
int ff_sws_ops_compile(SwsContext *ctx, const SwsOpList *ops, SwsOpChain *chain);

/**
 * Set of helpers for writing backends based on static function tables.
 * The use of these is optional, but they can help reduce common boilerplate.
 */

typedef struct SwsOpEntry {
    SwsOp op;
    SwsFunc func;
    bool flexible; /* if true, only the type and op are matched */
    int (*setup)(const SwsOp *op, SwsOpPriv *out); /* optional */
    void (*free)(void *priv);
} SwsOpEntry;

typedef struct SwsOpTable {
    unsigned cpu_flags;   /* required CPU flags for this table */
    int block_w;          /* fixed block size of this table */
    int block_h;
    SwsOpEntry entries[]; /* terminated by {0} */
} SwsOpTable;

/**
 * "Compile" a single op by looking it up in a list of fixed size op tables.
 * See `op_match` in `ops.c` for details on how the matching works.
 *
 * Returns 0, AVERROR(EAGAIN), or a negative error code.
 */
int ff_sws_op_compile_tables(const SwsOpTable *const tables[], int num_tables,
                             SwsOpList *ops, const int block_w, const int block_h,
                             SwsOpChain *chain);

/* Setup helpers */
int ff_sws_setup_u(const SwsOp *op, SwsOpPriv *out);
int ff_sws_setup_q(const SwsOp *op, SwsOpPriv *out);
int ff_sws_setup_q4(const SwsOp *op, SwsOpPriv *out);

#endif
