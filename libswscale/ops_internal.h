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

#include "ops.h"

/**
 * Match an operation against a reference operation. Returns a score for how
 * well the reference matches the operation, or 0 if there is no match.
 *
 * If `ref->comps` has any flags set, they must be set in `op` as well.
 * Likewise, if `ref->comps` has any components marked as unused, they must be
 * marked as as unused in `ops` as well.
 *
 * For SWS_OP_CLEAR, the clear value must match exactly if set in `ref`.
 * Otherwise, the clear pattern must match `ref->comps.unused` exactly.
 *
 * For SWS_OP_LSHIFT/RSHIFT, the shift amount must match exactly if nonzero
 * in `ref`. Otherwise, any shift amount is supported.
 *
 * For SWS_OP_LINEAR, `ref->linear.mask` must be a strict superset of
 * `op->linear.mask`, but may not contain any columns explicitly ignored by
 * `op->comps.unused`.
 *
 * Components set in `next.unused` are ignored when matching.
 */
int ff_sws_op_match(const SwsOp *op, const SwsOp *ref, SwsComps next);

/**
 * Global execution context for all operations.
 *
 * Note: Assembly routines accessing this struct should use avoid using
 * hard-coded offsets as the layout may change in the future. Instead, define
 * macros using offsetof() to get the field offsets.
 */
typedef struct SwsOpExec {
    SwsImg in, out;         /* Points to the current block start */
    int x, y;               /* Coordinates of the current block */
    int block_w, block_h;   /* Size of block in pixels */
} SwsOpExec;

typedef struct SwsOpImpl SwsOpImpl;
typedef void (*SwsOpFunc)(const SwsOpExec *exec, const SwsOpImpl *impl);

/* Per-implementation execution context */
struct SwsOpImpl {
    SwsOpFunc next;   /* Continuation for this operation. */
    const void *priv; /* Arbitrary allocated private data for this operation. */
};

/* Struct holding global information for the compile process */
typedef struct SwsCompileInfo {
    /* The associated read and write operations, for reference. */
    SwsOp read, write;

    /* Size in bytes of the largest pixel type used in the ops sequence */
    int pixel_size_max;
} SwsCompileInfo;

typedef struct SwsCompiledOp {
    /**
     * The number of pixels this compiled op will handle, or 0 if any block
     * size is supported by this function.
     *
     * Note: Mixing compiled operations with differing nonzero block sizes is
     * a runtime error.
     */
    int block_w, block_h;

    /**
     * The underlying function call for this operation.
     *
     * Note: Implementations of operations other than SWS_OP_READ may have a
     * different underlying type, or even a custom calling convention. Only
     * the first operation in the sequence will be directly called by the
     * calling code.
     *
     * Note: For read/write operations, the implementation may assume that
     * all image pointers are aligned to a multiple of the block size.
     */
    SwsOpFunc func;

    /**
     * Unaligned, variable size variant of `func`. Only used for READ/WRITE
     * operations. Optional. If NULL, `func` is used instead. The block size
     * is passed via `exec->block_w/h`.
     *
     * Note: The implementation may not assume any alignment.
     */
    SwsOpFunc func_n;

    /**
     * Private data associated with the compiled operation instance. Ownership
     * passes to the caller.
     */
    const void *priv;
    void (*free_priv)(void *priv); /* optional; if NULL, priv is not freed */
} SwsCompiledOp;

typedef struct SwsOpBackend {
    const char *name; /* Descriptive name for this backend */

    /**
     * Compile (one or more) operations. On success, `ops` is updated to point
     * to the remainder.
     *
     * Returns 0 or a negative error code.
     */
    int (*compile)(const SwsCompileInfo *info, SwsOpList *ops,
                   SwsCompiledOp *out_compiled);
} SwsOpBackend;

/**
 * Set of helpers for writing backends based on static function tables.
 * The use of these is optional, but they can help reduce common boilerplate.
 */

typedef struct SwsOpEntry {
    SwsOp op;
    SwsOpFunc func;
    SwsOpFunc func_n;
    int (*setup)(const SwsOp *op, const void **out_priv); /* optional */
    void (*free)(void *priv);
} SwsOpEntry;

typedef struct SwsOpTable {
    unsigned cpu_flags; /* required CPU flags for this table */
    int block_w;        /* fixed block size of this table */
    int block_h;

    SwsOpEntry entries[]; /* terminated by {0} */
} SwsOpTable;

/**
 * "Compile" an op by looking it up in a list of fixed size op tables.
 */
int ff_sws_op_compile(const SwsCompileInfo *info, const SwsOpTable *const tables[],
                      int num_tables, SwsOpList *ops, SwsCompiledOp *out_comp);

#endif
