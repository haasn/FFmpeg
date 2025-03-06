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

#ifndef SWSCALE_OPS_H
#define SWSCALE_OPS_H

#include <assert.h>
#include <stdbool.h>
#include <stdalign.h>

#include "graph.h"
#include "format.h"

typedef enum SwsPixelType {
    SWS_PIXEL_INVALID = 0,
    SWS_PIXEL_U8,
    SWS_PIXEL_U16,
    SWS_PIXEL_U32,
    SWS_PIXEL_F32,
    SWS_PIXEL_TYPE_NB
} SwsPixelType;

typedef enum SwsOpType {
    SWS_OP_INVALID = 0,

    /* Input/output handling */
    SWS_OP_READ,            /* gather raw pixels from planes */
    SWS_OP_WRITE,           /* write raw pixels to planes */
    SWS_OP_SWAP_BYTES,      /* swap byte order (for differing endianness) */
    SWS_OP_UNPACK,          /* split tightly packed data into components */
    SWS_OP_PACK,            /* compress components into tightly packed data */

    /* Pixel manipulation */
    SWS_OP_CLEAR,           /* clear pixel values */
    SWS_OP_LSHIFT,          /* logical left shift of raw pixel values */
    SWS_OP_RSHIFT,          /* right shift of raw pixel values */
    SWS_OP_SWIZZLE,         /* rearrange channel order, or duplicate channels */
    SWS_OP_CONVERT,         /* convert (cast) between formats */
    SWS_OP_DITHER,          /* add dithering noise */
    SWS_OP_CLAMP,           /* clamp pixel values to value range */

    /* Arithmetic operations */
    SWS_OP_LINEAR,          /* generalized linear affine transform */
    SWS_OP_SCALE,           /* multiplication by scalar */

    SWS_OP_TYPE_NB,
} SwsOpType;

enum SwsCompFlags {
    SWS_COMP_GARBAGE = 1 << 0, /* contents are undefined / garbage data */
    SWS_COMP_EXACT   = 1 << 1, /* value is an in-range, exact, integer */
    SWS_COMP_ZERO    = 1 << 2, /* known to be a constant zero */
};

typedef struct SwsComps {
    unsigned flags[4]; /* knowledge about (output) component contents */
    bool unused[4];    /* which input components are definitely unused */
} SwsComps;

typedef struct SwsReadWriteOp {
    /* Note: Unread pixel data is explicitly cleared to {0} for sanity */

    int elems;   /* number of elements (of type `op.type`) to read/write */
    bool planar; /* components live in separate planes */
    int frac;    /* fractional pixel step factor (log2) */

    /** Examples:
     *    rgba      = 4x u8 packed
     *    yuv444p   = 3x u8
     *    rgb565    = 1x u16   <- use SWS_OP_UNPACK to unpack
     *    monow     = 1x u8 (frac 3)
     *    rgb4      = 1x u8 (frac 1)
     */
} SwsReadWriteOp;

typedef struct SwsPackOp {
    SwsPixelType type; /* packed type (e.g. U16 for rgb565) */
    int pattern[4];    /* bit depth pattern, from MSB to LSB */
} SwsPackOp;

typedef struct SwsClearOp {
    /* Value to set each affected component to, or {0, 0} to ignore */
    AVRational value[4];
} SwsClearOp;

typedef struct SwsShiftOp {
    uint8_t amount; /* shift amount, applied to all components */
} SwsShiftOp;

typedef struct SwsSwizzleOp {
    /**
     * Input component for each output component:
     *   Out[x] := In[swizzle.in[x]]
     */
    union {
        uint32_t mask;
        uint8_t in[4];
        struct { uint8_t x, y, z, w; };
    };
} SwsSwizzleOp;

#define SWS_SWIZZLE(X,Y,Z,W) ((SwsSwizzleOp) { .in = {X, Y, Z, W} })

typedef struct SwsConvertOp {
    SwsPixelType to; /* type of pixel to convert to */
    bool expand; /* if true, integers are expanded to the full range */
} SwsConvertOp;

typedef struct SwsDitherOp {
    AVRational *matrix; /* tightly packed dither matrix, freed automatically */
    int size_log2; /* size (in bits) of the dither matrix */
} SwsDitherOp;

typedef struct SwsClampOp {
    /* The minimum value range is always hard-coded as 0 */
    AVRational max[4]; /* upper value range, or {0, 0} for no upper bound */
} SwsClampOp;

typedef struct SwsLinearOp {
    /**
     * Generalized 5x5 affine transformation:
     *   [ Out.x ] = [ A B C D E ]
     *   [ Out.y ] = [ F G H I J ] * [ x y z w 1 ]
     *   [ Out.z ] = [ K L M N O ]
     *   [ Out.w ] = [ P Q R S T ]
     *
     * The mask keeps track of which components differ from an identity matrix.
     * There may be more efficient implementations of particular subsets, for
     * example the common subset of {A, E, G, J, M, O} can be implemented with
     * just three fused multiply-add operations.
     */
    AVRational m[4][5];
    uint32_t mask; /* m[i][j] <-> 1 << (5 * i + j) */
} SwsLinearOp;

typedef struct SwsScaleOp {
    AVRational factor; /* scalar to multiply by */
} SwsScaleOp;

#define SWS_MASK(I, J)  (1 << (5 * (I) + (J)))
#define SWS_MASK_OFF(I) SWS_MASK(I, 4)
#define SWS_MASK_ROW(I) (0b11111 << (5 * (I)))
#define SWS_MASK_COL(J) (0b1000010000100001 << J)

enum {
    SWS_MASK_ALL   = (1 << 20) - 1,
    SWS_MASK_LUMA  = SWS_MASK(0, 0) | SWS_MASK_OFF(0),
    SWS_MASK_ALPHA = SWS_MASK(3, 3) | SWS_MASK_OFF(3),

    SWS_MASK_DIAG3 = SWS_MASK(0, 0)  | SWS_MASK(1, 1)  | SWS_MASK(2, 2),
    SWS_MASK_OFF3  = SWS_MASK_OFF(0) | SWS_MASK_OFF(1) | SWS_MASK_OFF(2),
    SWS_MASK_MAT3  = SWS_MASK(0, 0)  | SWS_MASK(0, 1)  | SWS_MASK(0, 2) |
                     SWS_MASK(1, 0)  | SWS_MASK(1, 1)  | SWS_MASK(1, 2) |
                     SWS_MASK(2, 0)  | SWS_MASK(2, 1)  | SWS_MASK(2, 2),

    SWS_MASK_DIAG4 = SWS_MASK_DIAG3  | SWS_MASK(3, 3),
    SWS_MASK_OFF4  = SWS_MASK_OFF3   | SWS_MASK_OFF(3),
    SWS_MASK_MAT4  = SWS_MASK_ALL & ~SWS_MASK_OFF4,
};

/* Helper function to compute the correct mask */
uint32_t ff_sws_linear_mask(SwsLinearOp);

typedef struct SwsOp {
    SwsOpType op;      /* operation to perform */
    SwsPixelType type; /* pixel type to operate on */
    union {
        SwsReadWriteOp  rw;
        SwsPackOp       pack;
        SwsClearOp      clear;
        SwsShiftOp      shift;
        SwsSwizzleOp    swizzle;
        SwsConvertOp    convert;
        SwsDitherOp     dither;
        SwsClampOp      clamp;
        SwsLinearOp     lin;
        SwsScaleOp      scale;
    };

    /* For use internal use inside ff_sws_*() functions */
    SwsComps comps;
} SwsOp;

/**
 * Frees any allocations associated with an SwsOp and sets it to {0}.
 */
void ff_sws_op_uninit(SwsOp *op);

/**
 * Apply an operation to a static pixel value. No-op for read/write operations.
 */
void ff_sws_apply_op_q(const SwsOp *op, AVRational x[4]);

/**
 * Helper struct for representing a list of operations.
 */
typedef struct SwsOpList {
    SwsOp *ops;
    int num_ops;
} SwsOpList;

SwsOpList *ff_sws_op_list_alloc(void);
void ff_sws_op_list_free(SwsOpList **ops);

/**
 * These will take over ownership of `op` and set it to {0}, even on failure.
 */
int ff_sws_op_list_append(SwsOpList *ops, SwsOp *op);
int ff_sws_op_list_insert_at(SwsOpList *ops, int index, SwsOp *op);

void ff_sws_op_list_remove_at(SwsOpList *ops, int index, int count);

/**
 * Print out the contents of an operation list.
 */
void ff_sws_op_list_print(void *log_ctx, int log_level, const SwsOpList *ops);

/**
 * Fuse compatible and eliminate redundant operations, as well as replacing
 * some operations with more efficient alternatives.
 */
int ff_sws_op_list_optimize(SwsOpList *ops);

enum SwsOpCompileFlags {
    /* Automatically optimize the operations when compiling */
    SWS_OP_FLAG_OPTIMIZE = 1 << 0,
};

/**
 * Resolves an operation list to a graph pass. The first and last operations
 * must be a read/write respectively. `flags` is a list of SwsOpCompileFlags.
 *
 * Note: `ops` may be modified by this function.
 */
int ff_sws_compile_pass(SwsGraph *graph, SwsOpList *ops, int flags, SwsFormat dst,
                        SwsPass *input, SwsPass **output);

#endif
