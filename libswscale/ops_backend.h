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

#ifndef SWSCALE_OPS_BACKEND_H
#define SWSCALE_OPS_BACKEND_H

/**
 * Common helper macros for C-based backends.
 * To use these macros, the following types must be defined:
 *  - PIXEL_TYPE should be one of SWS_PIXEL_*
 *  - PIXEL_ARG(n) should declare a continuation parameter named `n`
 *  - PIXEL_REF(n) should refer to the continuation parameter named `n`
 *  - pixel_t should be the type of pixels
 */

#include <assert.h>
#include <float.h>
#include <stdint.h>

#include "libavutil/attributes.h"

#include "ops_internal.h"

#ifdef __clang__
#  define SWS_FUNC
#  define SWS_LOOP AV_PRAGMA(clang loop vectorize(assume_safety))
#elif defined(__GNUC__)
#  define SWS_FUNC __attribute__((optimize("tree-vectorize")))
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
#  define SWS_ASSUME_ALIGNED(ptr, align)  __builtin_assume_aligned(ptr, align)
#else
#  define SWS_ASSUME_ALIGNED(ptr, align) ((void *) (ptr))
#endif

#define bitfn2(name, ext) name ## _ ## ext
#define bitfn(name, ext)  bitfn2(name, ext)

#define FN_SUFFIX AV_JOIN(FMT_CHAR, BIT_DEPTH)
#define fn(name)  bitfn(name, FN_SUFFIX)

/* Helper macros to make writing common function signatures less painful */
#define DECL_FUNC(NAME, ...)                                                    \
    static av_always_inline void fn(NAME)(const SwsOpExec *restrict exec,       \
                                          const SwsOpImpl *restrict impl,       \
                                          PIXEL_ARG(x), PIXEL_ARG(y),           \
                                          PIXEL_ARG(z), PIXEL_ARG(w),           \
                                          __VA_ARGS__)

#define DECL_READ(NAME, ...)                                                    \
    static av_always_inline void fn(NAME)(const SwsOpExec *restrict exec,       \
                                          const SwsOpImpl *restrict impl,       \
                                          const pixel_t *restrict in0,          \
                                          const pixel_t *restrict in1,          \
                                          const pixel_t *restrict in2,          \
                                          const pixel_t *restrict in3,          \
                                          __VA_ARGS__)

#define DECL_WRITE(NAME, ...)                                                   \
    DECL_FUNC(NAME, pixel_t *restrict out0, pixel_t *restrict out1,             \
                    pixel_t *restrict out2, pixel_t *restrict out3,             \
                    __VA_ARGS__)

#define DECL_FUNC_PATTERN(NAME) \
    DECL_FUNC(NAME, const bool X, const bool Y, const bool Z, const bool W)

/* Helper macros to call into functions declared with DECL_FUNC_* */
#define CALL_READONLY(FUNC, ...) \
    fn(FUNC)(exec, impl, __VA_ARGS__)

#define CALL(FUNC, ...)                                                         \
    CALL_READONLY(FUNC, PIXEL_REF(x), PIXEL_REF(y),                             \
                        PIXEL_REF(z), PIXEL_REF(w), __VA_ARGS__)

/* Helper macros to declare continuation functions */
#define DECL_IMPL_READONLY(NAME)                                                \
    static SWS_FUNC void fn(NAME)(const SwsOpExec *restrict exec,               \
                                  const SwsOpImpl *restrict impl)               \

#define DECL_IMPL(NAME)                                                         \
    static SWS_FUNC void fn(NAME)(const SwsOpExec *restrict exec,               \
                                  const SwsOpImpl *restrict impl,               \
                                  PIXEL_ARG(x), PIXEL_ARG(y),                   \
                                  PIXEL_ARG(z), PIXEL_ARG(w))                   \

/* Helper macro to call into the next continuation with a given type */
#define CONTINUE(VTYPE, ...)                                                    \
    ((void (*)(const SwsOpExec *, const SwsOpImpl *,                            \
               VTYPE x, VTYPE y, VTYPE z, VTYPE w)) impl->next)                 \
        (exec, &impl[1], __VA_ARGS__)

#define CONTINUE2(VTYPE, ...)                                                   \
    ((void (*)(const SwsOpExec *, const SwsOpImpl *,                            \
               VTYPE xl, VTYPE xh, VTYPE yl, VTYPE yh,                          \
               VTYPE zl, VTYPE zh, VTYPE wl, VTYPE wh)) impl->next)             \
        (exec, &impl[1], __VA_ARGS__)

/* Helper macros for common op setup code */
#define DECL_SETUP(NAME)                                                        \
    static int fn(setup_##NAME)(const SwsOp *op, const void **out_priv)

#define SETUP_MEMDUP(c) ff_setup_memdup(&c, sizeof(c), out_priv)
static inline int ff_setup_memdup(const void *c, size_t size, const void **out)
{
    *out = av_memdup(c, size);
    return *out ? 0 : AVERROR(ENOMEM);
}

/* Helper macros for declaring op table entries */
#define DECL_ENTRY(NAME, ...)                                                   \
    static const SwsOpEntry fn(op_##NAME) = {                                   \
        .op.type = PIXEL_TYPE,                                                  \
        .func    = (SwsOpFunc) fn(NAME),                                        \
        __VA_ARGS__                                                             \
    }

#define DECL_ENTRY_SIMPLE(NAME, ...)                                            \
    static const SwsOpEntry fn(op_##NAME) = {                                   \
        .func = (SwsOpFunc) fn(NAME),                                           \
        .op = {                                                                 \
            .type = PIXEL_TYPE,                                                 \
            __VA_ARGS__                                                         \
        },                                                                      \
    }

/* Helpers for dealing with (common) subsets of operations (Y, YA, YUV, YUVA) */
#define WRAP_PATTERN(FUNC, X, Y, Z, W, ...)                                     \
    DECL_IMPL(FUNC##_##X##Y##Z##W)                                              \
    {                                                                           \
        CALL(FUNC, X, Y, Z, W);                                                 \
    }                                                                           \
                                                                                \
    DECL_ENTRY(FUNC##_##X##Y##Z##W,                                             \
        .op.comps.unused = { !X, !Y, !Z, !W },                                  \
        __VA_ARGS__                                                             \
    )

#define WRAP_COMMON_PATTERNS(FUNC, ...)                                         \
    WRAP_PATTERN(FUNC, 1, 0, 0, 0, __VA_ARGS__);                                \
    WRAP_PATTERN(FUNC, 1, 0, 0, 1, __VA_ARGS__);                                \
    WRAP_PATTERN(FUNC, 1, 1, 1, 0, __VA_ARGS__);                                \
    WRAP_PATTERN(FUNC, 1, 1, 1, 1, __VA_ARGS__)

/* Miscellaneous helpers */
#define av_q2pixel(q) ((q).den ? (pixel_t) (q).num / (q).den : 0)

#endif
