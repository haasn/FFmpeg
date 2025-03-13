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

#include "ops_internal.h"
#include "ops_backend.h"

/* Array-based reference implementation */

#ifndef SWS_CHUNK_SIZE
#  define SWS_CHUNK_SIZE 32
#endif

#define PIXEL_ARG(X) pixel_t *restrict X
#define PIXEL_REF(X) X

#define BIT_DEPTH 8
# include "ops_tmpl_int.c"
#undef BIT_DEPTH

#define BIT_DEPTH 16
# include "ops_tmpl_int.c"
#undef BIT_DEPTH

#define BIT_DEPTH 32
# include "ops_tmpl_int.c"
# include "ops_tmpl_float.c"
#undef BIT_DEPTH

static int compile(const SwsCompileInfo *info, SwsOpList *ops, SwsCompiledOp *out)
{
    static const SwsOpTable *const tables[] = {
        &bitfn(op_table_int,    u8),
        &bitfn(op_table_int,   u16),
        &bitfn(op_table_int,   u32),
        &bitfn(op_table_float, f32),
    };

    return ff_sws_op_compile(info, tables, FF_ARRAY_ELEMS(tables), ops, out);
}

SwsOpBackend backend_c = {
    .name    = "c",
    .compile = compile,
};
