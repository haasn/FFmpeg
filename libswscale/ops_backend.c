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

#ifndef SWS_BLOCK_SIZE
#  define SWS_BLOCK_SIZE 32
#endif

typedef  uint8_t  u8block_t[SWS_BLOCK_SIZE];
typedef uint16_t u16block_t[SWS_BLOCK_SIZE];
typedef uint32_t u32block_t[SWS_BLOCK_SIZE];
typedef    float f32block_t[SWS_BLOCK_SIZE];

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

static int compile(SwsContext *ctx, SwsOpList *ops, SwsOpChain *chain)
{
    static const SwsOpTable *const tables[] = {
        &bitfn(op_table_int,    u8),
        &bitfn(op_table_int,   u16),
        &bitfn(op_table_int,   u32),
        &bitfn(op_table_float, f32),
    };

    chain->block_size = SWS_BLOCK_SIZE;
    return ff_sws_op_compile_tables(tables, FF_ARRAY_ELEMS(tables), ops,
                                    chain->block_size, chain);
}

SwsOpBackend backend_c = {
    .name       = "c",
    .compile    = compile,
};
