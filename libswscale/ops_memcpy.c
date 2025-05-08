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

#include "libavutil/avassert.h"

#include "ops_backend.h"

typedef struct MemcpyPriv {
    int index[4]; /* or -1 to clear plane */
    uint8_t clear_value[4];
} MemcpyPriv;

/* Memcpy backend for trivial cases */

static void process(const SwsOpExec *exec, const void *priv, int num_blocks,
                    int num_lines)
{
    const MemcpyPriv *p = priv;
    av_assert1(num_blocks == exec->width);

    for (int i = 0; i < 4 && exec->out[i]; i++) {
        uint8_t *out = exec->out[i];
        const int idx = p->index[i];
        if (idx < 0) {
            memset(out, p->clear_value[i], exec->out_stride[i] * num_lines);
        } else if (exec->out_stride[i] == exec->in_stride[idx]) {
            memcpy(out, exec->in[idx], exec->out_stride[i] * num_lines);
        } else {
            const int bytes = num_blocks * exec->pixel_bits_out >> 3;
            const uint8_t *in = exec->in[idx];
            for (int n = 0; n < num_lines; n++) {
                memcpy(out, in, bytes);
                out += exec->out_stride[i];
                in  += exec->in_stride[idx];
            }
        }
    }
}

static int compile(SwsContext *ctx, SwsOpList *ops, SwsCompiledOp *out)
{
    MemcpyPriv p = {0};

    for (int n = 0; n < ops->num_ops; n++) {
        const SwsOp *op = &ops->ops[n];
        switch (op->op) {
        case SWS_OP_READ:
            if (op->rw.packed || op->rw.frac)
                return AVERROR(ENOTSUP);
            for (int i = 0; i < op->rw.elems; i++)
                p.index[i] = i;
            break;

        case SWS_OP_SWIZZLE: {
            const MemcpyPriv orig = p;
            for (int i = 0; i < 4; i++) {
                /* Explicitly exclude swizzle masks that contain duplicates,
                 * because these are wasteful to implement as a memcpy */
                for (int j = 0; j < i; j++) {
                    if (op->swizzle.in[i] == op->swizzle.in[j])
                        return AVERROR(ENOTSUP);
                }
                p.index[i] = orig.index[op->swizzle.in[i]];
            }
            break;
        }

        case SWS_OP_CLEAR:
            for (int i = 0; i < 4; i++) {
                if (!op->c.q4[i].den)
                    continue;
                if (op->c.q4[i].den != 1)
                    return AVERROR(ENOTSUP);

                /* Ensure all bytes to be cleared are the same, because we
                 * can't memset on multi-byte sequences */
                uint8_t val = op->c.q4[i].num & 0xFF;
                uint32_t ref = val;
                switch (ff_sws_pixel_type_size(op->type)) {
                case 2: ref *= 0x101; break;
                case 4: ref *= 0x1010101; break;
                }
                if (ref != op->c.q4[i].num)
                    return AVERROR(ENOTSUP);
                p.clear_value[i] = val;
                p.index[i] = -1;
            }
            break;

        case SWS_OP_WRITE:
            if (op->rw.packed || op->rw.frac)
                return AVERROR(ENOTSUP);
            break;

        default:
            return AVERROR(ENOTSUP);
        }
    }

    *out = (SwsCompiledOp) {
        .block_size = 1,
        .func = process,
        .priv = av_memdup(&p, sizeof(p)),
        .free = av_free,
    };
    return out->priv ? 0 : AVERROR(ENOMEM);
}

SwsOpBackend backend_murder = {
    .name    = "memcpy",
    .compile = compile,
};
