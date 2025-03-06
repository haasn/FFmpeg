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
#include "libavutil/bswap.h"
#include "libavutil/mem.h"
#include "libavutil/rational.h"

#include "ops.h"
#include "ops_internal.h"

static const SwsOpBackend * const sws_op_backends[] = {
};

#define Q(N) ((AVRational) { N, 1 })

#define RET(x)                                                                 \
    do {                                                                       \
        if ((ret = (x)) < 0)                                                   \
            return ret;                                                        \
    } while (0)

static const char *pixel_type_name(SwsPixelType type)
{
    switch (type) {
    case SWS_PIXEL_U8:  return "u8";
    case SWS_PIXEL_U16: return "u16";
    case SWS_PIXEL_U32: return "u32";
    case SWS_PIXEL_F32: return "f32";
    case SWS_PIXEL_INVALID:
    case SWS_PIXEL_TYPE_NB: break;
    }

    av_assert0(!"Invalid pixel type!");
    return "ERR";
}

static av_const int pixel_type_size(SwsPixelType type)
{
    switch (type) {
    case SWS_PIXEL_U8:  return sizeof(uint8_t);
    case SWS_PIXEL_U16: return sizeof(uint16_t);
    case SWS_PIXEL_U32: return sizeof(uint32_t);
    case SWS_PIXEL_F32: return sizeof(float);
    case SWS_PIXEL_INVALID:
    case SWS_PIXEL_TYPE_NB: break;
    }

    av_assert0(!"Invalid pixel type!");
    return 0;
}

static av_const bool pixel_type_is_int(SwsPixelType type)
{
    switch (type) {
    case SWS_PIXEL_U8:
    case SWS_PIXEL_U16:
    case SWS_PIXEL_U32:
        return true;
    case SWS_PIXEL_F32:
        return false;
    case SWS_PIXEL_INVALID:
    case SWS_PIXEL_TYPE_NB: break;
    }

    av_assert0(!"Invalid pixel type!");
    return false;
}

/* Returns true for operations that are independent per channel. These can
 * usually be commuted freely other such operations. */
static bool op_type_is_independent(SwsOpType op)
{
    switch (op) {
    case SWS_OP_SWAP_BYTES:
    case SWS_OP_LSHIFT:
    case SWS_OP_RSHIFT:
    case SWS_OP_CONVERT:
    case SWS_OP_DITHER:
    case SWS_OP_CLAMP:
    case SWS_OP_SCALE:
        return true;
    case SWS_OP_INVALID:
    case SWS_OP_READ:
    case SWS_OP_WRITE:
    case SWS_OP_SWIZZLE:
    case SWS_OP_CLEAR:
    case SWS_OP_LINEAR:
    case SWS_OP_PACK:
    case SWS_OP_UNPACK:
        return false;
    case SWS_OP_TYPE_NB:
        break;
    }

    av_assert0(!"Invalid operation type!");
    return false;
}

static AVRational av_clip_q(AVRational a, AVRational min, AVRational max)
{
    /* No-op when either value is NaN */
    if (av_cmp_q(a, min) == -1)
        a = min;
    if (av_cmp_q(a, max) == 1)
        a = max;
    return a;
}

static AVRational expand_factor(SwsPixelType from, SwsPixelType to)
{
    const int src = pixel_type_size(from);
    const int dst = pixel_type_size(to);
    int scale = 0;
    for (int i = 0; i < dst / src; i++)
        scale = scale << src * 8 | 1;
    return Q(scale);
}

void ff_sws_apply_op_q(const SwsOp *op, AVRational x[4])
{
    switch (op->op) {
    case SWS_OP_READ:
    case SWS_OP_WRITE:
    case SWS_OP_UNPACK: {
        unsigned val = x[0].num;
        int shift = pixel_type_size(op->pack.type) * 8;
        for (int i = 0; i < 4; i++) {
            const unsigned mask = (1 << op->pack.pattern[i]) - 1;
            shift -= op->pack.pattern[i];
            x[i] = Q((val >> shift) & mask);
        }
        return;
    }
    case SWS_OP_PACK: {
        unsigned val = 0;
        int shift = pixel_type_size(op->pack.type) * 8;
        for (int i = 0; i < 4; i++) {
            const unsigned mask = (1 << op->pack.pattern[i]) - 1;
            shift -= op->pack.pattern[i];
            val |= (x[i].num & mask) << shift;
        }
        x[0] = Q(val);
        return;
    }
    case SWS_OP_SWAP_BYTES:
        switch (pixel_type_size(op->type)) {
        case 2:
            for (int i = 0; i < 4; i++)
                x[i].num = av_bswap16(x[i].num);
            break;
        case 4:
            for (int i = 0; i < 4; i++)
                x[i].num = av_bswap32(x[i].num);
            break;
        }
        return;
    case SWS_OP_CLEAR:
        for (int i = 0; i < 4; i++) {
            if (op->clear.value[i].den)
                x[i] = op->clear.value[i];
        }
        return;
    case SWS_OP_LSHIFT: {
        AVRational mult = Q(1 << op->shift.amount);
        for (int i = 0; i < 4; i++)
            x[i] = x[i].den ? av_mul_q(x[i], mult) : x[i];
        return;
    }
    case SWS_OP_RSHIFT: {
        AVRational mult = Q(1 << op->shift.amount);
        for (int i = 0; i < 4; i++)
            x[i] = x[i].den ? av_div_q(x[i], mult) : x[i];
        return;
    }
    case SWS_OP_SWIZZLE: {
        const AVRational orig[4] = { x[0], x[1], x[2], x[3] };
        for (int i = 0; i < 4; i++)
            x[i] = orig[op->swizzle.in[i]];
        return;
    }
    case SWS_OP_CONVERT:
        if (pixel_type_is_int(op->convert.to)) {
            const AVRational scale = expand_factor(op->type, op->convert.to);
            for (int i = 0; i < 4; i++) {
                x[i] = x[i].den ? Q(x[i].num / x[i].den) : x[i];
                if (op->convert.expand)
                    x[i] = av_mul_q(x[i], scale);
            }
        }
        return;
    case SWS_OP_DITHER:
        for (int i = 0; i < 4; i++)
            x[i] = x[i].den ? av_add_q(x[i], av_make_q(1, 2)) : x[i];
        return;
    case SWS_OP_CLAMP:
        for (int i = 0; i < 4; i++)
            x[i] = av_clip_q(x[i], Q(0), op->clamp.max[i]);
        return;
    case SWS_OP_LINEAR: {
        const AVRational orig[4] = { x[0], x[1], x[2], x[3] };
        for (int i = 0; i < 4; i++) {
            AVRational sum = op->lin.m[i][4];
            for (int j = 0; j < 4; j++)
                sum = av_add_q(sum, av_mul_q(orig[j], op->lin.m[i][j]));
            x[i] = sum;
        }
        return;
    }
    case SWS_OP_SCALE:
        for (int i = 0; i < 4; i++)
            x[i] = x[i].den ? av_mul_q(x[i], op->scale.factor) : x[i];
        return;
    }

    av_assert0(!"Invalid operation type!");
}

int ff_sws_op_match(const SwsOp *op, const SwsOp *ref, const SwsComps next)
{
    int score = 10;
    if (op->op != ref->op || op->type != ref->type)
        return 0;

    for (int i = 0; i < 4; i++) {
        if (ref->comps.unused[i]) {
            if (op->comps.unused[i])
                score += 1; /* Operating on fewer components is better .. */
            else
                return false; /* .. but not too few! */
        }

        if (ref->comps.flags[i]) {
            if (ref->comps.flags[i] & ~op->comps.flags[i]) {
                return false; /* Missing required output assumptions */
            } else {
                /* Implementation is more specialized */
                score += av_popcount(ref->comps.flags[i]);
            }
        }
    }

    switch (op->op) {
    case SWS_OP_INVALID:
        return 0;
    case SWS_OP_READ:
    case SWS_OP_WRITE:
        if (op->rw.elems  != ref->rw.elems  ||
            op->rw.planar != ref->rw.planar ||
            op->rw.frac   != ref->rw.frac)
            return 0;
        return score;
    case SWS_OP_SWAP_BYTES:
        return score;
    case SWS_OP_PACK:
    case SWS_OP_UNPACK:
        if (op->pack.type != ref->pack.type)
            return 0;
        for (int i = 0; i < 4; i++) {
            if (!op->pack.pattern[i]) /* allow ignoring unused extra components */
                break;
            if (op->pack.pattern[i] != ref->pack.pattern[i])
                return 0;
        }
        return score;
    case SWS_OP_CLEAR:
        /* Ensure that all needed components are actually cleared */
        for (int i = 0; i < 4; i++) {
            if (!op->clear.value[i].den)
                continue;
            if (!ref->comps.unused[i])
                return 0;
            if (ref->clear.value[i].den) {
                if (!av_cmp_q(op->clear.value[i], ref->clear.value[i]))
                    score += 4; /* Clearing with constant value */
                else
                    return 0;
            }
        }
        return score;
    case SWS_OP_LSHIFT:
    case SWS_OP_RSHIFT:
        if (ref->shift.amount && op->shift.amount != ref->shift.amount)
            return 0;
        return score;
    case SWS_OP_SWIZZLE:
        for (int i = 0; i < 4; i++) {
            if (op->swizzle.in[i] != ref->swizzle.in[i] && !next.unused[i])
                return 0;
        }
        return score;
    case SWS_OP_CONVERT:
        if (op->convert.to     != ref->convert.to ||
            op->convert.expand != ref->convert.expand)
            return 0;
        return score;
    case SWS_OP_DITHER:
        if (op->dither.size_log2 != ref->dither.size_log2)
            return 0;
        return score;
    case SWS_OP_CLAMP:
        return score;
    case SWS_OP_LINEAR:
        /* All required elements must be present */
        if (op->lin.mask & ~ref->lin.mask)
            return 0;
        /* To avoid operating on possibly undefined memory, filter out
         * implementations that operate on more input components */
        for (int i = 0; i < 4; i++) {
            if ((ref->lin.mask & SWS_MASK_COL(i)) && op->comps.unused[i])
                return 0;
        }
        return score;
    case SWS_OP_SCALE:
        return score;
    case SWS_OP_TYPE_NB:
        break;
    }

    av_assert0(!"Invalid operation type!");
    return 0;
}

static const char *print_q(const AVRational q, char buf[], int buf_len)
{
    if (!q.den) {
        switch (q.num) {
        case  1: return "inf";
        case -1: return "-inf";
        default: return "nan";
        }
    }

    if (q.den == 1) {
        snprintf(buf, buf_len, "%d", q.num);
        return buf;
    }

    if (abs(q.num) > 1000 || abs(q.den) > 1000) {
        snprintf(buf, buf_len, "%f", av_q2d(q));
        return buf;
    }

    snprintf(buf, buf_len, "%d/%d", q.num, q.den);
    return buf;
}

#define PRINTQ(q) print_q(q, (char[32]){0}, sizeof(char[32]) - 1)

void ff_sws_op_uninit(SwsOp *op)
{
    switch (op->op) {
    case SWS_OP_DITHER:
        av_free(op->dither.matrix);
        break;
    }

    *op = (SwsOp) {0};
}

SwsOpList *ff_sws_op_list_alloc(void)
{
    return av_mallocz(sizeof(SwsOpList));
}

void ff_sws_op_list_free(SwsOpList **p_ops)
{
    SwsOpList *ops = *p_ops;
    if (!ops)
        return;

    for (int i = 0; i < ops->num_ops; i++)
        ff_sws_op_uninit(&ops->ops[i]);

    av_freep(&ops->ops);
    av_free(ops);
    *p_ops = NULL;
}

void ff_sws_op_list_remove_at(SwsOpList *ops, int index, int count)
{
    const int end = ops->num_ops - count;
    av_assert2(index >= 0 && count >= 0 && index + count <= ops->num_ops);
    for (int i = index; i < end; i++)
        ops->ops[i] = ops->ops[i + count];
    ops->num_ops = end;
}

int ff_sws_op_list_insert_at(SwsOpList *ops, int index, SwsOp *op)
{
    void *ret;
    ret = av_dynarray2_add((void **) &ops->ops, &ops->num_ops, sizeof(*op),
                           (const void *) op);
    if (!ret) {
        ff_sws_op_uninit(op);
        return AVERROR(ENOMEM);
    }

    for (int i = ops->num_ops - 1; i > index; i--)
        ops->ops[i] = ops->ops[i - 1];
    ops->ops[index] = *op;
    *op = (SwsOp) {0};
    return 0;
}

int ff_sws_op_list_append(SwsOpList *ops, SwsOp *op)
{
    return ff_sws_op_list_insert_at(ops, ops->num_ops, op);
}

uint32_t ff_sws_linear_mask(const SwsLinearOp c)
{
    uint32_t mask = 0;
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 5; j++) {
            if (av_cmp_q(c.m[i][j], Q(i == j)))
                mask |= SWS_MASK(i, j);
        }
    }
    return mask;
}

static const char *describe_lin_mask(uint32_t mask)
{
    /* Try to be fairly descriptive without assuming too much */
    static const struct {
        const char *name;
        uint32_t mask;
    } patterns[] = {
        { "noop",               0 },
        { "luma",               SWS_MASK_LUMA },
        { "alpha",              SWS_MASK_ALPHA },
        { "luma+alpha",         SWS_MASK_LUMA | SWS_MASK_ALPHA },
        { "dot3",               0b111 },
        { "dot4",               0b1111 },
        { "row0",               SWS_MASK_ROW(0) },
        { "row0+alpha",         SWS_MASK_ROW(0) | SWS_MASK_ALPHA },
        { "col0",               SWS_MASK_COL(0) },
        { "col0+off3",          SWS_MASK_COL(0) | SWS_MASK_OFF3 },
        { "off3",               SWS_MASK_OFF3 },
        { "off3+alpha",         SWS_MASK_OFF3 | SWS_MASK_ALPHA },
        { "diag3",              SWS_MASK_DIAG3 },
        { "diag4",              SWS_MASK_DIAG4 },
        { "diag3+alpha",        SWS_MASK_DIAG3 | SWS_MASK_ALPHA },
        { "diag3+off3",         SWS_MASK_DIAG3 | SWS_MASK_OFF3 },
        { "diag3+off3+alpha",   SWS_MASK_DIAG3 | SWS_MASK_OFF3 | SWS_MASK_ALPHA },
        { "diag4+off4",         SWS_MASK_DIAG4 | SWS_MASK_OFF4 },
        { "matrix3",            SWS_MASK_MAT3 },
        { "matrix3+off3",       SWS_MASK_MAT3 | SWS_MASK_OFF3 },
        { "matrix3+off3+alpha", SWS_MASK_MAT3 | SWS_MASK_OFF3 | SWS_MASK_ALPHA },
        { "matrix4",            SWS_MASK_MAT4 },
        { "matrix4+off4",       SWS_MASK_MAT4 | SWS_MASK_OFF4 },
    };

    for (int i = 0; i < FF_ARRAY_ELEMS(patterns); i++) {
        if (!(mask & ~patterns[i].mask))
            return patterns[i].name;
    }

    return "full";
}

static char describe_comp_flags(unsigned flags)
{
    if (flags & SWS_COMP_GARBAGE)
        return 'X';
    else if (flags & SWS_COMP_ZERO)
        return '0';
    else if (flags & SWS_COMP_EXACT)
        return '+';
    else
        return '.';
}

/* merge_comp_flags() forms a monoid with flags_identity as the null element */
static const unsigned flags_identity = SWS_COMP_ZERO | SWS_COMP_EXACT;
static unsigned merge_comp_flags(unsigned a, unsigned b)
{
    const unsigned flags_or  = SWS_COMP_GARBAGE;
    const unsigned flags_and = SWS_COMP_ZERO | SWS_COMP_EXACT;
    return ((a & b) & flags_and) | ((a | b) & flags_or);
}

void ff_sws_op_list_print(void *log, int lev, const SwsOpList *ops)
{
    if (!ops->num_ops) {
        av_log(log, lev, "  (empty)\n");
        return;
    }

    for (int i = 0; i < ops->num_ops; i++) {
        const SwsOp *op = &ops->ops[i];
        av_log(log, lev, "  [%3s %c%c%c%c -> %c%c%c%c] ",
               pixel_type_name(op->type),
               op->comps.unused[0] ? 'X' : '.',
               op->comps.unused[1] ? 'X' : '.',
               op->comps.unused[2] ? 'X' : '.',
               op->comps.unused[3] ? 'X' : '.',
               describe_comp_flags(op->comps.flags[0]),
               describe_comp_flags(op->comps.flags[1]),
               describe_comp_flags(op->comps.flags[2]),
               describe_comp_flags(op->comps.flags[3]));

        switch (op->op) {
        case SWS_OP_INVALID:
            av_log(log, lev, "SWS_OP_INVALID\n");
            break;
        case SWS_OP_READ:
        case SWS_OP_WRITE:
            av_log(log, lev, "%-20s: %d elem(s) %s >> %d\n",
                   op->op == SWS_OP_READ ? "SWS_OP_READ"
                                         : "SWS_OP_WRITE",
                   op->rw.elems,  op->rw.planar ? "planar" : "packed",
                   op->rw.frac);
            break;
        case SWS_OP_SWAP_BYTES:
            av_log(log, lev, "SWS_OP_SWAP_BYTES\n");
            break;
        case SWS_OP_LSHIFT:
            av_log(log, lev, "%-20s: << %u\n", "SWS_OP_LSHIFT", op->shift.amount);
            break;
        case SWS_OP_RSHIFT:
            av_log(log, lev, "%-20s: >> %u\n", "SWS_OP_RSHIFT", op->shift.amount);
            break;
        case SWS_OP_PACK:
        case SWS_OP_UNPACK:
            av_log(log, lev, "%-20s: {%d %d %d %d} in %s\n",
                   op->op == SWS_OP_PACK ? "SWS_OP_PACK"
                                         : "SWS_OP_UNPACK",
                   op->pack.pattern[0], op->pack.pattern[1],
                   op->pack.pattern[2], op->pack.pattern[3],
                   pixel_type_name(op->pack.type));
            break;
        case SWS_OP_CLEAR:
            av_log(log, lev, "%-20s: {%s %s %s %s}\n", "SWS_OP_CLEAR",
                   op->clear.value[0].den ? PRINTQ(op->clear.value[0]) : "_",
                   op->clear.value[1].den ? PRINTQ(op->clear.value[1]) : "_",
                   op->clear.value[2].den ? PRINTQ(op->clear.value[2]) : "_",
                   op->clear.value[3].den ? PRINTQ(op->clear.value[3]) : "_");
            break;
        case SWS_OP_SWIZZLE:
            av_log(log, lev, "%-20s: %d%d%d%d\n", "SWS_OP_SWIZZLE",
                   op->swizzle.x, op->swizzle.y, op->swizzle.z, op->swizzle.w);
            break;
        case SWS_OP_CONVERT:
            av_log(log, lev, "%-20s: %s -> %s%s\n", "SWS_OP_CONVERT",
                   pixel_type_name(op->type), pixel_type_name(op->convert.to),
                   op->convert.expand ? " (expand)" : "");
            break;
        case SWS_OP_DITHER:
            av_log(log, lev, "%-20s: %dx%d matrix\n", "SWS_OP_DITHER",
                    1 << op->dither.size_log2, 1 << op->dither.size_log2);
            break;
        case SWS_OP_CLAMP:
            av_log(log, lev, "%-20s: 0 <= x <= {%s %s %s %s}\n", "SWS_OP_CLAMP",
                    op->clamp.max[0].den ? PRINTQ(op->clamp.max[0]) : "_",
                    op->clamp.max[1].den ? PRINTQ(op->clamp.max[1]) : "_",
                    op->clamp.max[2].den ? PRINTQ(op->clamp.max[2]) : "_",
                    op->clamp.max[3].den ? PRINTQ(op->clamp.max[3]) : "_");
            break;
        case SWS_OP_LINEAR:
            av_log(log, lev, "%-20s: %s [[%s %s %s %s %s] "
                                        "[%s %s %s %s %s] "
                                        "[%s %s %s %s %s] "
                                        "[%s %s %s %s %s]]\n",
                   "SWS_OP_LINEAR", describe_lin_mask(op->lin.mask),
                   PRINTQ(op->lin.m[0][0]), PRINTQ(op->lin.m[0][1]), PRINTQ(op->lin.m[0][2]), PRINTQ(op->lin.m[0][3]), PRINTQ(op->lin.m[0][4]),
                   PRINTQ(op->lin.m[1][0]), PRINTQ(op->lin.m[1][1]), PRINTQ(op->lin.m[1][2]), PRINTQ(op->lin.m[1][3]), PRINTQ(op->lin.m[1][4]),
                   PRINTQ(op->lin.m[2][0]), PRINTQ(op->lin.m[2][1]), PRINTQ(op->lin.m[2][2]), PRINTQ(op->lin.m[2][3]), PRINTQ(op->lin.m[2][4]),
                   PRINTQ(op->lin.m[3][0]), PRINTQ(op->lin.m[3][1]), PRINTQ(op->lin.m[3][2]), PRINTQ(op->lin.m[3][3]), PRINTQ(op->lin.m[3][4]));
            break;
        case SWS_OP_SCALE:
            av_log(log, lev, "%-20s: * %s\n", "SWS_OP_SCALE",
                   PRINTQ(op->scale.factor));
            break;
        case SWS_OP_TYPE_NB:
            break;
        }
    }

    av_log(log, lev, "    (X = unused, + = exact, 0 = zero)\n");
}

/* Infer + propagate known information about components */
static int op_list_update_comps(SwsOpList *ops)
{
    SwsComps next = { .unused = {true, true, true, true} };
    SwsComps prev = { .flags = {
        SWS_COMP_GARBAGE, SWS_COMP_GARBAGE, SWS_COMP_GARBAGE, SWS_COMP_GARBAGE,
    }};

    /* Forwards pass, propagates knowledge about the incoming pixel values */
    for (int n = 0; n < ops->num_ops; n++) {
        SwsOp *op = &ops->ops[n];

        switch (op->op) {
        case SWS_OP_READ:
            for (int i = 0; i < op->rw.elems; i++) {
                if (pixel_type_is_int(op->type))
                    op->comps.flags[i] |= SWS_COMP_EXACT;
            }
            for (int i = op->rw.elems; i < 4; i++)
                op->comps.flags[i] |= SWS_COMP_ZERO | SWS_COMP_EXACT;
            break;
        case SWS_OP_WRITE:
            for (int i = 0; i < op->rw.elems; i++)
                av_assert1(!(prev.flags[i] & SWS_COMP_GARBAGE));
            /* fall through */
        case SWS_OP_SWAP_BYTES:
        case SWS_OP_LSHIFT:
        case SWS_OP_RSHIFT:
        case SWS_OP_CLAMP:
            /* Linearly propagate flags per component */
            for (int i = 0; i < 4; i++)
                op->comps.flags[i] |= prev.flags[i];
            break;
        case SWS_OP_DITHER:
            /* Strip zero flag because of the nonzero dithering offset */
            for (int i = 0; i < 4; i++)
                op->comps.flags[i] |= prev.flags[i] & ~SWS_COMP_ZERO;
            break;
        case SWS_OP_UNPACK:
            for (int i = 0; i < 4; i++) {
                if (op->pack.pattern[i])
                    op->comps.flags[i] |= prev.flags[0];
                else
                    op->comps.flags[i] = SWS_COMP_GARBAGE;
            }
            break;
        case SWS_OP_PACK: {
            unsigned flags = flags_identity;
            for (int i = 0; i < 4; i++) {
                if (op->pack.pattern[i])
                    flags = merge_comp_flags(flags, prev.flags[i]);
                if (i > 0) /* clear remaining comps for sanity */
                    op->comps.flags[i] = SWS_COMP_GARBAGE;
            }
            op->comps.flags[0] |= flags;
            break;
        }
        case SWS_OP_CLEAR:
            for (int i = 0; i < 4; i++) {
                if (op->clear.value[i].den) {
                    if (op->clear.value[i].num == 0)
                        op->comps.flags[i] |= SWS_COMP_ZERO | SWS_COMP_EXACT;
                    if (op->clear.value[i].den == 1)
                        op->comps.flags[i] |= SWS_COMP_EXACT;
                }
                else
                    op->comps.flags[i] |= prev.flags[i];
            }
            break;
        case SWS_OP_SWIZZLE:
            for (int i = 0; i < 4; i++)
                op->comps.flags[i] |= prev.flags[op->swizzle.in[i]];
            break;
        case SWS_OP_CONVERT:
            for (int i = 0; i < 4; i++) {
                op->comps.flags[i] |= prev.flags[i];
                if (pixel_type_is_int(op->convert.to))
                    op->comps.flags[i] |= SWS_COMP_EXACT;
            }
            break;
        case SWS_OP_LINEAR:
            for (int i = 0; i < 4; i++) {
                unsigned flags = flags_identity;
                for (int j = 0; j < 4; j++) {
                    if (op->lin.m[i][j].num) {
                        flags = merge_comp_flags(flags, prev.flags[j]);
                        if (op->lin.m[i][j].den != 1) /* fractional coefficient */
                            flags &= ~SWS_COMP_EXACT;
                    }
                    if (op->lin.m[i][4].num) { /* nonzero offset */
                        flags &= ~SWS_COMP_ZERO;
                        if (op->lin.m[i][4].den != 1) /* fractional offset */
                            flags &= ~SWS_COMP_EXACT;
                    }
                }
                op->comps.flags[i] |= flags;
            }
            break;
        case SWS_OP_SCALE:
            for (int i = 0; i < 4; i++) {
                op->comps.flags[i] |= prev.flags[i];
                if (op->scale.factor.den != 1) /* fractional scale */
                    op->comps.flags[i] &= ~SWS_COMP_EXACT;
            }
            break;

        case SWS_OP_INVALID:
        case SWS_OP_TYPE_NB:
            return AVERROR(EINVAL);
        }

        prev = op->comps;
    }

    /* Backwards pass, solves for component dependencies */
    for (int n = ops->num_ops - 1; n >= 0; n--) {
        SwsOp *op = &ops->ops[n];

        switch (op->op) {
        case SWS_OP_READ:
        case SWS_OP_WRITE:
            for (int i = 0; i < op->rw.elems; i++)
                op->comps.unused[i] = op->op == SWS_OP_READ;
            for (int i = op->rw.elems; i < 4; i++)
                op->comps.unused[i] |= next.unused[i];
            break;
        case SWS_OP_SWAP_BYTES:
        case SWS_OP_LSHIFT:
        case SWS_OP_RSHIFT:
        case SWS_OP_CONVERT:
        case SWS_OP_DITHER:
        case SWS_OP_CLAMP:
        case SWS_OP_SCALE:
            for (int i = 0; i < 4; i++)
                op->comps.unused[i] |= next.unused[i];
            break;
        case SWS_OP_UNPACK: {
            bool unused = true;
            for (int i = 0; i < 4; i++) {
                if (op->pack.pattern[i])
                    unused &= next.unused[i];
                op->comps.unused[i] |= i > 0;
            }
            op->comps.unused[0] = unused;
            break;
        }
        case SWS_OP_PACK:
            for (int i = 0; i < 4; i++) {
                if (op->pack.pattern[i])
                    op->comps.unused[i] |= next.unused[0];
                else
                    op->comps.unused[i] = true;
            }
            break;
        case SWS_OP_CLEAR:
            for (int i = 0; i < 4; i++) {
                if (op->clear.value[i].den)
                    op->comps.unused[i] = true;
                else
                    op->comps.unused[i] |= next.unused[i];
            }
            break;
        case SWS_OP_SWIZZLE: {
            bool unused[4] = { true, true, true, true };
            for (int i = 0; i < 4; i++)
                unused[op->swizzle.in[i]] &= next.unused[i];
            for (int i = 0; i < 4; i++)
                op->comps.unused[i] = unused[i];
            break;
        }
        case SWS_OP_LINEAR:
            for (int j = 0; j < 4; j++) {
                bool unused = true;
                for (int i = 0; i < 4; i++) {
                    if (op->lin.m[i][j].num)
                        unused &= next.unused[i];
                }
                op->comps.unused[j] = unused;
            }
            break;
        }

        next = op->comps;
    }

    return 0;
}

/* Bit mask of active components */
typedef uint8_t comps_t;

/* returns log2(x) only if x is a power of two, or 0 otherwise */
static int exact_log2(const int x)
{
    int p;
    if (x <= 0)
        return 0;
    p = av_log2(x);
    return (1 << p) == x ? p : 0;
}

static int exact_log2_q(const AVRational x)
{
    if (x.den == 1)
        return exact_log2(x.num);
    else if (x.num == 1)
        return -exact_log2(x.den);
    else
        return 0;
}

/**
 * If a linear operation can be reduced to a scalar multiplication, returns
 * the corresponding scaling factor, or 0 otherwise.
 */
static bool extract_scalar(const SwsLinearOp *c, SwsComps prev, SwsComps next,
                           SwsScaleOp *out_scale)
{
    AVRational scale = {0};

    /* There are components not on the main diagonal */
    if (c->mask & ~SWS_MASK_DIAG4)
        return false;

    for (int i = 0; i < 4; i++) {
        const AVRational s = c->m[i][i];
        if ((prev.flags[i] & SWS_COMP_ZERO) || next.unused[i])
            continue;
        if (scale.den && av_cmp_q(s, scale))
            return false;
        scale = s;
    }

    if (scale.den)
        out_scale->factor = scale;
    return scale.den;
}

/* Extracts an integer clear operation (subset) from the given linear op. */
static bool extract_constant_rows(SwsLinearOp *c, SwsComps prev,
                                  SwsClearOp *out_clear)
{
    SwsClearOp clear = {0};
    bool ret = false;

    for (int i = 0; i < 4; i++) {
        bool const_row = c->m[i][4].den == 1; /* offset is integer */
        for (int j = 0; j < 4; j++) {
            const_row &= c->m[i][j].num == 0 || /* scalar is zero */
                         (prev.flags[j] & SWS_COMP_ZERO); /* input is zero */
        }
        if (const_row && (c->mask & SWS_MASK_ROW(i))) {
            clear.value[i] = c->m[i][4];
            for (int j = 0; j < 5; j++)
                c->m[i][j] = Q(i == j);
            c->mask &= ~SWS_MASK_ROW(i);
            ret = true;
        }
    }

    if (ret)
        *out_clear = clear;
    return ret;
}

/* Unswizzle a linear operation by aligning single-input rows with
 * their corresponding diagonal */
static bool extract_swizzle(SwsLinearOp *op, SwsComps prev, SwsSwizzleOp *out_swiz)
{
    SwsSwizzleOp swiz = SWS_SWIZZLE(0, 1, 2, 3);
    SwsLinearOp c = *op;

    for (int i = 0; i < 4; i++) {
        int idx = -1;
        for (int j = 0; j < 4; j++) {
            if (!c.m[i][j].num || (prev.flags[j] & SWS_COMP_ZERO))
                continue;
            if (idx >= 0)
                return false; /* multiple inputs */
            idx = j;
        }

        if (idx >= 0 && idx != i) {
            /* Move coefficient to the diagonal */
            c.m[i][i] = c.m[i][idx];
            c.m[i][idx] = Q(0);
            swiz.in[i] = idx;
        }
    }

    if (swiz.mask == SWS_SWIZZLE(0, 1, 2, 3).mask)
        return false; /* no swizzle was identified */

    c.mask = ff_sws_linear_mask(c);
    *out_swiz = swiz;
    *op = c;
    return true;
}

static void op_copy_flags(SwsOp *op, const SwsOp *op2)
{
    for (int i = 0; i < 4; i++)
        op->comps.flags[i] = op2->comps.flags[i];
}

/* Should only be used on ops that commute with each other, and only after
 * applying the necessary adjustments
 */
static void swap_ops(SwsOp *op, SwsOp *next)
{
    /* Clear all inferred flags */
    op->comps = next->comps = (SwsComps) {0};
    FFSWAP(SwsOp, *op, *next);
}

int ff_sws_op_list_optimize(SwsOpList *ops)
{
    int prev_num_ops, ret;
    bool progress;

    do {
        prev_num_ops = ops->num_ops;
        progress = false;

        RET(op_list_update_comps(ops));

        for (int n = 0; n < ops->num_ops;) {
            SwsOp dummy = {0};
            SwsOp *op = &ops->ops[n];
            SwsOp *prev = n ? &ops->ops[n - 1] : &dummy;
            SwsOp *next = n + 1 < ops->num_ops ? &ops->ops[n + 1] : &dummy;

            /* common helper variables */
            bool changed = false;
            bool noop = true;

            switch (op->op) {
            case SWS_OP_READ:
                /* Optimized further into refcopy / memcpy */
                if (next->op == SWS_OP_WRITE &&
                    next->rw.elems == op->rw.elems &&
                    next->rw.planar == op->rw.planar &&
                    next->rw.frac == op->rw.frac)
                {
                    ff_sws_op_list_remove_at(ops, n, 2);
                    av_assert1(ops->num_ops == 0);
                    return 0;
                }

                /* Skip reading extra unneeded components */
                if (op->rw.planar) {
                    int needed = op->rw.elems;
                    while (needed > 0 && next->comps.unused[needed - 1])
                        needed--;
                    if (op->rw.elems != needed) {
                        op->rw.elems = needed;
                        op->rw.planar &= op->rw.elems > 1;
                        progress = true;
                        continue;
                    }
                }
                break;

            case SWS_OP_SWAP_BYTES:
                /* Redundant (double) swap */
                if (next->op == SWS_OP_SWAP_BYTES) {
                    ff_sws_op_list_remove_at(ops, n, 2);
                    continue;
                }
                break;

            case SWS_OP_UNPACK:
                /* Redundant unpack+pack */
                if (next->op == SWS_OP_PACK && next->type == op->type &&
                    next->pack.pattern[0] == op->pack.pattern[0] &&
                    next->pack.pattern[1] == op->pack.pattern[1] &&
                    next->pack.pattern[2] == op->pack.pattern[2] &&
                    next->pack.pattern[3] == op->pack.pattern[3])
                {
                    ff_sws_op_list_remove_at(ops, n, 2);
                    continue;
                }

                /* Skip unpacking components that are not used */
                for (int i = 3; i > 0 && next->comps.unused[i]; i--)
                    op->pack.pattern[i] = 0;
                break;

            case SWS_OP_PACK:
                /* Skip packing known-to-be-zero components */
                for (int i = 3; i > 0; i--) {
                    if (!(prev->comps.flags[i] & SWS_COMP_ZERO))
                        break;
                    op->pack.pattern[i] = 0;
                }
                break;

            case SWS_OP_LSHIFT:
            case SWS_OP_RSHIFT:
                /* Two shifts in the same direction */
                if (next->op == op->op) {
                    op->shift.amount += next->shift.amount;
                    ff_sws_op_list_remove_at(ops, n + 1, 1);
                    continue;
                }

                /* No-op shift */
                if (!op->shift.amount) {
                    ff_sws_op_list_remove_at(ops, n, 1);
                    continue;
                }
                break;

            case SWS_OP_CLEAR:
                for (int i = 0; i < 4; i++) {
                    if (!op->clear.value[i].den)
                        continue;

                    if ((prev->comps.flags[i] & SWS_COMP_ZERO) &&
                        !(prev->comps.flags[i] & SWS_COMP_GARBAGE) &&
                        op->clear.value[i].num == 0)
                    {
                        /* Redundant clear-to-zero of zero component */
                        op->clear.value[i].den = 0;
                    } else if (next->comps.unused[i]) {
                        /* Unnecessary clear of unused component */
                        op->clear.value[i] = (AVRational) {0, 0};
                    } else if (op->clear.value[i].den) {
                        noop = false;
                    }
                }

                if (noop) {
                    ff_sws_op_list_remove_at(ops, n, 1);
                    continue;
                }

                /* Transitive clear */
                if (next->op == SWS_OP_CLEAR) {
                    for (int i = 0; i < 4; i++) {
                        if (next->clear.value[i].den)
                            op->clear.value[i] = next->clear.value[i];
                    }
                    ff_sws_op_list_remove_at(ops, n + 1, 1);
                    continue;
                }

                /* Prefer to clear as late as possible, to avoid doing
                 * redundant work */
                if ((op_type_is_independent(next->op) && next->op != SWS_OP_SWAP_BYTES) ||
                    next->op == SWS_OP_SWIZZLE)
                {
                    if (next->op == SWS_OP_CONVERT)
                        op->type = next->convert.to;
                    ff_sws_apply_op_q(next, op->clear.value);
                    swap_ops(op, next);
                    progress = true;
                    continue;
                }
                break;

            case SWS_OP_SWIZZLE:
                /* Identity swizzle */
                if (op->swizzle.mask == SWS_SWIZZLE(0, 1, 2, 3).mask) {
                    ff_sws_op_list_remove_at(ops, n, 1);
                    continue;
                }

                /* Transitive swizzle */
                if (next->op == SWS_OP_SWIZZLE) {
                    const SwsSwizzleOp orig = op->swizzle;
                    for (int i = 0; i < 4; i++)
                        op->swizzle.in[i] = orig.in[next->swizzle.in[i]];
                    op_copy_flags(op, next);
                    ff_sws_op_list_remove_at(ops, n + 1, 1);
                    continue;
                }

                /* Prefer swizzling on smaller element size */
                if (prev->op == SWS_OP_CONVERT &&
                    pixel_type_size(prev->type) < pixel_type_size(op->type))
                {
                    op->type = prev->type;
                    swap_ops(op, prev);
                    progress = true;
                    continue;
                }

                /* Otherwise, try to push swizzles towards the output */
                // TODO
                break;

            case SWS_OP_CONVERT:
                /* No-op conversion */
                if (op->type == op->convert.to) {
                    ff_sws_op_list_remove_at(ops, n, 1);
                    continue;
                }

                /* Transitive conversion */
                if (next->op == SWS_OP_CONVERT &&
                    op->convert.expand == next->convert.expand)
                {
                    av_assert1(op->convert.to == next->type);
                    op->convert.to = next->convert.to;
                    op_copy_flags(op, next);
                    ff_sws_op_list_remove_at(ops, n + 1, 1);
                    continue;
                }

                /* Conversion followed by integer expansion */
                if (next->op == SWS_OP_SCALE &&
                    !av_cmp_q(next->scale.factor, expand_factor(op->type, op->convert.to)))
                {
                    op->convert.expand = true;
                    ff_sws_op_list_remove_at(ops, n + 1, 1);
                    continue;
                }
                break;

            case SWS_OP_CLAMP:
                for (int i = 0; i < 4; i++) {
                    /* Redundant clamp on exact component */
                    if (prev->comps.flags[i] & SWS_COMP_EXACT)
                        op->clamp.max[i] = (AVRational) {0, 0};
                    /* Redundant clamp of unneeded component */
                    else if (next->comps.unused[i])
                        op->clamp.max[i] = (AVRational) {0, 0};
                    else if (op->clamp.max[i].den)
                        noop = false;
                }

                if (noop) {
                    ff_sws_op_list_remove_at(ops, n, 1);
                    continue;
                }
                break;

            case SWS_OP_DITHER:
                for (int i = 0; i < 4; i++) {
                    noop &= (prev->comps.flags[i] & SWS_COMP_EXACT) ||
                            next->comps.unused[i];
                }

                if (noop) {
                    ff_sws_op_list_remove_at(ops, n, 1);
                    continue;
                }
                break;

            case SWS_OP_LINEAR: {
                SwsSwizzleOp swizzle;
                SwsClearOp clear;
                SwsScaleOp scale;

                /* No-op (identity) linear operation */
                if (!op->lin.mask) {
                    ff_sws_op_list_remove_at(ops, n, 1);
                    continue;
                }

                if (next->op == SWS_OP_LINEAR) {
                    /* 5x5 matrix multiplication after appending [ 0 0 0 0 1 ] */
                    const SwsLinearOp m1 = op->lin;
                    const SwsLinearOp m2 = next->lin;
                    for (int i = 0; i < 4; i++) {
                        for (int j = 0; j < 5; j++) {
                            AVRational sum = Q(0);
                            for (int k = 0; k < 4; k++)
                                sum = av_add_q(sum, av_mul_q(m2.m[i][k], m1.m[k][j]));
                            if (j == 4) /* m1.m[4][j] == 1 */
                                sum = av_add_q(sum, m2.m[i][4]);
                            op->lin.m[i][j] = sum;
                        }
                    }
                    op_copy_flags(op, next);
                    op->lin.mask = ff_sws_linear_mask(op->lin);
                    ff_sws_op_list_remove_at(ops, n + 1, 1);
                    continue;
                }

                /* Optimize away zero columns */
                for (int j = 0; j < 4; j++) {
                    const uint32_t col = SWS_MASK_COL(j);
                    if (!(prev->comps.flags[j] & SWS_COMP_ZERO) || !(op->lin.mask & col))
                        continue;
                    for (int i = 0; i < 4; i++)
                        op->lin.m[i][j] = Q(i == j);
                    op->lin.mask &= ~col;
                    changed = true;
                }

                /* Optimize away unused rows */
                for (int i = 0; i < 4; i++) {
                    const uint32_t row = SWS_MASK_ROW(i);
                    if (!next->comps.unused[i] || !(op->lin.mask & row))
                        continue;
                    for (int j = 0; j < 5; j++)
                        op->lin.m[i][j] = Q(i == j);
                    op->lin.mask &= ~row;
                    changed = true;
                }

                if (changed) {
                    progress = true;
                    continue;
                }

                /* Convert constant rows to explicit clear instruction */
                if (extract_constant_rows(&op->lin, prev->comps, &clear)) {
                    RET(ff_sws_op_list_insert_at(ops, n + 1, &(SwsOp) {
                        .op    = SWS_OP_CLEAR,
                        .type  = op->type,
                        .comps = op->comps,
                        .clear = clear,
                    }));
                    continue;
                }

                /* Multiplication by scalar constant */
                if (extract_scalar(&op->lin, prev->comps, next->comps, &scale)) {
                    op->op = SWS_OP_SCALE;
                    op->scale = scale;
                    progress = true;
                    continue;
                }

                /* Swizzle by fixed pattern */
                if (extract_swizzle(&op->lin, prev->comps, &swizzle)) {
                    RET(ff_sws_op_list_insert_at(ops, n, &(SwsOp) {
                        .op      = SWS_OP_SWIZZLE,
                        .type    = op->type,
                        .swizzle = swizzle,
                    }));
                    continue;
                }
                break;
            }

            case SWS_OP_SCALE: {
                const int factor2 = exact_log2_q(op->scale.factor);

                /* No-op scaling */
                if (op->scale.factor.num == 1 && op->scale.factor.den == 1) {
                    ff_sws_op_list_remove_at(ops, n, 1);
                    continue;
                }

                /* Scaling by integer before conversion to int */
                if (op->scale.factor.den == 1 &&
                    next->op == SWS_OP_CONVERT && pixel_type_is_int(next->convert.to))
                {
                    op->type = next->convert.to;
                    swap_ops(op, next);
                    progress = true;
                    continue;
                }

                /* Scaling by exact power of two */
                if (factor2 && pixel_type_is_int(op->type)) {
                    op->op = factor2 > 0 ? SWS_OP_LSHIFT : SWS_OP_RSHIFT;
                    op->shift.amount = FFABS(factor2);
                    progress = true;
                    continue;
                }
                break;
            }
            }

            /* No optimization triggered, move on to next operation */
            n++;
        }
    } while (prev_num_ops != ops->num_ops || progress);

    return 0;
}

typedef struct SwsOpPass {
    int block_w;
    int block_h;
    int pixel_bits_in;
    int pixel_bits_out;

    SwsOpFunc read;
    SwsOpFunc read_n;
    SwsOpFunc write;
    SwsOpFunc write_n;
    int write_idx; /* idx of the write op func in ops[] */

#define SWS_MAX_OPS 16
    SwsOpImpl ops[SWS_MAX_OPS];
    void (*free_priv[SWS_MAX_OPS])(void *);
} SwsOpPass;

static void op_pass_reset(SwsOpPass *p)
{
    for (int i = 0; i < FF_ARRAY_ELEMS(p->ops); i++) {
        if (p->free_priv[i])
            p->free_priv[i]((void *) p->ops[i].priv);
    }

    *p = (SwsOpPass) {0};
}

static void op_pass_free(void *ptr)
{
    SwsOpPass *p = ptr;
    if (!p)
        return;

    op_pass_reset(p);
    av_free(p);
}

static av_const bool img_aligned(SwsImg img, const int alignment)
{
    if (!alignment)
        return true;

    for (int i = 0; i < 4; i++) {
        uintptr_t ptr = (uintptr_t) img.data[i];
        if (ptr & (alignment - 1))
            return false;
    }

    return true;
}

static void run_op_pass(const SwsImg *out_base, const SwsImg *in_base,
                        const int y_start, const int h, const SwsPass *pass)
{
    SwsOpPass *p = pass->priv;

    const int w          = pass->width;
    const int block_w    = p->block_w ? p->block_w : w;
    const int block_h    = p->block_h ? p->block_h : h;
    const int base_w     = p->block_w ? (w & ~(block_w - 1)) : w;
    const int base_h     = p->block_h ? (h & ~(block_h - 1)) : h;
    const int rest_w     = w - base_w;
    const int rest_h     = h - base_h;
    const int y_end_base = y_start + base_h;
    const int stride_in  = (block_w * p->pixel_bits_in)  >> 3;
    const int stride_out = (block_w * p->pixel_bits_out) >> 3;

    const bool aligned_in  = img_aligned(*in_base,  stride_in);
    const bool aligned_out = img_aligned(*out_base, stride_out);
    SwsOpFunc read, *write = &p->ops[p->write_idx].next;
    SwsOpExec exec;

    /* Block-aligned height */
    for (exec.y = y_start; exec.y < y_end_base; exec.y += block_h) {
        exec.in  = ff_sws_img_shift(*in_base,  exec.y);
        exec.out = ff_sws_img_shift(*out_base, exec.y);

        /* Block-aligned read/write */
        read   = aligned_in  ? p->read  : p->read_n;
        *write = aligned_out ? p->write : p->write_n;
        exec.block_w = block_w;
        exec.block_h = block_h;

        for (exec.x = 0; exec.x < base_w; exec.x += block_w) {
            read(&exec, p->ops);

            for (int i = 0; i < 4; i++) {
                exec.in.data[i]  += stride_in;
                exec.out.data[i] += stride_out;
            }
        }

        if (rest_w) {
            /* Always use unaligned path for the remainder */
            read   = p->read_n;
            *write = p->write_n;
            exec.block_w = rest_w;

            read(&exec, p->ops);
        }
    }

    /* Process the remaining lines using unaligned I/O functions */
    if (rest_h) {
        av_assert1(exec.y == y_end_base);
        exec.in  = ff_sws_img_shift(*in_base, y_end_base);
        exec.out = ff_sws_img_shift(*in_base, y_end_base);

        read   = p->read_n;
        *write = p->write_n;
        exec.block_w = block_w;
        exec.block_h = rest_h;

        for (exec.x = 0; exec.x < w; exec.x += block_w) {
            read(&exec, p->ops);

            for (int i = 0; i < 4; i++) {
                exec.in.data[i]  += stride_in;
                exec.out.data[i] += stride_out;
            }
        }

        if (rest_w) {
            exec.block_w = rest_w;
            read(&exec, p->ops);
        }
    }
}

static int rw_pixel_bits(const SwsOp op)
{
    const int elems = op.rw.planar ? 1 : op.rw.elems;
    const int size  = pixel_type_size(op.type);
    const int bits  = 8 >> op.rw.frac;
    av_assert1(bits >= 1);
    return elems * size * bits;
}

int ff_sws_compile_pass(SwsGraph *graph, SwsOpList *ops, int flags, SwsFormat dst,
                        SwsPass *input, SwsPass **output)
{
    SwsContext *ctx = graph->ctx;
    SwsCompileInfo info = {0};
    SwsOpPass *p = NULL;
    SwsPass *pass;
    int ret;

    if (ops->num_ops < 2) {
        av_log(ctx, AV_LOG_ERROR, "Need at least two operations.\n");
        return AVERROR(EINVAL);
    }

    if (ops->ops[0].op != SWS_OP_READ || ops->ops[ops->num_ops - 1].op != SWS_OP_WRITE) {
        av_log(ctx, AV_LOG_ERROR, "First and last operations must be a read "
               "and write, respectively.\n");
        return AVERROR(EINVAL);
    }

    if (flags & SWS_OP_FLAG_OPTIMIZE)
        RET(ff_sws_op_list_optimize(ops));
    else
        RET(op_list_update_comps(ops));

    info.read  = ops->ops[0];
    info.write = ops->ops[ops->num_ops - 1];
    for (int i = 0; i < ops->num_ops; i++) {
        info.pixel_size_max = FFMAX(info.pixel_size_max,
                                    pixel_type_size(ops->ops[i].type));
    }

    p = av_mallocz(sizeof(*p));
    if (!p)
        return AVERROR(ENOMEM);

    for (int n = 0; n < FF_ARRAY_ELEMS(sws_op_backends); n++) {
        const SwsOpBackend *backend = sws_op_backends[n];
        SwsOpList rest = *ops;

        p->pixel_bits_in  = rw_pixel_bits(info.read);
        p->pixel_bits_out = rw_pixel_bits(info.write);

        for (int idx_ops = 0; rest.num_ops; idx_ops++) {
            SwsCompiledOp comp;
            ret = backend->compile(&info, &rest, &comp);
            if (ret == AVERROR(ENOTSUP)) {
                av_log(ctx, AV_LOG_DEBUG, "Backend '%s' does not support operations:\n", backend->name);
                ff_sws_op_list_print(ctx, AV_LOG_DEBUG, &rest);
                goto next_backend;
            } else if (ret < 0) {
                av_log(ctx, AV_LOG_ERROR, "Failed to compile operations: %s\n", av_err2str(ret));
                ff_sws_op_list_print(ctx, AV_LOG_WARNING, &rest);
                goto next_backend;
            }

            if (comp.block_w) {
                av_assert1(!p->block_w || p->block_w == comp.block_w);
                p->block_w = comp.block_w;
            }

            if (comp.block_h) {
                av_assert1(!p->block_h || p->block_h == comp.block_h);
                p->block_h = comp.block_h;
            }

            av_assert0(idx_ops < FF_ARRAY_ELEMS(p->ops));
            p->ops[idx_ops].priv  = comp.priv;
            p->free_priv[idx_ops] = comp.free_priv;

            if (idx_ops == 0) {
                p->read        = comp.func;
                p->read_n      = comp.func_n;
            } else if (!rest.num_ops) {
                p->write       = comp.func;
                p->write_n     = comp.func_n;
                p->write_idx   = idx_ops - 1;
            } else {
                p->ops[idx_ops - 1].next = comp.func;
                av_assert1(!comp.func_n);
            }
        }

        pass = ff_sws_graph_add_pass(graph, dst.format, dst.width, dst.height, input,
                                     1, p, run_op_pass);
        if (!pass) {
            ret = AVERROR(ENOMEM);
            goto fail;
        }
        pass->free = op_pass_free;

        av_log(ctx, AV_LOG_VERBOSE, "Compiled using backend '%s'\n", backend->name);
        *output = pass;
        return 0;

next_backend:
        op_pass_reset(p);
        continue;
    }

    av_log(ctx, AV_LOG_WARNING, "No backend found for operations:\n");
    ff_sws_op_list_print(ctx, AV_LOG_WARNING, ops);
    return AVERROR(ENOTSUP);

fail:
    op_pass_free(p);
    return ret;
}

int ff_sws_op_compile(const SwsCompileInfo *info, const SwsOpTable *const tables[],
                      int num_tables, SwsOpList *ops, SwsCompiledOp *out_comp)
{
    static const SwsOp dummy = { .comps.unused = { true, true, true, true }};
    const SwsOp *next = ops->num_ops > 1 ? &ops->ops[1] : &dummy;
    const unsigned cpu_flags = av_get_cpu_flags();
    const SwsOpTable *best_table = NULL;
    const SwsOpEntry *best = NULL;
    const void *priv = NULL;
    SwsOp op = ops->ops[0];
    int best_score = 0;

    /* These operations can be replaced by integer operations with
     * no change in behavior, so convert the type before matching */
    switch (op.op) {
    case SWS_OP_READ:
    case SWS_OP_WRITE:
    case SWS_OP_SWAP_BYTES:
    case SWS_OP_SWIZZLE:
        switch (op.type) {
        case SWS_PIXEL_F32: op.type = SWS_PIXEL_U32; break;
        }
    }

    for (int n = 0; n < num_tables; n++) {
        const SwsOpTable *table = tables[n];
        if (table->cpu_flags & ~cpu_flags)
            continue;

        for (int i = 0; table->entries[i].op.op; i++) {
            const SwsOpEntry *entry = &table->entries[i];
            int score = ff_sws_op_match(&op, &entry->op, next->comps);
            if (score > best_score) {
                best_score = score;
                best_table = table;
                best = entry;
            }
        }
    }

    if (!best)
        return AVERROR(ENOTSUP);

    if (best->setup) {
        int ret = best->setup(&op, &priv);
        if (ret < 0)
            return ret;
    }

    *out_comp = (SwsCompiledOp) {
        .block_w    = best_table->block_w,
        .block_h    = best_table->block_h,
        .func       = best->func,
        .func_n     = best->func_n,
        .priv       = priv,
        .free_priv  = best->free,
    };

    ops->ops++;
    ops->num_ops--;
    return 0;
}
