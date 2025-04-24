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
#include "libavutil/refstruct.h"

#include "ops.h"
#include "ops_internal.h"

extern SwsOpBackend backend_x86;
extern SwsOpBackend backend_c;

const SwsOpBackend * const ff_sws_op_backends[] = {
#if ARCH_X86
    &backend_x86,
#endif
    &backend_c,
    NULL
};

const int ff_sws_num_op_backends = FF_ARRAY_ELEMS(ff_sws_op_backends) - 1;

#define Q(N) ((AVRational) { N, 1 })

#define RET(x)                                                                 \
    do {                                                                       \
        if ((ret = (x)) < 0)                                                   \
            return ret;                                                        \
    } while (0)

const char *ff_sws_pixel_type_name(SwsPixelType type)
{
    switch (type) {
    case SWS_PIXEL_U8:   return "u8";
    case SWS_PIXEL_U16:  return "u16";
    case SWS_PIXEL_U32:  return "u32";
    case SWS_PIXEL_F32:  return "f32";
    case SWS_PIXEL_NONE: return "none";
    case SWS_PIXEL_TYPE_NB: break;
    }

    av_assert0(!"Invalid pixel type!");
    return "ERR";
}

int ff_sws_pixel_type_size(SwsPixelType type)
{
    switch (type) {
    case SWS_PIXEL_U8:  return sizeof(uint8_t);
    case SWS_PIXEL_U16: return sizeof(uint16_t);
    case SWS_PIXEL_U32: return sizeof(uint32_t);
    case SWS_PIXEL_F32: return sizeof(float);
    case SWS_PIXEL_NONE: break;
    case SWS_PIXEL_TYPE_NB: break;
    }

    av_assert0(!"Invalid pixel type!");
    return 0;
}

bool ff_sws_pixel_type_is_int(SwsPixelType type)
{
    switch (type) {
    case SWS_PIXEL_U8:
    case SWS_PIXEL_U16:
    case SWS_PIXEL_U32:
        return true;
    case SWS_PIXEL_F32:
        return false;
    case SWS_PIXEL_NONE:
    case SWS_PIXEL_TYPE_NB: break;
    }

    av_assert0(!"Invalid pixel type!");
    return false;
}

SwsPixelType ff_sws_pixel_type_to_uint(SwsPixelType type)
{
    if (!type)
        return type;

    switch (ff_sws_pixel_type_size(type)) {
    case 8:  return SWS_PIXEL_U8;
    case 16: return SWS_PIXEL_U16;
    case 32: return SWS_PIXEL_U32;
    }

    av_assert0(!"Invalid pixel type!");
    return SWS_PIXEL_NONE;
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
    case SWS_OP_MIN:
    case SWS_OP_MAX:
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

/* biased towards `a` */
static AVRational av_min_q(AVRational a, AVRational b)
{
    return av_cmp_q(a, b) == 1 ? b : a;
}

static AVRational av_max_q(AVRational a, AVRational b)
{
    return av_cmp_q(a, b) == -1 ? b : a;
}

static AVRational expand_factor(SwsPixelType from, SwsPixelType to)
{
    const int src = ff_sws_pixel_type_size(from);
    const int dst = ff_sws_pixel_type_size(to);
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
        return;
    case SWS_OP_UNPACK: {
        unsigned val = x[0].num;
        int shift = ff_sws_pixel_type_size(op->type) * 8;
        for (int i = 0; i < 4; i++) {
            const unsigned mask = (1 << op->pack.pattern[i]) - 1;
            shift -= op->pack.pattern[i];
            x[i] = Q((val >> shift) & mask);
        }
        return;
    }
    case SWS_OP_PACK: {
        unsigned val = 0;
        int shift = ff_sws_pixel_type_size(op->type) * 8;
        for (int i = 0; i < 4; i++) {
            const unsigned mask = (1 << op->pack.pattern[i]) - 1;
            shift -= op->pack.pattern[i];
            val |= (x[i].num & mask) << shift;
        }
        x[0] = Q(val);
        return;
    }
    case SWS_OP_SWAP_BYTES:
        switch (ff_sws_pixel_type_size(op->type)) {
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
            if (op->c.q4[i].den)
                x[i] = op->c.q4[i];
        }
        return;
    case SWS_OP_LSHIFT: {
        AVRational mult = Q(1 << op->c.u);
        for (int i = 0; i < 4; i++)
            x[i] = x[i].den ? av_mul_q(x[i], mult) : x[i];
        return;
    }
    case SWS_OP_RSHIFT: {
        AVRational mult = Q(1 << op->c.u);
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
        if (ff_sws_pixel_type_is_int(op->convert.to)) {
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
    case SWS_OP_MIN:
        for (int i = 0; i < 4; i++)
            x[i] = av_min_q(x[i], op->c.q4[i]);
        return;
    case SWS_OP_MAX:
        for (int i = 0; i < 4; i++)
            x[i] = av_max_q(x[i], op->c.q4[i]);
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
            x[i] = x[i].den ? av_mul_q(x[i], op->c.q) : x[i];
        return;
    }

    av_assert0(!"Invalid operation type!");
}

/**
 * Match an operation against a reference operation. Returns a score for how
 * well the reference matches the operation, or 0 if there is no match.
 *
 * If `ref->comps` has any flags set, they must be set in `op` as well.
 * Likewise, if `ref->comps` has any components marked as unused, they must be
 * marked as as unused in `ops` as well.
 *
 * For SWS_OP_LINEAR, `ref->linear.mask` must be a strict superset of
 * `op->linear.mask`, but may not contain any columns explicitly ignored by
 * `op->comps.unused`.
 *
 * For SWS_OP_READ, SWS_OP_WRITE, SWS_OP_SWAP_BYTES and SWS_OP_SWIZZLE, the
 * exact type is not checked, just the size.
 *
 * Components set in `next.unused` are ignored when matching. If `flexible`
 * is true, the op body is ignored - only the operation, pixel type, and
 * component masks are checked.
 */
static int op_match(const SwsOp *op, const SwsOpEntry *entry, const SwsComps next)
{
    const SwsOp *ref = &entry->op;
    int score = 10;
    if (op->op != ref->op)
        return 0;

    switch (op->op) {
    case SWS_OP_READ:
    case SWS_OP_WRITE:
    case SWS_OP_SWAP_BYTES:
    case SWS_OP_SWIZZLE:
        /* Only the size matters for these operations */
        if (ff_sws_pixel_type_size(op->type) != ff_sws_pixel_type_size(ref->type))
            return 0;
        break;
    default:
        if (op->type != ref->type)
            return 0;
        break;
    }

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

    /* Flexible variants always match, but lower the score to prioritize more
     * specific implementations if they exist */
    if (entry->flexible)
        return score - 5;

    switch (op->op) {
    case SWS_OP_INVALID:
        return 0;
    case SWS_OP_READ:
    case SWS_OP_WRITE:
        if (op->rw.elems  != ref->rw.elems  ||
            op->rw.packed != ref->rw.packed ||
            op->rw.frac   != ref->rw.frac)
            return 0;
        return score;
    case SWS_OP_SWAP_BYTES:
        return score;
    case SWS_OP_PACK:
    case SWS_OP_UNPACK:
        for (int i = 0; i < 4 && op->pack.pattern[i]; i++) {
            if (op->pack.pattern[i] != ref->pack.pattern[i])
                return 0;
        }
        return score;
    case SWS_OP_CLEAR:
        for (int i = 0; i < 4; i++) {
            if (!op->c.q4[i].den)
                continue;
            if (av_cmp_q(op->c.q4[i], ref->c.q4[i]) && !next.unused[i])
                return 0;
        }
        return score;
    case SWS_OP_LSHIFT:
    case SWS_OP_RSHIFT:
        return op->c.u == ref->c.u ? score : 0;
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
        return op->dither.size_log2 == ref->dither.size_log2 ? score : 0;
    case SWS_OP_MIN:
    case SWS_OP_MAX:
        for (int i = 0; i < 4; i++) {
            if (av_cmp_q(op->c.q4[i], ref->c.q4[i]) && !next.unused[i])
                return 0;
        }
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
        /* Prioritize smaller implementations */
        score += av_popcount(SWS_MASK_ALL ^ ref->lin.mask);
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
        av_refstruct_unref(&op->dither.matrix);
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

SwsOpList *ff_sws_op_list_duplicate(const SwsOpList *ops)
{
    SwsOpList *copy = av_malloc(sizeof(*copy));
    if (!copy)
        return NULL;

    *copy = *ops;
    copy->ops = av_memdup(ops->ops, ops->num_ops * sizeof(ops->ops[0]));
    if (!copy->ops) {
        av_free(copy);
        return NULL;
    }

    for (int i = 0; i < ops->num_ops; i++) {
        const SwsOp *op = &ops->ops[i];
        switch (op->op) {
        case SWS_OP_DITHER:
            av_refstruct_ref(copy->ops[i].dither.matrix);
            break;
        }
    }

    return copy;
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

int ff_sws_op_list_max_size(const SwsOpList *ops)
{
    int max_size = 0;
    for (int i = 0; i < ops->num_ops; i++) {
        const int size = ff_sws_pixel_type_size(ops->ops[i].type);
        max_size = FFMAX(max_size, size);
    }

    return max_size;
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
               ff_sws_pixel_type_name(op->type),
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
                   op->rw.elems,  op->rw.packed ? "packed" : "planar",
                   op->rw.frac);
            break;
        case SWS_OP_SWAP_BYTES:
            av_log(log, lev, "SWS_OP_SWAP_BYTES\n");
            break;
        case SWS_OP_LSHIFT:
            av_log(log, lev, "%-20s: << %u\n", "SWS_OP_LSHIFT", op->c.u);
            break;
        case SWS_OP_RSHIFT:
            av_log(log, lev, "%-20s: >> %u\n", "SWS_OP_RSHIFT", op->c.u);
            break;
        case SWS_OP_PACK:
        case SWS_OP_UNPACK:
            av_log(log, lev, "%-20s: {%d %d %d %d}\n",
                   op->op == SWS_OP_PACK ? "SWS_OP_PACK"
                                         : "SWS_OP_UNPACK",
                   op->pack.pattern[0], op->pack.pattern[1],
                   op->pack.pattern[2], op->pack.pattern[3]);
            break;
        case SWS_OP_CLEAR:
            av_log(log, lev, "%-20s: {%s %s %s %s}\n", "SWS_OP_CLEAR",
                   op->c.q4[0].den ? PRINTQ(op->c.q4[0]) : "_",
                   op->c.q4[1].den ? PRINTQ(op->c.q4[1]) : "_",
                   op->c.q4[2].den ? PRINTQ(op->c.q4[2]) : "_",
                   op->c.q4[3].den ? PRINTQ(op->c.q4[3]) : "_");
            break;
        case SWS_OP_SWIZZLE:
            av_log(log, lev, "%-20s: %d%d%d%d\n", "SWS_OP_SWIZZLE",
                   op->swizzle.x, op->swizzle.y, op->swizzle.z, op->swizzle.w);
            break;
        case SWS_OP_CONVERT:
            av_log(log, lev, "%-20s: %s -> %s%s\n", "SWS_OP_CONVERT",
                   ff_sws_pixel_type_name(op->type),
                   ff_sws_pixel_type_name(op->convert.to),
                   op->convert.expand ? " (expand)" : "");
            break;
        case SWS_OP_DITHER:
            av_log(log, lev, "%-20s: %dx%d matrix\n", "SWS_OP_DITHER",
                    1 << op->dither.size_log2, 1 << op->dither.size_log2);
            break;
        case SWS_OP_MIN:
            av_log(log, lev, "%-20s: x <= {%s %s %s %s}\n", "SWS_OP_MIN",
                    op->c.q4[0].den ? PRINTQ(op->c.q4[0]) : "_",
                    op->c.q4[1].den ? PRINTQ(op->c.q4[1]) : "_",
                    op->c.q4[2].den ? PRINTQ(op->c.q4[2]) : "_",
                    op->c.q4[3].den ? PRINTQ(op->c.q4[3]) : "_");
            break;
        case SWS_OP_MAX:
            av_log(log, lev, "%-20s: {%s %s %s %s} <= x\n", "SWS_OP_MAX",
                    op->c.q4[0].den ? PRINTQ(op->c.q4[0]) : "_",
                    op->c.q4[1].den ? PRINTQ(op->c.q4[1]) : "_",
                    op->c.q4[2].den ? PRINTQ(op->c.q4[2]) : "_",
                    op->c.q4[3].den ? PRINTQ(op->c.q4[3]) : "_");
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
                   PRINTQ(op->c.q));
            break;
        case SWS_OP_TYPE_NB:
            break;
        }

        if (op->comps.min[0].den || op->comps.min[1].den ||
            op->comps.min[2].den || op->comps.min[3].den ||
            op->comps.max[0].den || op->comps.max[1].den ||
            op->comps.max[2].den || op->comps.max[3].den)
        {
            av_log(log, AV_LOG_TRACE, "    min: {%s, %s, %s, %s}, max: {%s, %s, %s, %s}\n",
                PRINTQ(op->comps.min[0]), PRINTQ(op->comps.min[1]),
                PRINTQ(op->comps.min[2]), PRINTQ(op->comps.min[3]),
                PRINTQ(op->comps.max[0]), PRINTQ(op->comps.max[1]),
                PRINTQ(op->comps.max[2]), PRINTQ(op->comps.max[3]));
        }

    }

    av_log(log, lev, "    (X = unused, + = exact, 0 = zero)\n");
}

/* Infer + propagate known information about components */
static void op_list_update_comps(SwsOpList *ops)
{
    SwsComps next = { .unused = {true, true, true, true} };
    SwsComps prev = { .flags = {
        SWS_COMP_GARBAGE, SWS_COMP_GARBAGE, SWS_COMP_GARBAGE, SWS_COMP_GARBAGE,
    }};

    /* Forwards pass, propagates knowledge about the incoming pixel values */
    for (int n = 0; n < ops->num_ops; n++) {
        SwsOp *op = &ops->ops[n];

        /* Prefill min/max values automatically; may have to be fixed in
         * special cases */
        memcpy(op->comps.min, prev.min, sizeof(prev.min));
        memcpy(op->comps.max, prev.max, sizeof(prev.max));
        ff_sws_apply_op_q(op, op->comps.min);
        ff_sws_apply_op_q(op, op->comps.max);

        switch (op->op) {
        case SWS_OP_READ:
            for (int i = 0; i < op->rw.elems; i++) {
                if (ff_sws_pixel_type_is_int(op->type)) {
                    const int size = ff_sws_pixel_type_size(op->type);
                    const uint64_t max_val = (1 << 8 * size) - 1;
                    op->comps.flags[i] |= SWS_COMP_EXACT;
                    op->comps.min[i] = Q(0);
                    op->comps.max[i] = Q(max_val);
                }
            }
            for (int i = op->rw.elems; i < 4; i++)
                op->comps.flags[i] |= prev.flags[i];
            break;
        case SWS_OP_WRITE:
            for (int i = 0; i < op->rw.elems; i++)
                av_assert1(!(prev.flags[i] & SWS_COMP_GARBAGE));
            /* fall through */
        case SWS_OP_SWAP_BYTES:
        case SWS_OP_LSHIFT:
        case SWS_OP_RSHIFT:
        case SWS_OP_MIN:
        case SWS_OP_MAX:
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
                if (op->c.q4[i].den) {
                    if (op->c.q4[i].num == 0)
                        op->comps.flags[i] |= SWS_COMP_ZERO | SWS_COMP_EXACT;
                    if (op->c.q4[i].den == 1)
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
                if (ff_sws_pixel_type_is_int(op->convert.to))
                    op->comps.flags[i] |= SWS_COMP_EXACT;
            }
            break;
        case SWS_OP_LINEAR:
            for (int i = 0; i < 4; i++) {
                unsigned flags = flags_identity;
                AVRational min = Q(0), max = Q(0);
                for (int j = 0; j < 4; j++) {
                    const AVRational k = op->lin.m[i][j];
                    AVRational mink = av_mul_q(prev.min[j], k);
                    AVRational maxk = av_mul_q(prev.max[j], k);
                    if (k.num) {
                        flags = merge_comp_flags(flags, prev.flags[j]);
                        if (k.den != 1) /* fractional coefficient */
                            flags &= ~SWS_COMP_EXACT;
                        if (k.num < 0)
                            FFSWAP(AVRational, mink, maxk);
                        min = av_add_q(min, mink);
                        max = av_add_q(max, maxk);
                    }
                }
                if (op->lin.m[i][4].num) { /* nonzero offset */
                    flags &= ~SWS_COMP_ZERO;
                    if (op->lin.m[i][4].den != 1) /* fractional offset */
                        flags &= ~SWS_COMP_EXACT;
                    min = av_add_q(min, op->lin.m[i][4]);
                    max = av_add_q(max, op->lin.m[i][4]);
                }
                op->comps.flags[i] |= flags;
                op->comps.min[i] = min;
                op->comps.max[i] = max;
            }
            break;
        case SWS_OP_SCALE:
            for (int i = 0; i < 4; i++) {
                op->comps.flags[i] |= prev.flags[i];
                if (op->c.q.den != 1) /* fractional scale */
                    op->comps.flags[i] &= ~SWS_COMP_EXACT;
                if (op->c.q.num < 0)
                    FFSWAP(AVRational, op->comps.min[i], op->comps.max[i]);
            }
            break;

        case SWS_OP_INVALID:
        case SWS_OP_TYPE_NB:
            av_assert0(!"Invalid operation type!");
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
        case SWS_OP_MIN:
        case SWS_OP_MAX:
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
                if (op->c.q4[i].den)
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
                           SwsConst *out_scale)
{
    SwsConst scale = {0};

    /* There are components not on the main diagonal */
    if (c->mask & ~SWS_MASK_DIAG4)
        return false;

    for (int i = 0; i < 4; i++) {
        const AVRational s = c->m[i][i];
        if ((prev.flags[i] & SWS_COMP_ZERO) || next.unused[i])
            continue;
        if (scale.q.den && av_cmp_q(s, scale.q))
            return false;
        scale.q = s;
    }

    if (scale.q.den)
        *out_scale = scale;
    return scale.q.den;
}

/* Extracts an integer clear operation (subset) from the given linear op. */
static bool extract_constant_rows(SwsLinearOp *c, SwsComps prev,
                                  SwsConst *out_clear)
{
    SwsConst clear = {0};
    bool ret = false;

    for (int i = 0; i < 4; i++) {
        bool const_row = c->m[i][4].den == 1; /* offset is integer */
        for (int j = 0; j < 4; j++) {
            const_row &= c->m[i][j].num == 0 || /* scalar is zero */
                         (prev.flags[j] & SWS_COMP_ZERO); /* input is zero */
        }
        if (const_row && (c->mask & SWS_MASK_ROW(i))) {
            clear.q4[i] = c->m[i][4];
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

        op_list_update_comps(ops);

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
                    next->rw.packed == op->rw.packed &&
                    next->rw.frac == op->rw.frac)
                {
                    ff_sws_op_list_remove_at(ops, n, 2);
                    av_assert1(ops->num_ops == 0);
                    return 0;
                }

                /* Skip reading extra unneeded components */
                if (!op->rw.packed) {
                    int needed = op->rw.elems;
                    while (needed > 0 && next->comps.unused[needed - 1])
                        needed--;
                    if (op->rw.elems != needed) {
                        op->rw.elems = needed;
                        op->rw.packed &= op->rw.elems > 1;
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
                    op->c.u += next->c.u;
                    ff_sws_op_list_remove_at(ops, n + 1, 1);
                    continue;
                }

                /* No-op shift */
                if (!op->c.u) {
                    ff_sws_op_list_remove_at(ops, n, 1);
                    continue;
                }
                break;

            case SWS_OP_CLEAR:
                for (int i = 0; i < 4; i++) {
                    if (!op->c.q4[i].den)
                        continue;

                    if ((prev->comps.flags[i] & SWS_COMP_ZERO) &&
                        !(prev->comps.flags[i] & SWS_COMP_GARBAGE) &&
                        op->c.q4[i].num == 0)
                    {
                        /* Redundant clear-to-zero of zero component */
                        op->c.q4[i].den = 0;
                    } else if (next->comps.unused[i]) {
                        /* Unnecessary clear of unused component */
                        op->c.q4[i] = (AVRational) {0, 0};
                    } else if (op->c.q4[i].den) {
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
                        if (next->c.q4[i].den)
                            op->c.q4[i] = next->c.q4[i];
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
                    ff_sws_apply_op_q(next, op->c.q4);
                    swap_ops(op, next);
                    progress = true;
                    continue;
                }
                break;

            case SWS_OP_SWIZZLE: {
                bool seen[4] = {0};
                bool has_duplicates = false;
                for (int i = 0; i < 4; i++) {
                    if (next->comps.unused[i])
                        continue;
                    if (op->swizzle.in[i] != i)
                        noop = false;
                    has_duplicates |= seen[op->swizzle.in[i]];
                    seen[op->swizzle.in[i]] = true;
                }

                /* Identity swizzle */
                if (noop) {
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

                /* Try to push swizzles with duplicates towards the output */
                if (has_duplicates && op_type_is_independent(next->op)) {
                    if (next->op == SWS_OP_CONVERT)
                        op->type = next->convert.to;
                    if (next->op == SWS_OP_MIN || next->op == SWS_OP_MAX) {
                        /* Un-swizzle the next operation */
                        const SwsConst c = next->c;
                        for (int i = 0; i < 4; i++) {
                            if (!next->comps.unused[i])
                                next->c.q4[op->swizzle.in[i]] = c.q4[i];
                        }
                    }
                    swap_ops(op, next);
                    progress = true;
                    continue;
                }
                break;
            }

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
                    !av_cmp_q(next->c.q, expand_factor(op->type, op->convert.to)))
                {
                    op->convert.expand = true;
                    ff_sws_op_list_remove_at(ops, n + 1, 1);
                    continue;
                }
                break;

            case SWS_OP_MIN:
                for (int i = 0; i < 4; i++) {
                    if (next->comps.unused[i] || !op->c.q4[i].den)
                        continue;
                    if (av_cmp_q(op->c.q4[i], prev->comps.max[i]) < 0)
                        noop = false;
                }

                if (noop) {
                    ff_sws_op_list_remove_at(ops, n, 1);
                    continue;
                }
                break;

            case SWS_OP_MAX:
                for (int i = 0; i < 4; i++) {
                        if (next->comps.unused[i] || !op->c.q4[i].den)
                        continue;
                    if (av_cmp_q(prev->comps.min[i], op->c.q4[i]) < 0)
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
                SwsConst c;

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
                if (extract_constant_rows(&op->lin, prev->comps, &c)) {
                    RET(ff_sws_op_list_insert_at(ops, n + 1, &(SwsOp) {
                        .op    = SWS_OP_CLEAR,
                        .type  = op->type,
                        .comps = op->comps,
                        .c     = c,
                    }));
                    continue;
                }

                /* Multiplication by scalar constant */
                if (extract_scalar(&op->lin, prev->comps, next->comps, &c)) {
                    op->op = SWS_OP_SCALE;
                    op->c  = c;
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
                const int factor2 = exact_log2_q(op->c.q);

                /* No-op scaling */
                if (op->c.q.num == 1 && op->c.q.den == 1) {
                    ff_sws_op_list_remove_at(ops, n, 1);
                    continue;
                }

                /* Scaling by integer before conversion to int */
                if (op->c.q.den == 1 &&
                    next->op == SWS_OP_CONVERT &&
                    ff_sws_pixel_type_is_int(next->convert.to))
                {
                    op->type = next->convert.to;
                    swap_ops(op, next);
                    progress = true;
                    continue;
                }

                /* Scaling by exact power of two */
                if (factor2 && ff_sws_pixel_type_is_int(op->type)) {
                    op->op = factor2 > 0 ? SWS_OP_LSHIFT : SWS_OP_RSHIFT;
                    op->c.u = FFABS(factor2);
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

void ff_sws_op_chain_uninit(SwsOpChain *chain)
{
    for (int i = 0; i < chain->num_impl; i++) {
        if (chain->free[i])
            chain->free[i](chain->impl[i].priv.ptr);
    }

    *chain = (SwsOpChain) {0};
}

int ff_sws_op_chain_append(SwsOpChain *chain, SwsFunc func, void (*free)(void *),
                           SwsOpPriv priv)
{
    const int idx = chain->num_impl;
    if (idx == SWS_MAX_OPS)
        return AVERROR(EINVAL);

    av_assert1(func);
    if (!idx)
        chain->entry = func;
    else
        chain->impl[idx - 1].cont = func;
    chain->impl[idx].priv = priv;
    chain->free[idx] = free;
    chain->num_impl++;
    return 0;
}

typedef struct SwsOpPass {
    SwsOpChain chain;
    SwsOpExec exec_base;
    int pixel_bits_in;
    int pixel_bits_out;
    int safe_w_in;
    int safe_w_out;
    int aligned_w;
} SwsOpPass;

static void op_pass_reset(SwsOpPass *p)
{
    ff_sws_op_chain_uninit(&p->chain);
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

/* Processing extra pixels is fine as long as they're within linesize */
static int safe_width(const SwsImg *img, const int pixel_bits)
{
    const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(img->fmt);
    int safe_w_min = INT_MAX;

    for (int i = 0; i < 4; i++) {
        const int sub_x  = (i == 1 || i == 2) ? desc->log2_chroma_w : 0;
        const int safe_w = (img->linesize[i] * 8) / pixel_bits << sub_x;
        if (!img->linesize[i])
            continue;
        av_assert2(safe_w > 0);
        safe_w_min = FFMIN(safe_w_min, safe_w);
    }

    return safe_w_min;
}

static void op_pass_setup(const SwsImg *out, const SwsImg *in, const SwsPass *pass)
{
    SwsOpPass *p = pass->priv;
    SwsOpExec *exec = &p->exec_base;
    const int w = pass->width;

    /* Set up main loop parameters */
    p->safe_w_in  = safe_width(in,  p->pixel_bits_in);
    p->safe_w_out = safe_width(out, p->pixel_bits_out);
    p->aligned_w = (w + exec->block_size - 1) / exec->block_size * exec->block_size;

    for (int i = 0; i < 4; i++) {
        exec->in_stride[i]  = in->linesize[i];
        exec->out_stride[i] = out->linesize[i];
    }
}

/* Dispatch kernel over the "main" part of the image, no extra padding */
static av_always_inline void
run_main(const SwsOpPass *p, const SwsImg *out_base, const SwsImg *in_base,
         const int y_start, const int y_end, const int x_end)
{
    const SwsOpImpl *const impl = p->chain.impl;
    const SwsFunc entry = p->chain.entry;
    SwsOpExec exec = p->exec_base;

    const ptrdiff_t block_step_in  = (exec.block_size * p->pixel_bits_in)  >> 3;
    const ptrdiff_t block_step_out = (exec.block_size * p->pixel_bits_out) >> 3;

    for (exec.y = y_start; exec.y < y_end; exec.y++) {
        const SwsImg in  = ff_sws_img_shift(*in_base,  exec.y);
        const SwsImg out = ff_sws_img_shift(*out_base, exec.y);
        for (int i = 0; i < 4; i++) {
            exec.in[i] = in.data[i];
            exec.out[i] = out.data[i];
        }

        for (exec.x = 0; exec.x < x_end; exec.x += exec.block_size) {
            entry(&exec, impl);

            for (int i = 0; i < 4; i++) {
                exec.in[i]  += block_step_in;
                exec.out[i] += block_step_out;
            }
        }
    }
}


/* Dispatch kernel over the last column part of the image using memcpy
 * into a padded buffer */
static av_always_inline void
run_tail(const SwsOpPass *p, const SwsImg *out_base, const bool copy_out,
         const SwsImg *in_base, const bool copy_in, const int y_start,
         const int y_end, const int x_tail)
{
    const SwsOpImpl *const impl = p->chain.impl;
    const SwsFunc entry = p->chain.entry;
    SwsOpExec exec = p->exec_base;

    const int offset_in     = x_tail * p->pixel_bits_in  >> 3;
    const int offset_out    = x_tail * p->pixel_bits_out >> 3;
    const int rest_w        = exec.w - x_tail;
    const int rest_size_in  = (rest_w * p->pixel_bits_in  + 7) >> 3;
    const int rest_size_out = (rest_w * p->pixel_bits_out + 7) >> 3;

    DECLARE_ALIGNED_64(uint8_t, tmp)[2][4][64 * sizeof(uint32_t)];
    av_assert1(rest_w <= 64);

    exec.x = x_tail;
    for (int i = 0; i < 4; i++) {
        if (copy_in) {
            exec.in[i] = tmp[0][i];
            exec.in_stride[i] = sizeof(tmp[0][i]);
        }
        if (copy_out) {
            exec.out[i] = tmp[1][i];
            exec.out_stride[i] = sizeof(tmp[1][i]);
        }
    }

    for (exec.y = y_start; exec.y < y_end; exec.y++) {
        SwsImg in  = ff_sws_img_shift(*in_base,  exec.y);
        SwsImg out = ff_sws_img_shift(*out_base, exec.y);
        for (int i = 0; i < 4; i++) {
            if (!copy_in)
                exec.in[i] = in.data[i] + offset_in;
            if (!copy_out)
                exec.out[i] = out.data[i] + offset_out;
        }

        if (copy_in) {
            for (int i = 0; i < 4 && in.data[i]; i++)
                memcpy(tmp[0][i], in.data[i] + offset_in, rest_size_in);
        }

        entry(&exec, impl);

        if (copy_out) {
            for (int i = 0; i < 4 && out.data[i]; i++)
                memcpy(out.data[i] + offset_out, tmp[1][i], rest_size_out);
        }
    }
}

static av_always_inline void
op_pass_run(const SwsImg *out, const SwsImg *in, const int y, const int h,
            const SwsPass *pass)
{
    /**
     *  To ensure safety, we need to consider the following:
     *
     * 1. We can overread the input, unless this is the last line of an
     *    unpadded buffer. All operation chains must be able to handle
     *    arbitrary pixel input, so arbitrary overread is fine.
     *
     * 2. We can overwrite the output, as long as we don't write more than the
     *    amount of pixels that fit into one linesize. So we always need to
     *    memcpy the last column on the output side if unpadded.
     *
     * 3. For the last row, we also need to memcpy the remainder of the input,
     *    to avoid reading past the end of the buffer. Note that since we know
     *    the run() function is called on stripes of the same buffer, we don't
     *    need to worry about this for the end of a slice.
     */

    const SwsOpPass *p = pass->priv;
    const int last_slice = y + h == pass->height;
    const bool in_unpadded = last_slice && p->aligned_w > p->safe_w_in;
    const bool out_unpadded = p->aligned_w > p->safe_w_out;
    const int block_size = p->exec_base.block_size;
    const int x_end = p->aligned_w;
    const int y_end = y + h;
    const int y_end_safe = y_end - 1;
    const int x_end_safe = x_end - block_size;

    if (out_unpadded) {
        /* Run last column separately */
        run_main(p, out, in, y, y_end, x_end_safe);
        run_tail(p, out, true, in, in_unpadded, y, y_end, x_end_safe);
    } else if (in_unpadded) {
        /* Run last row separately */
        run_main(p, out, in, y, y_end_safe, x_end);
        run_main(p, out, in, y_end_safe, y_end, x_end_safe);
        run_tail(p, out, false, in, true, y_end_safe, y_end, x_end_safe);
    } else {
        run_main(p, out, in, y, y_end, x_end);
    }
}

static int rw_pixel_bits(const SwsOp *op)
{
    const int elems = op->rw.packed ? op->rw.elems : 1;
    const int size  = ff_sws_pixel_type_size(op->type);
    const int bits  = 8 >> op->rw.frac;
    av_assert1(bits >= 1);
    return elems * size * bits;
}

int ff_sws_ops_compile_backend(SwsContext *ctx, const SwsOpBackend *backend,
                               const SwsOpList *ops, SwsOpChain *out_chain)
{
    SwsOpChain chain = {0};
    SwsOpList *copy, rest;
    int ret = 0;

    copy = ff_sws_op_list_duplicate(ops);
    if (!copy)
        return AVERROR(ENOMEM);

    /* Ensure these are always set during compilation */
    op_list_update_comps(copy);

    /* Make an on-stack copy of `ops` to ensure we can still properly clean up
     * the copy afterwards */
    rest = *copy;
    do {
        ret = backend->compile(ctx, &rest, &chain);
    } while (ret == AVERROR(EAGAIN));

    if (ret == AVERROR(ENOTSUP)) {
        av_log(ctx, AV_LOG_DEBUG, "Backend '%s' does not support operations:\n", backend->name);
        ff_sws_op_list_print(ctx, AV_LOG_DEBUG, &rest);
        goto fail;
    } else if (ret < 0) {
        av_log(ctx, AV_LOG_ERROR, "Failed to compile operations: %s\n", av_err2str(ret));
        ff_sws_op_list_print(ctx, AV_LOG_ERROR, &rest);
        goto fail;
    }

    ff_sws_op_list_free(&copy);
    *out_chain = chain;
    return 0;

fail:
    ff_sws_op_list_free(&copy);
    ff_sws_op_chain_uninit(&chain);
    return ret;
}

int ff_sws_ops_compile(SwsContext *ctx, const SwsOpList *ops, SwsOpChain *chain)
{
    for (int n = 0; ff_sws_op_backends[n]; n++) {
        const SwsOpBackend *backend = ff_sws_op_backends[n];
        if (ff_sws_ops_compile_backend(ctx, backend, ops, chain) < 0)
            continue;

        av_log(ctx, AV_LOG_VERBOSE, "Compiled using backend '%s': "
               "num_impl = %d, block size = %d\n",
               backend->name, chain->num_impl, chain->block_size);
        return 0;
    }

    av_log(ctx, AV_LOG_WARNING, "No backend found for operations:\n");
    ff_sws_op_list_print(ctx, AV_LOG_WARNING, ops);
    return AVERROR(ENOTSUP);
}

int ff_sws_compile_pass(SwsGraph *graph, SwsOpList *ops, int flags, SwsFormat dst,
                        SwsPass *input, SwsPass **output)
{
    SwsContext *ctx = graph->ctx;
    SwsOpPass *p = NULL;
    const SwsOp *read = &ops->ops[0];
    const SwsOp *write = &ops->ops[ops->num_ops - 1];
    SwsPass *pass;
    int ret;

    if (ops->num_ops < 2) {
        av_log(ctx, AV_LOG_ERROR, "Need at least two operations.\n");
        return AVERROR(EINVAL);
    }

    if (read->op != SWS_OP_READ || write->op != SWS_OP_WRITE) {
        av_log(ctx, AV_LOG_ERROR, "First and last operations must be a read "
               "and write, respectively.\n");
        return AVERROR(EINVAL);
    }

    if (flags & SWS_OP_FLAG_OPTIMIZE)
        RET(ff_sws_op_list_optimize(ops));
    else
        op_list_update_comps(ops);

    p = av_mallocz(sizeof(*p));
    if (!p)
        return AVERROR(ENOMEM);

    p->pixel_bits_in  = rw_pixel_bits(read);
    p->pixel_bits_out = rw_pixel_bits(write);
    p->exec_base = (SwsOpExec) {
        .w = dst.width,
        .h = dst.height,
    };

    ret = ff_sws_ops_compile(ctx, ops, &p->chain);
    if (ret < 0)
        goto fail;

    p->exec_base.block_size = p->chain.block_size;
    pass = ff_sws_graph_add_pass(graph, dst.format, dst.width, dst.height, input,
                                 1, p, op_pass_run);
    if (!pass) {
        ret = AVERROR(ENOMEM);
        goto fail;
    }
    pass->setup = op_pass_setup;
    pass->free  = op_pass_free;

    *output = pass;
    return 0;

fail:
    op_pass_free(p);
    return ret;
}

int ff_sws_op_compile_tables(const SwsOpTable *const tables[], int num_tables,
                             SwsOpList *ops, const int block_size,
                             SwsOpChain *chain)
{
    static const SwsOp dummy = { .comps.unused = { true, true, true, true }};
    const SwsOp *next = ops->num_ops > 1 ? &ops->ops[1] : &dummy;
    const unsigned cpu_flags = av_get_cpu_flags();
    const SwsOpEntry *best = NULL;
    const SwsOp *op = &ops->ops[0];
    int ret, best_score = 0;
    SwsOpPriv priv = {0};

    for (int n = 0; n < num_tables; n++) {
        const SwsOpTable *table = tables[n];
        if (table->block_size && table->block_size != block_size ||
            table->cpu_flags & ~cpu_flags)
            continue;

        for (int i = 0; table->entries[i].op.op; i++) {
            const SwsOpEntry *entry = &table->entries[i];
            int score = op_match(op, entry, next->comps);
            if (score > best_score) {
                best_score = score;
                best = entry;
            }
        }
    }

    if (!best)
        return AVERROR(ENOTSUP);

    if (best->setup) {
        ret = best->setup(op, &priv);
        if (ret < 0)
            return ret;
    }

    ret = ff_sws_op_chain_append(chain, best->func, best->free, priv);
    if (ret < 0) {
        if (best->free)
            best->free(&priv);
        return ret;
    }

    ops->ops++;
    ops->num_ops--;
    return ops->num_ops ? AVERROR(EAGAIN) : 0;
}

#define q2pixel(type, q) ((q).den ? (type) (q).num / (q).den : 0)

int ff_sws_setup_u8(const SwsOp *op, SwsOpPriv *out)
{
    out->u8[0] = op->c.u;
    return 0;
}

int ff_sws_setup_u(const SwsOp *op, SwsOpPriv *out)
{
    switch (op->type) {
    case SWS_PIXEL_U8:  out->u8[0]  = op->c.u; return 0;
    case SWS_PIXEL_U16: out->u16[0] = op->c.u; return 0;
    case SWS_PIXEL_U32: out->u32[0] = op->c.u; return 0;
    case SWS_PIXEL_F32: out->f32[0] = op->c.u; return 0;
    default: return AVERROR(EINVAL);
    }
}

int ff_sws_setup_q(const SwsOp *op, SwsOpPriv *out)
{
    switch (op->type) {
    case SWS_PIXEL_U8:  out->u8[0]  = q2pixel(uint8_t,  op->c.q); return 0;
    case SWS_PIXEL_U16: out->u16[0] = q2pixel(uint16_t, op->c.q); return 0;
    case SWS_PIXEL_U32: out->u32[0] = q2pixel(uint32_t, op->c.q); return 0;
    case SWS_PIXEL_F32: out->f32[0] = q2pixel(float,    op->c.q); return 0;
    default: return AVERROR(EINVAL);
    }

    return 0;
}

int ff_sws_setup_q4(const SwsOp *op, SwsOpPriv *out)
{
    for (int i = 0; i < 4; i++) {
        switch (op->type) {
        case SWS_PIXEL_U8:  out->u8[i]  = q2pixel(uint8_t,  op->c.q4[i]); break;
        case SWS_PIXEL_U16: out->u16[i] = q2pixel(uint16_t, op->c.q4[i]); break;
        case SWS_PIXEL_U32: out->u32[i] = q2pixel(uint32_t, op->c.q4[i]); break;
        case SWS_PIXEL_F32: out->f32[i] = q2pixel(float,    op->c.q4[i]); break;
        default: return AVERROR(EINVAL);
        }
    }

    return 0;
}
