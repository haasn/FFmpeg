/**
 * Copyright (C) 2025 Niklas Haas
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with FFmpeg; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <string.h>

#include "libavutil/avassert.h"
#include "libavutil/mem_internal.h"
#include "libavutil/refstruct.h"

#include "libswscale/ops.h"
#include "libswscale/ops_internal.h"

#include "checkasm.h"

enum {
    MAX_BLOCK_W = 64,
    MAX_BLOCK_H = 1,
};

enum {
    U8  = SWS_PIXEL_U8,
    U16 = SWS_PIXEL_U16,
    U32 = SWS_PIXEL_U32,
    F32 = SWS_PIXEL_F32,
};

#define FMT(fmt, ...) tprintf((char[256]) {0}, 256, fmt, __VA_ARGS__)
static const char *tprintf(char buf[], size_t size, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(buf, size, fmt, ap);
    va_end(ap);
    return buf;
}

static int rw_pixel_bits(const SwsOp *op)
{
    const int elems = op->rw.packed ? op->rw.elems : 1;
    const int size  = ff_sws_pixel_type_size(op->type);
    const int bits  = 8 >> op->rw.frac;
    av_assert1(bits >= 1);
    return elems * size * bits;
}

static void check_ops(const char *report, const uint32_t mask, const SwsOp *ops)
{
    SwsOpChain chain0 = {0}, chain1 = {0};
    SwsOpExec exec0 = {0}, exec1 = {0};
    SwsOpList oplist = { .ops = (SwsOp *) ops };
    int pixel_bits_in, pixel_bits_out;
    const SwsOp *read_op, *write_op;

    declare_func(void, const SwsOpExec *exec, const SwsOpImpl *impl);

    DECLARE_ALIGNED_64(uint32_t, src0)[4][MAX_BLOCK_H][4 * MAX_BLOCK_W];
    DECLARE_ALIGNED_64(uint32_t, src1)[4][MAX_BLOCK_H][4 * MAX_BLOCK_W];
    DECLARE_ALIGNED_64(uint32_t, dst0)[4][MAX_BLOCK_H][4 * MAX_BLOCK_W];
    DECLARE_ALIGNED_64(uint32_t, dst1)[4][MAX_BLOCK_H][4 * MAX_BLOCK_W];

    for (int p = 0; p < 4; p++) {
        for (int y = 0; y < MAX_BLOCK_H; y++) {
            for (int x = 0; x < 4 * MAX_BLOCK_W; x++)
                src0[p][y][x] = rnd() & mask;
        }
    }

    memcpy(src1, src0, sizeof(src0));
    memset(dst0, 0, sizeof(dst0));
    memset(dst1, 0, sizeof(dst1));

    read_op = &ops[0];
    for (oplist.num_ops = 0; ops[oplist.num_ops].op; oplist.num_ops++)
        write_op = &ops[oplist.num_ops];
    pixel_bits_in  = rw_pixel_bits(read_op);
    pixel_bits_out = rw_pixel_bits(write_op);

    /* Compile `ops` using both the asm and c backends */
    for (int n = 0; ff_sws_op_backends[n]; n++) {
        const SwsOpBackend *backend = ff_sws_op_backends[n];
        const bool is_ref = !strcmp(backend->name, "c");
        if (is_ref || !chain1.entry) {
            SwsOpChain chain;
            int ret = ff_sws_ops_compile_backend(backend, &oplist, &chain);
            if (ret == AVERROR(ENOTSUP))
                continue;
            else if (ret < 0)
                fail();
            else if (chain.block_w > MAX_BLOCK_W || chain.block_h > MAX_BLOCK_H)
                fail();

            if (is_ref)
                chain0 = chain;
            if (!chain1.entry)
                chain1 = chain;
        }
    }

    av_assert0(chain0.entry);

    for (int i = 0; i < 4; i++) {
        exec1.in[i]  = (void *) src1[i];
        exec1.out[i] = (void *) dst1[i];
        exec0.in_stride[i]  = exec1.in_stride[i]  = sizeof(src0[i][0]);
        exec0.out_stride[i] = exec1.out_stride[i] = sizeof(dst0[i][0]);
    }

    exec0.block_w = chain0.block_w ? chain0.block_w : MAX_BLOCK_W;
    exec0.block_h = chain0.block_h ? chain0.block_h : MAX_BLOCK_H;
    exec1.block_w = chain1.block_w ? chain1.block_w : MAX_BLOCK_W;
    exec1.block_h = chain1.block_h ? chain1.block_h : MAX_BLOCK_H;
    exec0.w = exec1.w = exec1.block_w;
    exec0.h = exec1.h = exec0.slice_h = exec1.slice_h = exec1.block_h;

    if (check_func(chain1.entry, "%s", report)) {
        func_ref = chain0.entry; /* ignore any other asm versions */

        /* Reference function may have smaller block size, so make sure
         * to properly loop to cover the whole expected result */
        for (int y = 0; y < exec1.block_h; y += exec0.block_h) {
            for (int i = 0; i < 4; i++) {
                exec0.in[i]  = (void *) src0[i][y];
                exec0.out[i] = (void *) dst0[i][y];
            }

            for (int x = 0; x < exec1.block_w; x += exec0.block_w) {
                av_assert1(x + exec0.block_w <= MAX_BLOCK_W);
                call_ref(&exec0, chain0.impl);
                for (int i = 0; i < 4; i++) {
                    exec0.in[i]  += exec0.block_w * pixel_bits_in  >> 3;
                    exec0.out[i] += exec0.block_w * pixel_bits_out >> 3;
                }
            }
        }

        call_new(&exec1, chain1.impl);

        for (int i = 0; i < 4; i++) {
            const char *name = FMT("%s[%d]", report, i);
            switch (write_op->type) {
            case U8:
                checkasm_check(uint8_t, (void *) dst0[i], exec0.out_stride[i],
                                        (void *) dst1[i], exec1.out_stride[i],
                                        exec1.w, exec1.h, name);
                break;
            case U16:
                checkasm_check(uint16_t, (void *) dst0[i], exec0.out_stride[i],
                                         (void *) dst1[i], exec1.out_stride[i],
                                         exec1.w, exec1.h, name);
                break;
            case U32:
            case F32:
                checkasm_check(uint32_t, (void *) dst0[i], exec0.out_stride[i],
                                         (void *) dst1[i], exec1.out_stride[i],
                                         exec1.w, exec1.h, name);
                break;
            }
        }

        /* Check for over-write */
        for (int y = 0; y < exec1.block_h; y++) {
            for (int p = 0; p < 4; p++) {
                const int base = exec1.w * pixel_bits_out >> 5; /* as uint32_t */
                for (int i = base; i < FF_ARRAY_ELEMS(dst1[p][y]); i++) {
                    if (dst1[p][y][i] != 0) {
                        fprintf(stderr, "Overwrite detected at dst[%d][%d][%d] = 0x%08x\n",
                                p, y, i * 4, dst1[p][y][i]);
                        fail();
                    }
                }
            }
        }

        bench_new(&exec1, chain1.impl);
    }

    if (chain1.entry != chain0.entry)
        ff_sws_op_chain_uninit(&chain1);
    ff_sws_op_chain_uninit(&chain0);
}

#define CHECK_MASK(NAME, MASK, N_IN, N_OUT, IN, OUT, ...)                       \
  do {                                                                          \
    check_ops(NAME, MASK, (SwsOp[]) {                                           \
        {                                                                       \
            .op = SWS_OP_READ,                                                  \
            .type = IN,                                                         \
            .rw.elems = N_IN,                                                   \
        },                                                                      \
        __VA_ARGS__,                                                            \
        {                                                                       \
            .op = SWS_OP_WRITE,                                                 \
            .type = OUT,                                                        \
            .rw.elems = N_OUT,                                                  \
        }, {0}                                                                  \
    });                                                                         \
  } while (0)

#define CHECK_COMMON_MASK(NAME, MASK, IN, OUT, ...)                             \
    CHECK_MASK(FMT("%s_p1000", NAME), MASK, 1, 1, IN, OUT, __VA_ARGS__);        \
    CHECK_MASK(FMT("%s_p1110", NAME), MASK, 3, 3, IN, OUT, __VA_ARGS__);        \
    CHECK_MASK(FMT("%s_p1111", NAME), MASK, 4, 4, IN, OUT, __VA_ARGS__);        \
    CHECK_MASK(FMT("%s_p1001", NAME), MASK, 4, 2, IN, OUT, __VA_ARGS__, {       \
        .op = SWS_OP_SWIZZLE,                                                   \
        .type = OUT,                                                            \
        .swizzle = SWS_SWIZZLE(0, 3, 1, 2),                                     \
    })

#define CHECK(NAME, N_IN, N_OUT, IN, OUT, ...) \
    CHECK_MASK(NAME, 0xFFFFFFFF, N_IN, N_OUT, IN, OUT, __VA_ARGS__)

#define CHECK_COMMON(NAME, IN, OUT, ...) \
    CHECK_COMMON_MASK(NAME, 0xFFFFFFFF, IN, OUT, __VA_ARGS__)

static void check_read_write(void)
{
    for (SwsPixelType t = U8; t < SWS_PIXEL_TYPE_NB; t++) {
        const char *type = ff_sws_pixel_type_name(t);
        for (int i = 1; i <= 4; i++) {
            /* Test N->N planar read/write */
            for (int o = 1; o <= i; o++) {
                check_ops(FMT("rw_%d_%d_%s", i, o, type), 0xFFFFFFFF, (SwsOp[]) {
                    {
                        .op = SWS_OP_READ,
                        .type = t,
                        .rw.elems = i,
                    }, {
                        .op = SWS_OP_WRITE,
                        .type = t,
                        .rw.elems = o,
                    }, {0}
                });
            }

            /* Test packed read/write */
            if (i == 1)
                continue;

            check_ops(FMT("read_packed%d_%s", i, type), 0xFFFFFFFF, (SwsOp[]) {
                {
                    .op = SWS_OP_READ,
                    .type = t,
                    .rw.elems = i,
                    .rw.packed = true,
                }, {
                    .op = SWS_OP_WRITE,
                    .type = t,
                    .rw.elems = i,
                }, {0}
            });

            check_ops(FMT("write_packed%d_%s", i, type), 0xFFFFFFFF, (SwsOp[]) {
                {
                    .op = SWS_OP_READ,
                    .type = t,
                    .rw.elems = i,
                }, {
                    .op = SWS_OP_WRITE,
                    .type = t,
                    .rw.elems = i,
                    .rw.packed = true,
                }, {0}
            });
        }
    }
}

static void check_swap_bytes(void)
{
    CHECK_COMMON("swap_bytes_16", U16, U16, {
        .op   = SWS_OP_SWAP_BYTES,
        .type = U16,
    });

    CHECK_COMMON("swap_bytes_32", U32, U32, {
        .op   = SWS_OP_SWAP_BYTES,
        .type = U32,
    });
}

static void check_pack_unpack(void)
{
    const struct {
        SwsPixelType type;
        SwsPackOp op;
    } patterns[] = {
        { U8, { U8, { 3,  3,  2 }}},
        { U8, { U8, { 2,  3,  3 }}},
        { U8, { U8, { 1,  2,  1 }}},
        { U8, {U16, { 5,  6,  5 }}},
        { U8, {U16, { 5,  5,  5 }}},
        { U8, {U16, { 4,  4,  4 }}},
        {U16, {U32, { 2, 10, 10, 10 }}},
        {U16, {U32, {10, 10, 10,  2 }}},
    };

    for (int i = 0; i < FF_ARRAY_ELEMS(patterns); i++) {
        const SwsPackOp pack = patterns[i].op;
        const int num = pack.pattern[3] ? 4 : 3;
        const char *pat = FMT("%d%d%d%d", pack.pattern[0], pack.pattern[1],
                                          pack.pattern[2], pack.pattern[3]);

        CHECK(FMT("pack_%s", pat), num, 1, patterns[i].type, pack.type, {
            .op   = SWS_OP_PACK,
            .type = patterns[i].type,
            .pack = pack,
        });

        CHECK(FMT("unpack_%s", pat), 1, num, pack.type, patterns[i].type, {
            .op   = SWS_OP_UNPACK,
            .type = patterns[i].type,
            .pack = pack,
        });
    }
}

static AVRational rndq(SwsPixelType t)
{
    const unsigned num = rnd();
    if (ff_sws_pixel_type_is_int(t)) {
        const unsigned mask = (1 << (ff_sws_pixel_type_size(t) * 8)) - 1;
        return (AVRational) { num & mask, 1 };
    } else {
        const unsigned den = rnd();
        return (AVRational) { num, den ? den : 1 };
    }
}

static void check_clear(void)
{
    for (SwsPixelType t = U8; t < SWS_PIXEL_TYPE_NB; t++) {
        const char *type = ff_sws_pixel_type_name(t);
        const int bits = ff_sws_pixel_type_size(t) * 8;

        /* TODO: AVRational can't fit 32 bit constants */
        if (bits < 32) {
            const AVRational chroma = (AVRational) { 1 << (bits - 1), 1};
            const AVRational alpha  = (AVRational) { (1 << bits) - 1, 1};
            const AVRational none = {0};

            const SwsClearOp patterns[] = {
                /* Alpha only */
                {{   none,   none,   none,  alpha }},
                {{  alpha,   none,   none,   none }},
                /* Chroma only */
                {{ chroma, chroma,   none,   none }},
                {{   none, chroma, chroma,   none }},
                {{   none,   none, chroma, chroma }},
                {{ chroma,   none, chroma,   none }},
                {{   none, chroma,   none, chroma }},
                /* Alpha+chroma */
                {{ chroma, chroma,   none,  alpha }},
                {{   none, chroma, chroma,  alpha }},
                {{  alpha,   none, chroma, chroma }},
                {{ chroma,   none, chroma,  alpha }},
                {{  alpha, chroma,   none, chroma }},
                /* Random values */
                {{ none, rndq(t), rndq(t), rndq(t) }},
                {{ none, rndq(t), rndq(t), rndq(t) }},
                {{ none, rndq(t), rndq(t), rndq(t) }},
                {{ none, rndq(t), rndq(t), rndq(t) }},
            };

            for (int i = 0; i < FF_ARRAY_ELEMS(patterns); i++) {
                CHECK(FMT("clear_pattern_%s[%d]", type, i), 4, 4, t, t, {
                    .op = SWS_OP_CLEAR,
                    .type = t,
                    .clear = patterns[i],
                });
            }
        } else if (!ff_sws_pixel_type_is_int(t)) {
            /* Floating point YUV doesn't exist, only alpha needs to be cleared */
            CHECK(FMT("clear_alpha_%s", type), 4, 4, t, t, {
                .op = SWS_OP_CLEAR,
                .type = t,
                .clear.value[3] = { 0, 1 },
            });
        }
    }
}

static void check_shift(void)
{
    for (SwsPixelType t = U16; t < SWS_PIXEL_TYPE_NB; t++) {
        const char *type = ff_sws_pixel_type_name(t);
        if (!ff_sws_pixel_type_is_int(t))
            continue;

        for (int shift = 1; shift <= 8; shift++) {
            CHECK_COMMON(FMT("lshift%d_%s", shift, type), t, t, {
                .op = SWS_OP_LSHIFT,
                .type = t,
                .shift.amount = shift,
            });

            CHECK_COMMON(FMT("rshift%d_%s", shift, type), t, t, {
                .op = SWS_OP_RSHIFT,
                .type = t,
                .shift.amount = shift,
            });
        }
    }
}

static void check_swizzle(void)
{
    for (SwsPixelType t = U8; t < SWS_PIXEL_TYPE_NB; t++) {
        const char *type = ff_sws_pixel_type_name(t);
        static const int patterns[][4] = {
            /* Pure swizzle */
            {0, 1, 2, 3},
            {3, 0, 1, 2},
            {2, 1, 0, 3},
            {3, 2, 1, 0},
            {3, 1, 0, 2},
            {3, 2, 0, 1},
            {1, 2, 0, 3},
            {1, 0, 2, 3},
            {2, 0, 1, 3},
            {2, 3, 1, 0},
            {2, 1, 3, 0},
            {1, 2, 3, 0},
            {0, 2, 1, 3},
            {0, 2, 3, 1},
            {0, 3, 1, 2},
            {3, 1, 2, 0},
            {0, 3, 2, 1},
            /* Luma expansion */
            {0, 0, 0, 3},
            {3, 0, 0, 0},
            {0, 0, 0, 1},
            {1, 0, 0, 0},
        };

        for (int i = 0; i < FF_ARRAY_ELEMS(patterns); i++) {
            const int x = patterns[i][0], y = patterns[i][1],
                      z = patterns[i][2], w = patterns[i][3];
            CHECK(FMT("swizzle_%d%d%d%d_%s", x, y, z, w, type), 4, 4, t, t, {
                .op = SWS_OP_SWIZZLE,
                .type = t,
                .swizzle = SWS_SWIZZLE(x, y, z, w),
            });
        }
    }
}

static void check_convert(void)
{
    for (SwsPixelType i = U8; i < SWS_PIXEL_TYPE_NB; i++) {
        const char *itype = ff_sws_pixel_type_name(i);
        const int isize = ff_sws_pixel_type_size(i);
        for (SwsPixelType o = U8; o < SWS_PIXEL_TYPE_NB; o++) {
            const char *otype = ff_sws_pixel_type_name(o);
            const int osize = ff_sws_pixel_type_size(o);
            const char *name = FMT("convert_%s_%s", itype, otype);
            if (i == o)
                continue;

            if (isize < osize || !ff_sws_pixel_type_is_int(o)) {
                CHECK_COMMON(name, i, o, {
                    .op = SWS_OP_CONVERT,
                    .type = i,
                    .convert.to = o,
                });
            } else if (!ff_sws_pixel_type_is_int(i)) {
                const AVRational max = { (1 << osize * 8) - 1, 1 };
                CHECK_COMMON(name, i, o, {
                    .op = SWS_OP_CLAMP,
                    .type = i,
                    .clamp.max = { max, max, max, max },
                }, {
                    .op = SWS_OP_CONVERT,
                    .type = i,
                    .convert.to = o,
                });
            } else if (isize > osize) {
                uint32_t mask = (1 << osize * 8) - 1;
                if (isize == 2)
                    mask |= mask << 16;

                CHECK_COMMON_MASK(name, mask, i, o, {
                    .op = SWS_OP_CONVERT,
                    .type = i,
                    .convert.to = o,
                });
            }
        }
    }

    /* Check expanding conversions */
    CHECK_COMMON("expand16", U8, U16, {
        .op = SWS_OP_CONVERT,
        .type = U8,
        .convert.to = U16,
        .convert.expand = true,
    });

    CHECK_COMMON("expand32", U8, U32, {
        .op = SWS_OP_CONVERT,
        .type = U8,
        .convert.to = U32,
        .convert.expand = true,
    });
}

static void check_dither(void)
{
    for (SwsPixelType t = F32; t < SWS_PIXEL_TYPE_NB; t++) {
        const char *type = ff_sws_pixel_type_name(t);
        if (ff_sws_pixel_type_is_int(t))
            continue;

        /* Test all sizes up to 16x16 */
        for (int size_log2 = 0; size_log2 <= 4; size_log2++) {
            const int size = 1 << size_log2;
            AVRational *matrix = av_refstruct_allocz(size * size * sizeof(*matrix));
            if (!matrix)
                fail();

            if (size == 1) {
                matrix[0] = (AVRational) { 1, 2 };
            } else {
                for (int i = 0; i < size * size; i++)
                    matrix[i] = rndq(t);
            }

            CHECK_COMMON(FMT("dither%d_%s", size, type), t, t, {
                .op = SWS_OP_DITHER,
                .type = t,
                .dither.size_log2 = size_log2,
                .dither.matrix = matrix,
            });

            av_refstruct_unref(&matrix);
        }
    }
}

static void check_clamp(void)
{
    for (SwsPixelType t = U8; t < SWS_PIXEL_TYPE_NB; t++) {
        const char *type = ff_sws_pixel_type_name(t);
        CHECK_COMMON(FMT("clamp_%s", type), t, t, {
            .op = SWS_OP_CLAMP,
            .type = t,
            .clamp.max = { rndq(t), rndq(t), rndq(t), rndq(t) },
        });
    }
}

static void check_linear(void)
{
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

    for (SwsPixelType t = F32; t < SWS_PIXEL_TYPE_NB; t++) {
        const char *type = ff_sws_pixel_type_name(t);
        if (ff_sws_pixel_type_is_int(t))
            continue;

        for (int p = 0; p < FF_ARRAY_ELEMS(patterns); p++) {
            const uint32_t mask = patterns[p].mask;
            SwsLinearOp lin = { .mask = mask };

            for (int i = 0; i < 4; i++) {
                for (int j = 0; j < 5; j++) {
                    if (mask & SWS_MASK(i, j)) {
                        lin.m[i][j] = rndq(t);
                    } else {
                        lin.m[i][j] = (AVRational) { i == j, 1 };
                    }
                }
            }

            CHECK(FMT("linear_%s_%s", patterns[p].name, type), 4, 4, t, t, {
                .op = SWS_OP_LINEAR,
                .type = t,
                .lin = lin,
            });
        }
    }
}

static void check_scale(void)
{
    for (SwsPixelType t = F32; t < SWS_PIXEL_TYPE_NB; t++) {
        const char *type = ff_sws_pixel_type_name(t);
        const int bits = ff_sws_pixel_type_size(t) * 8;
        if (ff_sws_pixel_type_is_int(t)) {
            /* Ensure the result won't exceed the value range */
            const unsigned max = (1 << bits) - 1;
            const unsigned scale = rnd() & max;
            const AVRational maxq = { max / (scale ? scale : 1), 1 };
            CHECK_COMMON(FMT("scale_%s", type), t, t, {
                .op = SWS_OP_CLAMP,
                .type = t,
                .clamp.max = { maxq, maxq, maxq, maxq },
            }, {
                .op = SWS_OP_SCALE,
                .type = t,
                .scale.factor = { scale, 1 },
            });
        } else {
            CHECK_COMMON(FMT("scale_%s", type), t, t, {
                .op = SWS_OP_SCALE,
                .type = t,
                .scale.factor = rndq(t),
            });
        }
    }
}

void checkasm_check_sw_ops(void)
{
    check_read_write();
    report("read_write");
    check_swap_bytes();
    report("swap_bytes");
    check_pack_unpack();
    report("pack_unpack");
    check_clear();
    report("clear");
    check_shift();
    report("shift");
    check_swizzle();
    report("swizzle");
    check_convert();
    report("convert");
    check_dither();
    report("dither");
    check_clamp();
    report("clamp");
    check_linear();
    report("linear");
    check_scale();
    report("scale");
}
