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

static float rndf(void)
{
    union { uint32_t u; float f; } x;
    do {
        x.u = rnd();
    } while (!isnormal(x.f));
    return x.f;
}

static void fill32f(float *line, int num, unsigned range)
{
    const float scale = (float) range / UINT32_MAX;
    for (int i = 0; i < num; i++)
        line[i] = range ? scale * rnd() : rndf();
}

static void fill32(uint32_t *line, int num, unsigned range)
{
    for (int i = 0; i < num; i++)
        line[i] = range ? rnd() % (range + 1) : rnd();
}

static void fill16(uint16_t *line, int num, unsigned range)
{
    if (!range) {
        fill32((uint32_t *) line, AV_CEIL_RSHIFT(num, 1), 0);
    } else {
        for (int i = 0; i < num; i++)
            line[i] = rnd() % (range + 1);
    }
}

static void fill8(uint8_t *line, int num, unsigned range)
{
    if (!range) {
        fill32((uint32_t *) line, AV_CEIL_RSHIFT(num, 2), 0);
    } else {
        for (int i = 0; i < num; i++)
            line[i] = rnd() % (range + 1);
    }
}

static void check_ops(const char *report, unsigned range, const SwsOp *ops)
{
    SwsContext *ctx = sws_alloc_context();
    SwsOpChain chain0 = {0}, chain1 = {0};
    SwsOpExec exec0 = {0}, exec1 = {0};
    SwsOpList oplist = { .ops = (SwsOp *) ops };
    int pixel_bits_in, pixel_bits_out;
    const SwsOp *read_op, *write_op;

    declare_func(void, const SwsOpExec *exec, const SwsOpImpl *impl);

    DECLARE_ALIGNED_64(char, src0)[4][MAX_BLOCK_H][4 * MAX_BLOCK_W * sizeof(uint32_t)];
    DECLARE_ALIGNED_64(char, src1)[4][MAX_BLOCK_H][4 * MAX_BLOCK_W * sizeof(uint32_t)];
    DECLARE_ALIGNED_64(char, dst0)[4][MAX_BLOCK_H][4 * MAX_BLOCK_W * sizeof(uint32_t)];
    DECLARE_ALIGNED_64(char, dst1)[4][MAX_BLOCK_H][4 * MAX_BLOCK_W * sizeof(uint32_t)];

    if (!ctx)
        return;
    ctx->flags = SWS_BITEXACT;

    read_op = &ops[0];
    for (oplist.num_ops = 0; ops[oplist.num_ops].op; oplist.num_ops++)
        write_op = &ops[oplist.num_ops];
    pixel_bits_in  = rw_pixel_bits(read_op);
    pixel_bits_out = rw_pixel_bits(write_op);

    for (int p = 0; p < 4; p++) {
        for (int y = 0; y < MAX_BLOCK_H; y++) {
            void *line = src0[p][y];
            switch (read_op->type) {
            case U8:    fill8(line, sizeof(src0[p][y]) /  sizeof(uint8_t), range); break;
            case U16:  fill16(line, sizeof(src0[p][y]) / sizeof(uint16_t), range); break;
            case U32:  fill32(line, sizeof(src0[p][y]) / sizeof(uint32_t), range); break;
            case F32: fill32f(line, sizeof(src0[p][y]) / sizeof(uint32_t), range); break;
            }
        }
    }

    memcpy(src1, src0, sizeof(src0));
    memset(dst0, 0, sizeof(dst0));
    memset(dst1, 0, sizeof(dst1));

    /* Compile `ops` using both the asm and c backends */
    for (int n = 0; ff_sws_op_backends[n]; n++) {
        const SwsOpBackend *backend = ff_sws_op_backends[n];
        const bool is_ref = !strcmp(backend->name, "c");
        if (is_ref || !chain1.entry) {
            SwsOpChain chain;
            int ret = ff_sws_ops_compile_backend(ctx, backend, &oplist, &chain);
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

    if (check_func(chain1.entry, "%s_%dx%d", report, exec1.block_w, exec1.block_h)) {
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
                checkasm_check(uint32_t, (void *) dst0[i], exec0.out_stride[i],
                                         (void *) dst1[i], exec1.out_stride[i],
                                         exec1.w, exec1.h, name);
            case F32:
                checkasm_check(float, (void *) dst0[i], exec0.out_stride[i],
                                      (void *) dst1[i], exec1.out_stride[i],
                                      exec1.w, exec1.h, name);
                break;
            }
        }

        /* Check for over-write */
        for (int y = 0; y < exec1.block_h; y++) {
            for (int p = 0; p < 4; p++) {
                const int base = exec1.w * pixel_bits_out >> 3;
                for (int i = base; i < FF_ARRAY_ELEMS(dst1[p][y]); i++) {
                    if (dst1[p][y][i] != 0) {
                        fprintf(stderr, "Overwrite detected at dst[%d][%d][%d] = 0x%02x\n",
                                p, y, i, dst1[p][y][i]);
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
    sws_free_context(&ctx);
}

#define CHECK_RANGE(NAME, RANGE, N_IN, N_OUT, IN, OUT, ...)                     \
  do {                                                                          \
    check_ops(NAME, RANGE, (SwsOp[]) {                                          \
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

#define CHECK_COMMON_RANGE(NAME, RANGE, IN, OUT, ...)                           \
    CHECK_RANGE(FMT("%s_p1000", NAME), RANGE, 1, 1, IN, OUT, __VA_ARGS__);      \
    CHECK_RANGE(FMT("%s_p1110", NAME), RANGE, 3, 3, IN, OUT, __VA_ARGS__);      \
    CHECK_RANGE(FMT("%s_p1111", NAME), RANGE, 4, 4, IN, OUT, __VA_ARGS__);      \
    CHECK_RANGE(FMT("%s_p1001", NAME), RANGE, 4, 2, IN, OUT, __VA_ARGS__, {     \
        .op = SWS_OP_SWIZZLE,                                                   \
        .type = OUT,                                                            \
        .swizzle = SWS_SWIZZLE(0, 3, 1, 2),                                     \
    })

#define CHECK(NAME, N_IN, N_OUT, IN, OUT, ...) \
    CHECK_RANGE(NAME, 0, N_IN, N_OUT, IN, OUT, __VA_ARGS__)

#define CHECK_COMMON(NAME, IN, OUT, ...) \
    CHECK_COMMON_RANGE(NAME, 0, IN, OUT, __VA_ARGS__)

static void check_read_write(void)
{
    for (SwsPixelType t = U8; t < SWS_PIXEL_TYPE_NB; t++) {
        const char *type = ff_sws_pixel_type_name(t);
        for (int i = 1; i <= 4; i++) {
            /* Test N->N planar read/write */
            for (int o = 1; o <= i; o++) {
                check_ops(FMT("rw_%d_%d_%s", i, o, type), 0, (SwsOp[]) {
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

            check_ops(FMT("read_packed%d_%s", i, type), 0, (SwsOp[]) {
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

            check_ops(FMT("write_packed%d_%s", i, type), 0, (SwsOp[]) {
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
        { U8, {{ 3,  3,  2 }}},
        { U8, {{ 2,  3,  3 }}},
        { U8, {{ 1,  2,  1 }}},
        {U16, {{ 5,  6,  5 }}},
        {U16, {{ 5,  5,  5 }}},
        {U16, {{ 4,  4,  4 }}},
        {U32, {{ 2, 10, 10, 10 }}},
        {U32, {{10, 10, 10,  2 }}},
    };

    for (int i = 0; i < FF_ARRAY_ELEMS(patterns); i++) {
        const SwsPixelType type = patterns[i].type;
        const SwsPackOp pack = patterns[i].op;
        const int num = pack.pattern[3] ? 4 : 3;
        const char *pat = FMT("%d%d%d%d", pack.pattern[0], pack.pattern[1],
                                          pack.pattern[2], pack.pattern[3]);

        CHECK(FMT("pack_%s", pat), num, 1, type, type, {
            .op   = SWS_OP_PACK,
            .type = type,
            .pack = pack,
        });

        CHECK(FMT("unpack_%s", pat), 1, num, type, type, {
            .op   = SWS_OP_UNPACK,
            .type = type,
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
            const AVRational zero   = (AVRational) { 0, 1};
            const AVRational none = {0};

            const SwsConst patterns[] = {
                /* Zero only */
                {.q4 = {   none,   none,   none,   zero }},
                {.q4 = {   zero,   none,   none,   none }},
                /* Alpha only */
                {.q4 = {   none,   none,   none,  alpha }},
                {.q4 = {  alpha,   none,   none,   none }},
                /* Chroma only */
                {.q4 = { chroma, chroma,   none,   none }},
                {.q4 = {   none, chroma, chroma,   none }},
                {.q4 = {   none,   none, chroma, chroma }},
                {.q4 = { chroma,   none, chroma,   none }},
                {.q4 = {   none, chroma,   none, chroma }},
                /* Alpha+chroma */
                {.q4 = { chroma, chroma,   none,  alpha }},
                {.q4 = {   none, chroma, chroma,  alpha }},
                {.q4 = {  alpha,   none, chroma, chroma }},
                {.q4 = { chroma,   none, chroma,  alpha }},
                {.q4 = {  alpha, chroma,   none, chroma }},
                /* Random values */
                {.q4 = { none, rndq(t), rndq(t), rndq(t) }},
                {.q4 = { none, rndq(t), rndq(t), rndq(t) }},
                {.q4 = { none, rndq(t), rndq(t), rndq(t) }},
                {.q4 = { none, rndq(t), rndq(t), rndq(t) }},
            };

            for (int i = 0; i < FF_ARRAY_ELEMS(patterns); i++) {
                CHECK(FMT("clear_pattern_%s[%d]", type, i), 4, 4, t, t, {
                    .op   = SWS_OP_CLEAR,
                    .type = t,
                    .c    = patterns[i],
                });
            }
        } else if (!ff_sws_pixel_type_is_int(t)) {
            /* Floating point YUV doesn't exist, only alpha needs to be cleared */
            CHECK(FMT("clear_alpha_%s", type), 4, 4, t, t, {
                .op      = SWS_OP_CLEAR,
                .type    = t,
                .c.q4[3] = { 0, 1 },
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
                .op   = SWS_OP_LSHIFT,
                .type = t,
                .c.u  = shift,
            });

            CHECK_COMMON(FMT("rshift%d_%s", shift, type), t, t, {
                .op   = SWS_OP_RSHIFT,
                .type = t,
                .c.u  = shift,
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
            {3, 0, 1, 2},
            {3, 0, 2, 1},
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
            {1, 3, 2, 0},
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
            } else if (isize > osize || !ff_sws_pixel_type_is_int(i)) {
                uint32_t range = (1 << osize * 8) - 1;
                CHECK_COMMON_RANGE(name, range, i, o, {
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
            if (!matrix) {
                fail();
                return;
            }

            if (size == 1) {
                matrix[0] = (AVRational) { 1, 2 };
            } else {
                for (int i = 0; i < size * size; i++)
                    matrix[i] = rndq(t);
            }

            CHECK_COMMON(FMT("dither_%dx%d_%s", size, size, type), t, t, {
                .op = SWS_OP_DITHER,
                .type = t,
                .dither.size_log2 = size_log2,
                .dither.matrix = matrix,
            });

            av_refstruct_unref(&matrix);
        }
    }
}

static void check_min_max(void)
{
    for (SwsPixelType t = U8; t < SWS_PIXEL_TYPE_NB; t++) {
        const char *type = ff_sws_pixel_type_name(t);
        CHECK_COMMON(FMT("min_%s", type), t, t, {
            .op = SWS_OP_MIN,
            .type = t,
            .c.q4 = { rndq(t), rndq(t), rndq(t), rndq(t) },
        });

        CHECK_COMMON(FMT("max_%s", type), t, t, {
            .op = SWS_OP_MAX,
            .type = t,
            .c.q4 = { rndq(t), rndq(t), rndq(t), rndq(t) },
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
            const unsigned range = max / (scale ? scale : 1);
            CHECK_COMMON_RANGE(FMT("scale_%s", type), range, t, t, {
                .op   = SWS_OP_SCALE,
                .type = t,
                .c.q  = { scale, 1 },
            });
        } else {
            CHECK_COMMON(FMT("scale_%s", type), t, t, {
                .op   = SWS_OP_SCALE,
                .type = t,
                .c.q  = rndq(t),
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
    check_min_max();
    report("min_max");
    check_linear();
    report("linear");
    check_scale();
    report("scale");
}
