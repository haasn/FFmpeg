/*
 * AOM film grain synthesis
 * Copyright (c) 2023 Niklas Haas <ffmpeg@haasn.xyz>
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

/*
 * Copyright © 2018, Niklas Haas
 * Copyright © 2018, VideoLAN and dav1d authors
 * Copyright © 2018, Two Orioles, LLC
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#undef PIXEL
#undef ENTRY
#undef SCALING_SIZE
#undef av_clip_pixel

#if BITDEPTH > 8
#define PIXEL uint16_t
#define ENTRY int16_t
#define SCALING_SIZE 4096
#define av_clip_pixel(x) av_clip(x, 0, (1 << BITDEPTH) - 1)
#else
#define PIXEL uint8_t
#define ENTRY int8_t
#define SCALING_SIZE 256
#define av_clip_pixel(x) av_clip_uint8(x)
#endif

// Symbols that do not depend on bit depth
#ifndef FN
# define FN(n) AV_JOIN(n ## _, BITDEPTH)
# define BITDEPTH_MAX ((1 << BITDEPTH) - 1)
# define PXSTRIDE(x) ((x) / sizeof(PIXEL))
# define GRAIN_WIDTH 82
# define GRAIN_HEIGHT 73
# define SUB_GRAIN_WIDTH 44
# define SUB_GRAIN_HEIGHT 38
# define FG_BLOCK_SIZE 32
#endif

static void FN(generate_grain_y_c)(ENTRY buf[][GRAIN_WIDTH],
                                   const AVFilmGrainParams *const params)
{
    const AVFilmGrainAOMParams *const data = &params->codec.aom;
    const int bitdepth_min_8 = BITDEPTH - 8;
    unsigned seed = params->seed;
    const int shift = 4 - bitdepth_min_8 + data->grain_scale_shift;
    const int grain_ctr = 128 << bitdepth_min_8;
    const int grain_min = -grain_ctr, grain_max = grain_ctr - 1;

    const int ar_pad = 3;
    const int ar_lag = data->ar_coeff_lag;

    for (int y = 0; y < GRAIN_HEIGHT; y++) {
        for (int x = 0; x < GRAIN_WIDTH; x++) {
            const int value = get_random_number(11, &seed);
            buf[y][x] = round2(gaussian_sequence[ value ], shift);
        }
    }

    for (int y = ar_pad; y < GRAIN_HEIGHT; y++) {
        for (int x = ar_pad; x < GRAIN_WIDTH - ar_pad; x++) {
            const int8_t *coeff = data->ar_coeffs_y;
            int sum = 0, grain;
            for (int dy = -ar_lag; dy <= 0; dy++) {
                for (int dx = -ar_lag; dx <= ar_lag; dx++) {
                    if (!dx && !dy)
                        break;
                    sum += *(coeff++) * buf[y + dy][x + dx];
                }
            }

            grain = buf[y][x] + round2(sum, data->ar_coeff_shift);
            buf[y][x] = av_clip(grain, grain_min, grain_max);
        }
    }
}

static void
FN(generate_grain_uv_c)(ENTRY buf[][GRAIN_WIDTH],
                        const ENTRY buf_y[][GRAIN_WIDTH],
                        const AVFilmGrainParams *const params, const intptr_t uv,
                        const int subx, const int suby)
{
    const AVFilmGrainAOMParams *const data = &params->codec.aom;
    const int bitdepth_min_8 = BITDEPTH - 8;
    unsigned seed = params->seed ^ (uv ? 0x49d8 : 0xb524);
    const int shift = 4 - bitdepth_min_8 + data->grain_scale_shift;
    const int grain_ctr = 128 << bitdepth_min_8;
    const int grain_min = -grain_ctr, grain_max = grain_ctr - 1;

    const int chromaW = subx ? SUB_GRAIN_WIDTH  : GRAIN_WIDTH;
    const int chromaH = suby ? SUB_GRAIN_HEIGHT : GRAIN_HEIGHT;

    const int ar_pad = 3;
    const int ar_lag = data->ar_coeff_lag;

    for (int y = 0; y < chromaH; y++) {
        for (int x = 0; x < chromaW; x++) {
            const int value = get_random_number(11, &seed);
            buf[y][x] = round2(gaussian_sequence[ value ], shift);
        }
    }

    for (int y = ar_pad; y < chromaH; y++) {
        for (int x = ar_pad; x < chromaW - ar_pad; x++) {
            const int8_t *coeff = data->ar_coeffs_uv[uv];
            int sum = 0, grain;
            for (int dy = -ar_lag; dy <= 0; dy++) {
                for (int dx = -ar_lag; dx <= ar_lag; dx++) {
                    // For the final (current) pixel, we need to add in the
                    // contribution from the luma grain texture
                    if (!dx && !dy) {
                        const int lumaX = ((x - ar_pad) << subx) + ar_pad;
                        const int lumaY = ((y - ar_pad) << suby) + ar_pad;
                        int luma = 0;
                        if (!data->num_y_points)
                            break;
                        for (int i = 0; i <= suby; i++) {
                            for (int j = 0; j <= subx; j++) {
                                luma += buf_y[lumaY + i][lumaX + j];
                            }
                        }
                        luma = round2(luma, subx + suby);
                        sum += luma * (*coeff);
                        break;
                    }

                    sum += *(coeff++) * buf[y + dy][x + dx];
                }
            }

            grain = buf[y][x] + round2(sum, data->ar_coeff_shift);
            buf[y][x] = av_clip(grain, grain_min, grain_max);
        }
    }
}

// samples from the correct block of a grain LUT, while taking into account the
// offsets provided by the offsets cache
static inline ENTRY FN(sample_lut)(const ENTRY grain_lut[][GRAIN_WIDTH],
                                   const int offsets[2][2],
                                   const int subx, const int suby,
                                   const int bx, const int by,
                                   const int x, const int y)
{
    const int randval = offsets[bx][by];
    const int offx = 3 + (2 >> subx) * (3 + (randval >> 4));
    const int offy = 3 + (2 >> suby) * (3 + (randval & 0xF));
    return grain_lut[offy + y + (FG_BLOCK_SIZE >> suby) * by]
                    [offx + x + (FG_BLOCK_SIZE >> subx) * bx];
}

static void FN(fgy_32x32xn_c)(PIXEL *const dst_row, const PIXEL *const src_row,
                              const ptrdiff_t stride,
                              const AVFilmGrainParams *const params, const size_t pw,
                              const uint8_t scaling[SCALING_SIZE],
                              const ENTRY grain_lut[][GRAIN_WIDTH],
                              const int bh, const int row_num)
{
    const AVFilmGrainAOMParams *const data = &params->codec.aom;
    const int rows = 1 + (data->overlap_flag && row_num > 0);
    const int bitdepth_min_8 = BITDEPTH - 8;
    const int grain_ctr = 128 << bitdepth_min_8;
    const int grain_min = -grain_ctr, grain_max = grain_ctr - 1;
    unsigned seed[2];
    int offsets[2 /* col offset */][2 /* row offset */];

    int min_value, max_value;
    if (data->limit_output_range) {
        min_value = 16 << bitdepth_min_8;
        max_value = 235 << bitdepth_min_8;
    } else {
        min_value = 0;
        max_value = BITDEPTH_MAX;
    }

    // seed[0] contains the current row, seed[1] contains the previous
    for (int i = 0; i < rows; i++) {
        seed[i] = params->seed;
        seed[i] ^= (((row_num - i) * 37  + 178) & 0xFF) << 8;
        seed[i] ^= (((row_num - i) * 173 + 105) & 0xFF);
    }

    av_assert1(stride % (FG_BLOCK_SIZE * sizeof(PIXEL)) == 0);

    // process this row in FG_BLOCK_SIZE^2 blocks
    for (unsigned bx = 0; bx < pw; bx += FG_BLOCK_SIZE) {
        const int bw = FFMIN(FG_BLOCK_SIZE, (int) pw - bx);
        const PIXEL *src;
        PIXEL *dst;
        int noise;

        // x/y block offsets to compensate for overlapped regions
        const int ystart = data->overlap_flag && row_num ? FFMIN(2, bh) : 0;
        const int xstart = data->overlap_flag && bx      ? FFMIN(2, bw) : 0;

        static const int w[2][2] = { { 27, 17 }, { 17, 27 } };

        if (data->overlap_flag && bx) {
            // shift previous offsets left
            for (int i = 0; i < rows; i++)
                offsets[1][i] = offsets[0][i];
        }

        // update current offsets
        for (int i = 0; i < rows; i++)
            offsets[0][i] = get_random_number(8, &seed[i]);

#define add_noise_y(x, y, grain)                                        \
        src = src_row + (y) * PXSTRIDE(stride) + (x) + bx;              \
        dst = dst_row + (y) * PXSTRIDE(stride) + (x) + bx;              \
        noise = round2(scaling[ *src ] * (grain), data->scaling_shift); \
        *dst = av_clip(*src + noise, min_value, max_value);

        for (int y = ystart; y < bh; y++) {
            // Non-overlapped image region (straightforward)
            for (int x = xstart; x < bw; x++) {
                int grain = FN(sample_lut)(grain_lut, offsets, 0, 0, 0, 0, x, y);
                add_noise_y(x, y, grain);
            }

            // Special case for overlapped column
            for (int x = 0; x < xstart; x++) {
                int grain = FN(sample_lut)(grain_lut, offsets, 0, 0, 0, 0, x, y);
                int old   = FN(sample_lut)(grain_lut, offsets, 0, 0, 1, 0, x, y);
                grain = round2(old * w[x][0] + grain * w[x][1], 5);
                grain = av_clip(grain, grain_min, grain_max);
                add_noise_y(x, y, grain);
            }
        }

        for (int y = 0; y < ystart; y++) {
            // Special case for overlapped row (sans corner)
            for (int x = xstart; x < bw; x++) {
                int grain = FN(sample_lut)(grain_lut, offsets, 0, 0, 0, 0, x, y);
                int old   = FN(sample_lut)(grain_lut, offsets, 0, 0, 0, 1, x, y);
                grain = round2(old * w[y][0] + grain * w[y][1], 5);
                grain = av_clip(grain, grain_min, grain_max);
                add_noise_y(x, y, grain);
            }

            // Special case for doubly-overlapped corner
            for (int x = 0; x < xstart; x++) {
                int grain = FN(sample_lut)(grain_lut, offsets, 0, 0, 0, 0, x, y);
                int top = FN(sample_lut)(grain_lut, offsets, 0, 0, 0, 1, x, y);
                int old = FN(sample_lut)(grain_lut, offsets, 0, 0, 1, 1, x, y);

                // Blend the top pixel with the top left block
                top = round2(old * w[x][0] + top * w[x][1], 5);
                top = av_clip(top, grain_min, grain_max);

                // Blend the current pixel with the left block
                old = FN(sample_lut)(grain_lut, offsets, 0, 0, 1, 0, x, y);
                grain = round2(old * w[x][0] + grain * w[x][1], 5);
                grain = av_clip(grain, grain_min, grain_max);

                // Mix the row rows together and apply grain
                grain = round2(top * w[y][0] + grain * w[y][1], 5);
                grain = av_clip(grain, grain_min, grain_max);
                add_noise_y(x, y, grain);
            }
        }
    }
}

static void
FN(fguv_32x32xn_c)(PIXEL *const dst_row, const PIXEL *const src_row,
                   const ptrdiff_t stride, const AVFilmGrainParams *const params,
                   const size_t pw, const uint8_t scaling[SCALING_SIZE],
                   const ENTRY grain_lut[][GRAIN_WIDTH], const int bh,
                   const int row_num, const PIXEL *const luma_row,
                   const ptrdiff_t luma_stride, const int uv, const int is_id,
                   const int sx, const int sy)
{
    const AVFilmGrainAOMParams *const data = &params->codec.aom;
    const int rows = 1 + (data->overlap_flag && row_num > 0);
    const int bitdepth_min_8 = BITDEPTH - 8;
    const int grain_ctr = 128 << bitdepth_min_8;
    const int grain_min = -grain_ctr, grain_max = grain_ctr - 1;
    unsigned seed[2];
    int offsets[2 /* col offset */][2 /* row offset */];

    int min_value, max_value;
    if (data->limit_output_range) {
        min_value = 16 << bitdepth_min_8;
        max_value = (is_id ? 235 : 240) << bitdepth_min_8;
    } else {
        min_value = 0;
        max_value = BITDEPTH_MAX;
    }

    // seed[0] contains the current row, seed[1] contains the previous
    for (int i = 0; i < rows; i++) {
        seed[i] = params->seed;
        seed[i] ^= (((row_num - i) * 37  + 178) & 0xFF) << 8;
        seed[i] ^= (((row_num - i) * 173 + 105) & 0xFF);
    }

    av_assert1(stride % (FG_BLOCK_SIZE * sizeof(PIXEL)) == 0);

    // process this row in FG_BLOCK_SIZE^2 blocks (subsampled)
    for (unsigned bx = 0; bx < pw; bx += FG_BLOCK_SIZE >> sx) {
        const int bw = FFMIN(FG_BLOCK_SIZE >> sx, (int)(pw - bx));
        int val, lx, ly, noise;
        const PIXEL *src, *luma;
        PIXEL *dst, avg;

        // x/y block offsets to compensate for overlapped regions
        const int ystart = data->overlap_flag && row_num ? FFMIN(2 >> sy, bh) : 0;
        const int xstart = data->overlap_flag && bx      ? FFMIN(2 >> sx, bw) : 0;

        static const int w[2 /* sub */][2 /* off */][2] = {
            { { 27, 17 }, { 17, 27 } },
            { { 23, 22 } },
        };

        if (data->overlap_flag && bx) {
            // shift previous offsets left
            for (int i = 0; i < rows; i++)
                offsets[1][i] = offsets[0][i];
        }

        // update current offsets
        for (int i = 0; i < rows; i++)
            offsets[0][i] = get_random_number(8, &seed[i]);

#define add_noise_uv(x, y, grain)                                                    \
            lx = (bx + x) << sx;                                                     \
            ly = y << sy;                                                            \
            luma = luma_row + ly * PXSTRIDE(luma_stride) + lx;                       \
            avg = luma[0];                                                           \
            if (sx)                                                                  \
                avg = (avg + luma[1] + 1) >> 1;                                      \
            src = src_row + (y) * PXSTRIDE(stride) + (bx + (x));                     \
            dst = dst_row + (y) * PXSTRIDE(stride) + (bx + (x));                     \
            val = avg;                                                               \
            if (!data->chroma_scaling_from_luma) {                                   \
                const int combined = avg * data->uv_mult_luma[uv] +                  \
                               *src * data->uv_mult[uv];                             \
                val = av_clip_pixel( (combined >> 6) +                               \
                                     (data->uv_offset[uv] * (1 << bitdepth_min_8)) );\
            }                                                                        \
            noise = round2(scaling[ val ] * (grain), data->scaling_shift);           \
            *dst = av_clip(*src + noise, min_value, max_value);

        for (int y = ystart; y < bh; y++) {
            // Non-overlapped image region (straightforward)
            for (int x = xstart; x < bw; x++) {
                int grain = FN(sample_lut)(grain_lut, offsets, sx, sy, 0, 0, x, y);
                add_noise_uv(x, y, grain);
            }

            // Special case for overlapped column
            for (int x = 0; x < xstart; x++) {
                int grain = FN(sample_lut)(grain_lut, offsets, sx, sy, 0, 0, x, y);
                int old   = FN(sample_lut)(grain_lut, offsets, sx, sy, 1, 0, x, y);
                grain = round2(old * w[sx][x][0] + grain * w[sx][x][1], 5);
                grain = av_clip(grain, grain_min, grain_max);
                add_noise_uv(x, y, grain);
            }
        }

        for (int y = 0; y < ystart; y++) {
            // Special case for overlapped row (sans corner)
            for (int x = xstart; x < bw; x++) {
                int grain = FN(sample_lut)(grain_lut, offsets, sx, sy, 0, 0, x, y);
                int old   = FN(sample_lut)(grain_lut, offsets, sx, sy, 0, 1, x, y);
                grain = round2(old * w[sy][y][0] + grain * w[sy][y][1], 5);
                grain = av_clip(grain, grain_min, grain_max);
                add_noise_uv(x, y, grain);
            }

            // Special case for doubly-overlapped corner
            for (int x = 0; x < xstart; x++) {
                int top = FN(sample_lut)(grain_lut, offsets, sx, sy, 0, 1, x, y);
                int old = FN(sample_lut)(grain_lut, offsets, sx, sy, 1, 1, x, y);
                int grain = FN(sample_lut)(grain_lut, offsets, sx, sy, 0, 0, x, y);

                // Blend the top pixel with the top left block
                top = round2(old * w[sx][x][0] + top * w[sx][x][1], 5);
                top = av_clip(top, grain_min, grain_max);

                // Blend the current pixel with the left block
                old = FN(sample_lut)(grain_lut, offsets, sx, sy, 1, 0, x, y);
                grain = round2(old * w[sx][x][0] + grain * w[sx][x][1], 5);
                grain = av_clip(grain, grain_min, grain_max);

                // Mix the row rows together and apply to image
                grain = round2(top * w[sy][y][0] + grain * w[sy][y][1], 5);
                grain = av_clip(grain, grain_min, grain_max);
                add_noise_uv(x, y, grain);
            }
        }
    }
}

static void FN(generate_scaling)(const uint8_t points[][2], const int num,
                                 uint8_t scaling[SCALING_SIZE])
{
    const int shift_x = BITDEPTH - 8;
    const int scaling_size = 1 << BITDEPTH;
    const int max_value = points[num - 1][0] << shift_x;
    av_assert0(scaling_size <= SCALING_SIZE);

    if (num == 0) {
        memset(scaling, 0, scaling_size);
        return;
    }

    // Fill up the preceding entries with the initial value
    memset(scaling, points[0][1], points[0][0] << shift_x);

    // Linearly interpolate the values in the middle
    for (int i = 0; i < num - 1; i++) {
        const int bx = points[i][0];
        const int by = points[i][1];
        const int ex = points[i+1][0];
        const int ey = points[i+1][1];
        const int dx = ex - bx;
        const int dy = ey - by;
        const int delta = dy * ((0x10000 + (dx >> 1)) / dx);
        av_assert1(dx > 0);
        for (int x = 0, d = 0x8000; x < dx; x++) {
            scaling[(bx + x) << shift_x] = by + (d >> 16);
            d += delta;
        }
    }

    // Fill up the remaining entries with the final value
    memset(&scaling[max_value], points[num - 1][1], scaling_size - max_value);

#if BITDEPTH != 8
    for (int i = 0; i < num - 1; i++) {
        const int pad = 1 << shift_x, rnd = pad >> 1;
        const int bx = points[i][0] << shift_x;
        const int ex = points[i+1][0] << shift_x;
        const int dx = ex - bx;
        for (int x = 0; x < dx; x += pad) {
            const int range = scaling[bx + x + pad] - scaling[bx + x];
            for (int n = 1, r = rnd; n < pad; n++) {
                r += range;
                scaling[bx + x + n] = scaling[bx + x] + (r >> shift_x);
            }
        }
    }
#endif
}

static av_always_inline void
FN(apply_grain_row)(AVFrame *out, const AVFrame *in,
                    const int ss_x, const int ss_y,
                    const uint8_t scaling[3][SCALING_SIZE],
                    const ENTRY grain_lut[3][GRAIN_HEIGHT+1][GRAIN_WIDTH],
                    const AVFilmGrainParams *params,
                    const int row)
{
    // Synthesize grain for the affected planes
    const AVFilmGrainAOMParams *const data = &params->codec.aom;
    const int cpw = (out->width + ss_x) >> ss_x;
    const int is_id = out->colorspace == AVCOL_SPC_RGB;
    const int bh = (FFMIN(out->height - row * FG_BLOCK_SIZE, FG_BLOCK_SIZE) + ss_y) >> ss_y;
    const ptrdiff_t uv_off = row * FG_BLOCK_SIZE * PXSTRIDE(out->linesize[1]) >> ss_y;
    PIXEL *const luma_src =
        ((PIXEL *) in->data[0]) + row * FG_BLOCK_SIZE * PXSTRIDE(in->linesize[0]);

    if (data->num_y_points) {
        const int bh = FFMIN(out->height - row * FG_BLOCK_SIZE, FG_BLOCK_SIZE);
        const ptrdiff_t off = row * FG_BLOCK_SIZE * PXSTRIDE(out->linesize[0]);
        FN(fgy_32x32xn_c)(((PIXEL *) out->data[0]) + off, luma_src,
                          out->linesize[0], params, out->width, scaling[0],
                          grain_lut[0], bh, row);
    }

    if (!data->num_uv_points[0] && !data->num_uv_points[1] &&
        !data->chroma_scaling_from_luma)
    {
        return;
    }

    // extend padding pixels
    if (out->width & ss_x) {
        PIXEL *ptr = luma_src;
        for (int y = 0; y < bh; y++) {
            ptr[out->width] = ptr[out->width - 1];
            ptr += PXSTRIDE(in->linesize[0]) << ss_y;
        }
    }

    if (data->chroma_scaling_from_luma) {
        for (int pl = 0; pl < 2; pl++)
            FN(fguv_32x32xn_c)(((PIXEL *) out->data[1 + pl]) + uv_off,
                               ((const PIXEL *) in->data[1 + pl]) + uv_off,
                               in->linesize[1], params, cpw, scaling[0],
                               grain_lut[1 + pl], bh, row, luma_src,
                               in->linesize[0], pl, is_id, ss_x, ss_y);
    } else {
        for (int pl = 0; pl < 2; pl++) {
            if (data->num_uv_points[pl]) {
                FN(fguv_32x32xn_c)(((PIXEL *) out->data[1 + pl]) + uv_off,
                                   ((const PIXEL *) in->data[1 + pl]) + uv_off,
                                   in->linesize[1], params, cpw, scaling[1 + pl],
                                   grain_lut[1 + pl], bh, row, luma_src,
                                   in->linesize[0], pl, is_id, ss_x, ss_y);
            }
        }
    }
}

static int FN(apply_film_grain)(AVFrame *out_frame, const AVFrame *in_frame,
                                const AVFilmGrainParams *params)
{
    ENTRY grain_lut[3][GRAIN_HEIGHT + 1][GRAIN_WIDTH];
    uint8_t scaling[3][SCALING_SIZE];

    const AVFilmGrainAOMParams *const data = &params->codec.aom;
    const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(out_frame->format);
    const int rows = AV_CEIL_RSHIFT(out_frame->height, 5); /* log2(FG_BLOCK_SIZE) */
    const int subx = desc->log2_chroma_w, suby = desc->log2_chroma_h;
    av_assert0(params->type == AV_FILM_GRAIN_PARAMS_AV1);
    av_assert0(out_frame->format == in_frame->format);
    if (in_frame->format != AV_PIX_FMT_YUV420P10LE)
        return AVERROR_PATCHWELCOME;

    // Generate grain LUTs as needed
    FN(generate_grain_y_c)(grain_lut[0], params);
    if (data->num_uv_points[0] || data->chroma_scaling_from_luma)
        FN(generate_grain_uv_c)(grain_lut[1], grain_lut[0], params, 0, subx, suby);
    if (data->num_uv_points[1] || data->chroma_scaling_from_luma)
        FN(generate_grain_uv_c)(grain_lut[2], grain_lut[0], params, 1, subx, suby);

    // Generate scaling LUTs as needed
    if (data->num_y_points || data->chroma_scaling_from_luma)
        FN(generate_scaling)(data->y_points, data->num_y_points, scaling[0]);
    if (data->num_uv_points[0])
        FN(generate_scaling)(data->uv_points[0], data->num_uv_points[0], scaling[1]);
    if (data->num_uv_points[1])
        FN(generate_scaling)(data->uv_points[1], data->num_uv_points[1], scaling[2]);

    // Copy over the non-modified planes
    if (!data->num_y_points) {
        av_image_copy_plane(out_frame->data[0], out_frame->linesize[0],
                            in_frame->data[0], in_frame->linesize[0],
                            out_frame->width * sizeof(PIXEL), out_frame->height);
    }
    for (int uv = 0; uv < 2; uv++) {
        if (!data->num_uv_points[uv]) {
            av_image_copy_plane(out_frame->data[1+uv], out_frame->linesize[1+uv],
                                in_frame->data[1+uv], in_frame->linesize[1+uv],
                                AV_CEIL_RSHIFT(out_frame->width, subx) * sizeof(PIXEL),
                                AV_CEIL_RSHIFT(out_frame->height, suby));
        }
    }

    for (int row = 0; row < rows; row++) {
        FN(apply_grain_row)(out_frame, in_frame, subx, suby, scaling, grain_lut,
                            params, row);
    }

    return 0;
}
