/*
 * Dither pattern generators.
 *
 * Copyright (c) 2025 Niklas Haas
 * Copyright (c) 2013 Wessel Dankers <wsl@fruit.je>
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

#include <float.h>
#include <inttypes.h>
#include <math.h>
#include <stdbool.h>
#include <string.h>

#include "libavutil/error.h"
#include "libavutil/mem.h"

#include "dither.h"

static int find_voids(const uint32_t *array, int size, int *candidates)
{
    uint32_t min = UINT32_MAX;
    int num = 0;

    for (int i = 0; i < size; i++) {
        if (array[i] > min)
            continue;
        if (array[i] < min) {
            min = array[i];
            num = 0;
        }

        candidates[num++] = i;
    }

    return num;
}

static av_always_inline uint32_t pcg(uint64_t *s)
{
    const uint32_t x = ((*s >> 18) ^ *s) >> 27;
    const uint32_t r = *s >> 59;
    *s = *s * 6364136223846793005ULL + 1;
    return (x >> r) | (x << ((-r) & 31));
}

int ff_sws_generate_blue_noise(uint16_t *data, int size_log2)
{
    if (size_log2 > 8)
        return AVERROR(EINVAL);
    if (!size_log2) {
        *data = 0;
        return 0;
    }

    const int N = 1 << size_log2;
    const int radius = N / 2 - 1;
    const int ksize  = radius * 2 + 1;
    const int ksize2 = ksize * ksize;
    int ret = 0;

    #define IDX(x, y) ((x) | ((y) << size_log2))
    uint32_t *kernel = av_calloc(sizeof(uint32_t), N * N);
    uint32_t *energy = av_malloc_array(sizeof(uint32_t), N * N);
    int *candidates  = av_malloc_array(sizeof(int), N * N);
    if (!kernel || !energy || !candidates) {
        ret = AVERROR(ENOMEM);
        goto fail;
    }

    /* Generate 2D exponential decay distribution */
    const double alpha = -log(1.5 / (double) UINT32_MAX * ksize2) / radius;
    for (int y = 0; y <= radius; y++) {
        for (int x = 0; x <= y; x++) {
            const int x2 = ksize - 1 - x;
            const int y2 = ksize - 1 - y;
            const int dx = x - radius;
            const int dy = y - radius;
            const double d = sqrt(dx * dx + dy * dy);
            const double e = exp(-d * alpha);
            const uint32_t v = e / ksize2 * (double) UINT32_MAX;

            kernel[IDX(x,  y )] =
            kernel[IDX(y,  x )] =
            kernel[IDX(x,  y2)] =
            kernel[IDX(y,  x2)] =
            kernel[IDX(x2, y )] =
            kernel[IDX(y2, x )] =
            kernel[IDX(x2, y2)] =
            kernel[IDX(y2, x2)] = v;
        }
    }

    /**
     * Initialize by placing a single copy of the kernel into the exact center
     * of the image. This saves one iteration, and needing to zero the energy
     * matrix. It's worth pointing out that, because we compute energy on a
     * cyclic matrix, the algorithm is invariant under translation (with the
     * exception of the coordinates chosen by the PRNG sequence), so the exact
     * placement location of the first data point does not matter.
     */
    memset(data, 0, N * N * sizeof(*data));
    memcpy(energy, kernel, N * N * sizeof(*energy));

    /* Determined by fair dice roll */
    uint64_t prng_state = UINT64_C(0x40dfd12f6d0c1e7b);

    for (int i = 1; i < N * N; i++) {
        /* Find the location of all voids (darkest pixels in the energy matrix) */
        int num_voids = find_voids(energy, N * N, candidates);
        int pos = candidates[pcg(&prng_state) % num_voids];
        data[pos] = i;

        /**
         * Update the energy matrix by adding a copy of the kernel into the
         * largest void, wrapping around the edges to make the resulting dither
         * matrix cyclic (i.e. tileable without seams).
         */
        const int pos_x = pos & (N - 1);
        const int pos_y = pos >> size_log2;
        for (int y = 0; y < N; y++) {
            const int yy = (radius - pos_y + y) & (N - 1);
            for (int x = 0; x < N; x++) {
                const int xx = (radius - pos_x + x) & (N - 1);
                energy[IDX(x, y)] += kernel[IDX(xx, yy)];
            }
        }
    }

fail:
    av_free(kernel);
    av_free(energy);
    av_free(candidates);
    #undef IDX
    return ret;
}
