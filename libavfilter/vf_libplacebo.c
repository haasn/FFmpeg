/*
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

#include "libavutil/file.h"
#include "libavutil/opt.h"
#include "internal.h"
#include "scale_eval.h"

#include <libplacebo/renderer.h>
#include <libplacebo/utils/libav.h>
#include <libplacebo/vulkan.h>

typedef struct LibplaceboContext {
    const AVClass *class;

    /* libplacebo */
    struct pl_context *ctx;
    const struct pl_vulkan *vulkan;
    const struct pl_gpu *gpu;
    struct pl_renderer *renderer;
    const struct pl_tex *image_tex[4];
    const struct pl_tex *target_tex[4];

    /* settings */
    char *out_format_string;
    char *w_expr;
    char *h_expr;
    AVRational target_sar;
    float pad_crop_ratio;
    int force_original_aspect_ratio;
    int force_divisible_by;
    int normalize_sar;
    int skip_av1_grain;
    int colorspace;
    int color_range;
    int color_primaries;
    int color_trc;

    /* pl_render_params */
    char *upscaler;
    char *downscaler;
    int lut_entries;
    float antiringing;
    int sigmoid;
    int skip_aa;
    float polar_cutoff;
    int disable_linear;
    int disable_builtin;
    int force_3dlut;
    int force_dither;
    int disable_fbos;

    /* pl_deband_params */
    int deband;
    int deband_iterations;
    float deband_threshold;
    float deband_radius;
    float deband_grain;

    /* pl_color_adjustment */
    float brightness;
    float contrast;
    float saturation;
    float hue;
    float gamma;

    /* pl_peak_detect_params */
    int peakdetect;
    float smoothing;
    float scene_low;
    float scene_high;
    float overshoot;

    /* pl_color_map_params */
    int intent;
    int tonemapping;
    float tonemapping_param;
    float desat_str;
    float desat_exp;
    float desat_base;
    float max_boost;
    int gamut_warning;
    int gamut_clipping;

     /* pl_dither_params */
    int dithering;
    int dither_lut_size;
    int dither_temporal;

    /* pl_cone_params */
    int cones;
    float cone_str;

    /* custom shaders */
    char *shader_path;
    void *shader_bin;
    int shader_bin_len;
    const struct pl_hook *hooks[2];
    int num_hooks;
} LibplaceboContext;

static void pl_av_log(void *log_ctx, enum pl_log_level level, const char *msg)
{
    int av_lev;

    switch (level) {
    case PL_LOG_FATAL:  av_lev = AV_LOG_FATAL;   break;
    case PL_LOG_ERR:    av_lev = AV_LOG_ERROR;   break;
    case PL_LOG_WARN:   av_lev = AV_LOG_WARNING; break;
    case PL_LOG_INFO:   av_lev = AV_LOG_VERBOSE; break;
    case PL_LOG_DEBUG:  av_lev = AV_LOG_DEBUG;   break;
    case PL_LOG_TRACE:  av_lev = AV_LOG_TRACE;   break;
    default: return;
    }

    av_log(log_ctx, av_lev, "%s\n", msg);
}

static int parse_shader(AVFilterContext *avctx, const void *shader, size_t len)
{
    LibplaceboContext *s = avctx->priv;
    const struct pl_hook *hook;

    hook = pl_mpv_user_shader_parse(s->gpu, shader, len);
    if (!hook) {
        av_log(s, AV_LOG_ERROR, "Failed parsing custom shader!\n");
        return AVERROR(EINVAL);
    }

    s->hooks[s->num_hooks++] = hook;
    return 0;
}

#define RET(x)                                                                 \
    do {                                                                       \
        if ((err = (x)) < 0)                                                   \
            goto fail;                                                         \
    } while (0)

static int init(AVFilterContext *avctx)
{
    int err;
    LibplaceboContext *s = avctx->priv;
    uint8_t *buf = NULL;
    size_t buf_len;

    /* Create the main libplacebo context */
    s->ctx = pl_context_create(PL_API_VER, &(struct pl_context_params) {
        .log_cb = pl_av_log,
        .log_priv = s,
        .log_level = PL_LOG_DEBUG,
    });

    if (!s->ctx)
        return AVERROR(ENOMEM);

    /* Create the libplacebo GPU context */
    s->vulkan = pl_vulkan_create(s->ctx, &pl_vulkan_default_params);
    if (!s->vulkan) {
        av_log(s, AV_LOG_ERROR, "Failed creating vulkan device!\n");
        return AVERROR_EXTERNAL;
    }

    /* Create the renderer */
    s->gpu = s->vulkan->gpu;
    s->renderer = pl_renderer_create(s->ctx, s->gpu);

    /* Parse the user shaders, if requested */
    if (s->shader_bin_len)
        RET(parse_shader(avctx, s->shader_bin, s->shader_bin_len));

    if (s->shader_path && s->shader_path[0]) {
        RET(av_file_map(s->shader_path, &buf, &buf_len, 0, s));
        RET(parse_shader(avctx, buf, buf_len));
    }

    err = 0;
    // fall through

fail:
    if (buf)
        av_file_unmap(buf, buf_len);
    return err;
}

static void uninit(AVFilterContext *avctx)
{
    LibplaceboContext *s = avctx->priv;

    for (int i = 0; i < 4; i++) {
        pl_tex_destroy(s->gpu, &s->image_tex[i]);
        pl_tex_destroy(s->gpu, &s->target_tex[i]);
    }
    for (int i = 0; i < s->num_hooks; i++)
        pl_mpv_user_shader_destroy(&s->hooks[i]);
    pl_renderer_destroy(&s->renderer);
    pl_vulkan_destroy(&s->vulkan);
    pl_context_destroy(&s->ctx);
    s->gpu = NULL;
}

static int query_formats(AVFilterContext *avctx)
{
    LibplaceboContext *s = avctx->priv;
    AVFilterFormats *formats = NULL;
    const AVPixFmtDescriptor *desc = NULL;
    int err;

    while ((desc = av_pix_fmt_desc_next(desc))) {
        enum AVPixelFormat pixfmt = av_pix_fmt_desc_get_id(desc);
        if (pl_test_pixfmt(s->gpu, pixfmt)) {
            if ((err = ff_add_format(&formats, pixfmt)) < 0)
                return err;
        }
    }

    return ff_set_common_formats(avctx, formats);
}

static int find_scaler(AVFilterContext *avctx,
                       const struct pl_filter_config **opt,
                       const char *name)
{
    const struct pl_named_filter_config *preset;
    if (!strcmp(name, "help")) {
        av_log(avctx, AV_LOG_INFO, "Available scaler presets:\n");
        for (preset = pl_named_filters; preset->name; preset++)
            av_log(avctx, AV_LOG_INFO, "    %s\n", preset->name);
        return AVERROR_EXIT;
    }

    preset = pl_find_named_filter(name);
    if (!preset) {
        av_log(avctx, AV_LOG_ERROR, "No such scaler preset '%s'.\n", name);
        return AVERROR(EINVAL);
    }

    *opt = preset->filter;
    return 0;
}

static int process_frames(AVFilterContext *avctx, AVFrame *out, AVFrame *in)
{
    int err = 0;
    LibplaceboContext *s = avctx->priv;
    struct pl_render_params params;
    struct pl_frame image, target;

    /* Update render params */
    params = (struct pl_render_params) {
        .lut_entries = s->lut_entries,
        .antiringing_strength = s->antiringing,

        .deband_params = !s->deband ? NULL : &(struct pl_deband_params) {
            .iterations = s->deband_iterations,
            .threshold = s->deband_threshold,
            .radius = s->deband_radius,
            .grain = s->deband_grain,
        },

        .sigmoid_params = s->sigmoid ? &pl_sigmoid_default_params : NULL,

        .color_adjustment = &(struct pl_color_adjustment) {
            .brightness = s->brightness,
            .contrast = s->contrast,
            .saturation = s->saturation,
            .hue = s->hue,
            .gamma = s->gamma,
        },

        .peak_detect_params = !s->peakdetect ? NULL : &(struct pl_peak_detect_params) {
            .smoothing_period = s->smoothing,
            .scene_threshold_low = s->scene_low,
            .scene_threshold_high = s->scene_high,
            .overshoot_margin = s->overshoot,
        },

        .color_map_params = &(struct pl_color_map_params) {
            .intent = s->intent,
            .tone_mapping_algo = s->tonemapping,
            .tone_mapping_param = s->tonemapping_param,
            .desaturation_strength = s->desat_str,
            .desaturation_exponent = s->desat_exp,
            .desaturation_base = s->desat_base,
            .max_boost = s->max_boost,
            .gamut_warning = s->gamut_warning,
            .gamut_clipping = s->gamut_clipping,
        },

        .dither_params = s->dithering < 0 ? NULL : &(struct pl_dither_params) {
            .method = s->dithering,
            .lut_size = s->dither_lut_size,
            .temporal = s->dither_temporal,
        },

        .cone_params = !s->cones ? NULL : &(struct pl_cone_params) {
            .cones = s->cones,
            .strength = s->cone_str,
        },

        .hooks = s->hooks,
        .num_hooks = s->num_hooks,

        .skip_anti_aliasing = s->skip_aa,
        .polar_cutoff = s->polar_cutoff,
        .disable_linear_scaling = s->disable_linear,
        .disable_builtin_scalers = s->disable_builtin,
        .force_3dlut = s->force_3dlut,
        .force_dither = s->force_dither,
        .disable_fbos = s->disable_fbos,
    };

    RET(find_scaler(avctx, &params.upscaler, s->upscaler));
    RET(find_scaler(avctx, &params.downscaler, s->downscaler));

    /* Prepare and upload input frames */

    pl_upload_avframe(s->gpu, &image, s->image_tex, in);

    if (s->skip_av1_grain)
        image.av1_grain = (struct pl_av1_grain_data) {0};

    /* Prepare textures for target frame */

    pl_frame_recreate_from_avframe(s->gpu, &target, s->target_tex, out);
    if (s->target_sar.num) {
        float aspect = pl_rect2df_aspect(&target.crop) * av_q2d(s->target_sar);
        pl_rect2df_aspect_set(&target.crop, aspect, s->pad_crop_ratio);
    }

    /* Perform the actual rendering */
    if (pl_frame_is_cropped(&target))
        pl_frame_clear(s->gpu, &target, (float[3]) {0});
    pl_render_image(s->renderer, &image, &target, &params);
    pl_download_avframe(s->gpu, &target, out);

    /* Flush the command queues for performance */
    pl_gpu_flush(s->gpu);
    return 0;

fail:
    return err;
}

static int filter_frame(AVFilterLink *link, AVFrame *in)
{
    int err;
    AVFilterContext *ctx = link->dst;
    LibplaceboContext *s = ctx->priv;
    AVFilterLink *outlink = ctx->outputs[0];

    AVFrame *out = ff_get_video_buffer(outlink, outlink->w, outlink->h);
    if (!out) {
        err = AVERROR(ENOMEM);
        goto fail;
    }

    err = av_frame_copy_props(out, in);
    if (err < 0)
        goto fail;

    out->width = outlink->w;
    out->height = outlink->h;

    if (s->colorspace >= 0)
        out->colorspace = s->colorspace;
    if (s->color_range >= 0)
        out->color_range = s->color_range;
    if (s->color_trc >= 0)
        out->color_trc = s->color_trc;
    if (s->color_primaries >= 0)
        out->color_primaries = s->color_primaries;

    RET(process_frames(ctx, out, in));

    if (!s->skip_av1_grain)
        av_frame_remove_side_data(out, AV_FRAME_DATA_FILM_GRAIN_PARAMS);

    av_frame_free(&in);

    return ff_filter_frame(outlink, out);

fail:
    av_frame_free(&in);
    av_frame_free(&out);
    return err;
}

static int config_props(AVFilterLink *outlink)
{
    int err;
    AVFilterContext *avctx = outlink->src;
    LibplaceboContext *s   = avctx->priv;
    AVFilterLink *inlink   = outlink->src->inputs[0];
    AVRational scale_sar;

    RET(ff_scale_eval_dimensions(s, s->w_expr, s->h_expr, inlink, outlink,
                                 &outlink->w, &outlink->h));

    ff_scale_adjust_dimensions(inlink, &outlink->w, &outlink->w,
                               s->force_original_aspect_ratio,
                               s->force_divisible_by);

    scale_sar = (AVRational){outlink->h * inlink->w, outlink->w * inlink->h};
    if (inlink->sample_aspect_ratio.num)
        scale_sar = av_mul_q(scale_sar, inlink->sample_aspect_ratio);

    if (s->normalize_sar) {
        /* Apply all SAR during scaling, so we don't need to set the out SAR */
        s->target_sar = scale_sar;
    } else {
        /* This is consistent with other scale_* filters, which only
         * set the outlink SAR to be equal to the scale SAR iff the input SAR
         * was set to something nonzero */
        if (inlink->sample_aspect_ratio.num)
            outlink->sample_aspect_ratio = scale_sar;
    }

    if (s->out_format_string) {
        outlink->format = av_get_pix_fmt(s->out_format_string);
        if (outlink->format == AV_PIX_FMT_NONE) {
            av_log(avctx, AV_LOG_ERROR, "Invalid output format.\n");
            return AVERROR(EINVAL);
        }
    }

    return 0;

fail:
    return err;
}

#define OFFSET(x) offsetof(LibplaceboContext, x)
#define STATIC (AV_OPT_FLAG_FILTERING_PARAM | AV_OPT_FLAG_VIDEO_PARAM)
#define DYNAMIC (STATIC | AV_OPT_FLAG_RUNTIME_PARAM)

static const AVOption libplacebo_options[] = {
    { "w", "Output video width",  OFFSET(w_expr), AV_OPT_TYPE_STRING, {.str = "iw"}, .flags = STATIC },
    { "h", "Output video height", OFFSET(h_expr), AV_OPT_TYPE_STRING, {.str = "ih"}, .flags = STATIC },
    { "format", "Output video format", OFFSET(out_format_string), AV_OPT_TYPE_STRING, .flags = STATIC },
    { "force_original_aspect_ratio", "decrease or increase w/h if necessary to keep the original AR", OFFSET(force_original_aspect_ratio), AV_OPT_TYPE_INT, { .i64 = 0 }, 0, 2, STATIC, "force_oar" },
        { "disable",  NULL, 0, AV_OPT_TYPE_CONST, {.i64 = 0 }, 0, 0, STATIC, "force_oar" },
        { "decrease", NULL, 0, AV_OPT_TYPE_CONST, {.i64 = 1 }, 0, 0, STATIC, "force_oar" },
        { "increase", NULL, 0, AV_OPT_TYPE_CONST, {.i64 = 2 }, 0, 0, STATIC, "force_oar" },
    { "force_divisible_by", "enforce that the output resolution is divisible by a defined integer when force_original_aspect_ratio is used", OFFSET(force_divisible_by), AV_OPT_TYPE_INT, { .i64 = 1 }, 1, 256, STATIC },
    { "normalize_sar", "force SAR normalization to 1:1", OFFSET(normalize_sar), AV_OPT_TYPE_BOOL, {.i64 = 1}, 0, 1, STATIC },
    { "pad_crop_ratio", "ratio between padding and cropping when normalizing SAR (0=pad, 1=crop)", OFFSET(pad_crop_ratio), AV_OPT_TYPE_FLOAT, {.dbl=0.0}, 0.0, 1.0, DYNAMIC },

    {"colorspace", "select colorspace", OFFSET(colorspace), AV_OPT_TYPE_INT, {.i64=-1}, -1, AVCOL_SPC_NB-1, DYNAMIC, "colorspace"},
    {"auto", "keep the same colorspace",  0, AV_OPT_TYPE_CONST, {.i64=-1},                          INT_MIN, INT_MAX, STATIC, "colorspace"},
    {"gbr",                        NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_SPC_RGB},               INT_MIN, INT_MAX, STATIC, "colorspace"},
    {"bt709",                      NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_SPC_BT709},             INT_MIN, INT_MAX, STATIC, "colorspace"},
    {"unknown",                    NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_SPC_UNSPECIFIED},       INT_MIN, INT_MAX, STATIC, "colorspace"},
    {"bt470bg",                    NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_SPC_BT470BG},           INT_MIN, INT_MAX, STATIC, "colorspace"},
    {"smpte170m",                  NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_SPC_SMPTE170M},         INT_MIN, INT_MAX, STATIC, "colorspace"},
    {"smpte240m",                  NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_SPC_SMPTE240M},         INT_MIN, INT_MAX, STATIC, "colorspace"},
    {"ycgco",                      NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_SPC_YCGCO},             INT_MIN, INT_MAX, STATIC, "colorspace"},
    {"bt2020nc",                   NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_SPC_BT2020_NCL},        INT_MIN, INT_MAX, STATIC, "colorspace"},
    {"bt2020c",                    NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_SPC_BT2020_CL},         INT_MIN, INT_MAX, STATIC, "colorspace"},
    {"ictcp",                      NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_SPC_ICTCP},             INT_MIN, INT_MAX, STATIC, "colorspace"},

    {"range", "select color range", OFFSET(color_range), AV_OPT_TYPE_INT, {.i64=-1}, -1, AVCOL_RANGE_NB-1, DYNAMIC, "range"},
    {"auto",  "keep the same color range",   0, AV_OPT_TYPE_CONST, {.i64=-1},                       0, 0, STATIC, "range"},
    {"unspecified",                  NULL,   0, AV_OPT_TYPE_CONST, {.i64=AVCOL_RANGE_UNSPECIFIED},  0, 0, STATIC, "range"},
    {"unknown",                      NULL,   0, AV_OPT_TYPE_CONST, {.i64=AVCOL_RANGE_UNSPECIFIED},  0, 0, STATIC, "range"},
    {"limited",                      NULL,   0, AV_OPT_TYPE_CONST, {.i64=AVCOL_RANGE_MPEG},         0, 0, STATIC, "range"},
    {"tv",                           NULL,   0, AV_OPT_TYPE_CONST, {.i64=AVCOL_RANGE_MPEG},         0, 0, STATIC, "range"},
    {"mpeg",                         NULL,   0, AV_OPT_TYPE_CONST, {.i64=AVCOL_RANGE_MPEG},         0, 0, STATIC, "range"},
    {"full",                         NULL,   0, AV_OPT_TYPE_CONST, {.i64=AVCOL_RANGE_JPEG},         0, 0, STATIC, "range"},
    {"pc",                           NULL,   0, AV_OPT_TYPE_CONST, {.i64=AVCOL_RANGE_JPEG},         0, 0, STATIC, "range"},
    {"jpeg",                         NULL,   0, AV_OPT_TYPE_CONST, {.i64=AVCOL_RANGE_JPEG},         0, 0, STATIC, "range"},

    {"color_primaries", "select color primaries", OFFSET(color_primaries), AV_OPT_TYPE_INT, {.i64=-1}, -1, AVCOL_PRI_NB-1, DYNAMIC, "color_primaries"},
    {"auto", "keep the same color primaries",  0, AV_OPT_TYPE_CONST, {.i64=-1},                     INT_MIN, INT_MAX, STATIC, "color_primaries"},
    {"bt709",                           NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_PRI_BT709},        INT_MIN, INT_MAX, STATIC, "color_primaries"},
    {"unknown",                         NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_PRI_UNSPECIFIED},  INT_MIN, INT_MAX, STATIC, "color_primaries"},
    {"bt470m",                          NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_PRI_BT470M},       INT_MIN, INT_MAX, STATIC, "color_primaries"},
    {"bt470bg",                         NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_PRI_BT470BG},      INT_MIN, INT_MAX, STATIC, "color_primaries"},
    {"smpte170m",                       NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_PRI_SMPTE170M},    INT_MIN, INT_MAX, STATIC, "color_primaries"},
    {"smpte240m",                       NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_PRI_SMPTE240M},    INT_MIN, INT_MAX, STATIC, "color_primaries"},
    {"film",                            NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_PRI_FILM},         INT_MIN, INT_MAX, STATIC, "color_primaries"},
    {"bt2020",                          NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_PRI_BT2020},       INT_MIN, INT_MAX, STATIC, "color_primaries"},
    {"smpte428",                        NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_PRI_SMPTE428},     INT_MIN, INT_MAX, STATIC, "color_primaries"},
    {"smpte431",                        NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_PRI_SMPTE431},     INT_MIN, INT_MAX, STATIC, "color_primaries"},
    {"smpte432",                        NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_PRI_SMPTE432},     INT_MIN, INT_MAX, STATIC, "color_primaries"},
    {"jedec-p22",                       NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_PRI_JEDEC_P22},    INT_MIN, INT_MAX, STATIC, "color_primaries"},
    {"ebu3213",                         NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_PRI_EBU3213},      INT_MIN, INT_MAX, STATIC, "color_primaries"},

    {"color_trc", "select color transfer", OFFSET(color_trc), AV_OPT_TYPE_INT, {.i64=-1}, -1, AVCOL_TRC_NB-1, DYNAMIC, "color_trc"},
    {"auto", "keep the same color transfer",  0, AV_OPT_TYPE_CONST, {.i64=-1},                     INT_MIN, INT_MAX, STATIC, "color_trc"},
    {"bt709",                          NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_TRC_BT709},        INT_MIN, INT_MAX, STATIC, "color_trc"},
    {"unknown",                        NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_TRC_UNSPECIFIED},  INT_MIN, INT_MAX, STATIC, "color_trc"},
    {"bt470m",                         NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_TRC_GAMMA22},      INT_MIN, INT_MAX, STATIC, "color_trc"},
    {"bt470bg",                        NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_TRC_GAMMA28},      INT_MIN, INT_MAX, STATIC, "color_trc"},
    {"smpte170m",                      NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_TRC_SMPTE170M},    INT_MIN, INT_MAX, STATIC, "color_trc"},
    {"smpte240m",                      NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_TRC_SMPTE240M},    INT_MIN, INT_MAX, STATIC, "color_trc"},
    {"linear",                         NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_TRC_LINEAR},       INT_MIN, INT_MAX, STATIC, "color_trc"},
    {"iec61966-2-4",                   NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_TRC_IEC61966_2_4}, INT_MIN, INT_MAX, STATIC, "color_trc"},
    {"bt1361e",                        NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_TRC_BT1361_ECG},   INT_MIN, INT_MAX, STATIC, "color_trc"},
    {"iec61966-2-1",                   NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_TRC_IEC61966_2_1}, INT_MIN, INT_MAX, STATIC, "color_trc"},
    {"bt2020-10",                      NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_TRC_BT2020_10},    INT_MIN, INT_MAX, STATIC, "color_trc"},
    {"bt2020-12",                      NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_TRC_BT2020_12},    INT_MIN, INT_MAX, STATIC, "color_trc"},
    {"smpte2084",                      NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_TRC_SMPTE2084},    INT_MIN, INT_MAX, STATIC, "color_trc"},
    {"arib-std-b67",                   NULL,  0, AV_OPT_TYPE_CONST, {.i64=AVCOL_TRC_ARIB_STD_B67}, INT_MIN, INT_MAX, STATIC, "color_trc"},

    { "upscaler", "Upscaler function", OFFSET(upscaler), AV_OPT_TYPE_STRING, {.str = "spline36"}, .flags = DYNAMIC },
    { "downscaler", "Downscaler function", OFFSET(downscaler), AV_OPT_TYPE_STRING, {.str = "mitchell"}, .flags = DYNAMIC },
    { "lut_entries", "Number of scaler LUT entries", OFFSET(lut_entries), AV_OPT_TYPE_INT, {.i64 = 0}, 0, 256, DYNAMIC },
    { "antiringing", "Antiringing strength (for non-EWA filters)", OFFSET(antiringing), AV_OPT_TYPE_FLOAT, {.dbl = 0.0}, 0.0, 1.0, DYNAMIC },
    { "sigmoid", "Enable sigmoid upscaling", OFFSET(sigmoid), AV_OPT_TYPE_BOOL, {.i64 = 1}, 0, 1, DYNAMIC },
    { "skip_av1_grain", "Disable AV1 grain application", OFFSET(skip_av1_grain), AV_OPT_TYPE_BOOL, {.i64 = 0}, 0, 1, DYNAMIC },

    { "deband", "Enable debanding", OFFSET(deband), AV_OPT_TYPE_BOOL, {.i64 = 0}, 0, 1, DYNAMIC },
    { "deband_iterations", "Deband iterations", OFFSET(deband_iterations), AV_OPT_TYPE_INT, {.i64 = 1}, 0, 16, DYNAMIC },
    { "deband_threshold", "Deband threshold", OFFSET(deband_threshold), AV_OPT_TYPE_FLOAT, {.dbl = 4.0}, 0.0, 1024.0, DYNAMIC },
    { "deband_radius", "Deband radius", OFFSET(deband_radius), AV_OPT_TYPE_FLOAT, {.dbl = 16.0}, 0.0, 1024.0, DYNAMIC },
    { "deband_grain", "Deband grain", OFFSET(deband_grain), AV_OPT_TYPE_FLOAT, {.dbl = 6.0}, 0.0, 1024.0, DYNAMIC },

    { "brightness", "Brightness boost", OFFSET(brightness), AV_OPT_TYPE_FLOAT, {.dbl = 0.0}, -1.0, 1.0, DYNAMIC },
    { "contrast", "Contrast gain", OFFSET(contrast), AV_OPT_TYPE_FLOAT, {.dbl = 1.0}, 0.0, 16.0, DYNAMIC },
    { "saturation", "Saturation gain", OFFSET(saturation), AV_OPT_TYPE_FLOAT, {.dbl = 1.0}, 0.0, 16.0, DYNAMIC },
    { "hue", "Hue shift", OFFSET(hue), AV_OPT_TYPE_FLOAT, {.dbl = 0.0}, -M_PI, M_PI, DYNAMIC },
    { "gamma", "Gamma adjustment", OFFSET(gamma), AV_OPT_TYPE_FLOAT, {.dbl = 1.0}, 0.0, 16.0, DYNAMIC },

    { "peak_detect", "Enable dynamic peak detection for HDR tone-mapping", OFFSET(peakdetect), AV_OPT_TYPE_BOOL, {.i64 = 1}, 0, 1, DYNAMIC },
    { "smoothing_period", "Peak detection smoothing period", OFFSET(smoothing), AV_OPT_TYPE_FLOAT, {.dbl = 100.0}, 0.0, 1000.0, DYNAMIC },
    { "scene_threshold_low", "Scene change low threshold", OFFSET(scene_low), AV_OPT_TYPE_FLOAT, {.dbl = 5.5}, -1.0, 100.0, DYNAMIC },
    { "scene_threshold_high", "Scene change high threshold", OFFSET(scene_high), AV_OPT_TYPE_FLOAT, {.dbl = 10.0}, -1.0, 100.0, DYNAMIC },
    { "overshoot", "Tone-mapping overshoot margin", OFFSET(overshoot), AV_OPT_TYPE_FLOAT, {.dbl = 0.05}, 0.0, 1.0, DYNAMIC },

    { "intent", "Rendering intent", OFFSET(intent), AV_OPT_TYPE_INT, {.i64 = PL_INTENT_RELATIVE_COLORIMETRIC}, 0, 3, DYNAMIC, "intent" },
        { "perceptual", "Perceptual", 0, AV_OPT_TYPE_CONST, {.i64 = PL_INTENT_PERCEPTUAL}, 0, 0, STATIC, "intent" },
        { "relative", "Relative colorimetric", 0, AV_OPT_TYPE_CONST, {.i64 = PL_INTENT_RELATIVE_COLORIMETRIC}, 0, 0, STATIC, "intent" },
        { "absolute", "Absolute colorimetric", 0, AV_OPT_TYPE_CONST, {.i64 = PL_INTENT_ABSOLUTE_COLORIMETRIC}, 0, 0, STATIC, "intent" },
        { "saturation", "Saturation mapping", 0, AV_OPT_TYPE_CONST, {.i64 = PL_INTENT_SATURATION}, 0, 0, STATIC, "intent" },
    { "tonemapping", "Tone-mapping algorithm", OFFSET(tonemapping), AV_OPT_TYPE_INT, {.i64 = PL_TONE_MAPPING_BT_2390}, 0, PL_TONE_MAPPING_ALGORITHM_COUNT - 1, DYNAMIC, "tonemap" },
        { "clip", "Hard-clipping", 0, AV_OPT_TYPE_CONST, {.i64 = PL_TONE_MAPPING_CLIP}, 0, 0, STATIC, "tonemap" },
        { "mobius", "Mobius tone-mapping", 0, AV_OPT_TYPE_CONST, {.i64 = PL_TONE_MAPPING_MOBIUS}, 0, 0, STATIC, "tonemap" },
        { "reinhard", "Reinhard tone-mapping", 0, AV_OPT_TYPE_CONST, {.i64 = PL_TONE_MAPPING_REINHARD}, 0, 0, STATIC, "tonemap" },
        { "hable", "Hable/Filmic tone-mapping", 0, AV_OPT_TYPE_CONST, {.i64 = PL_TONE_MAPPING_HABLE}, 0, 0, STATIC, "tonemap" },
        { "gamma", "Gamma tone-mapping", 0, AV_OPT_TYPE_CONST, {.i64 = PL_TONE_MAPPING_GAMMA}, 0, 0, STATIC, "tonemap" },
        { "linear", "Linear tone-mapping", 0, AV_OPT_TYPE_CONST, {.i64 = PL_TONE_MAPPING_LINEAR}, 0, 0, STATIC, "tonemap" },
        { "bt.2390", "ITU-R BT.2390 tone-mapping", 0, AV_OPT_TYPE_CONST, {.i64 = PL_TONE_MAPPING_BT_2390}, 0, 0, STATIC, "tonemap" },
    { "tonemapping_param", "Tunable parameter for some tone-mapping functions", OFFSET(tonemapping_param), AV_OPT_TYPE_FLOAT, {.dbl = 0.0}, .flags = DYNAMIC },
    { "desaturation_strength", "Desaturation strength", OFFSET(desat_str), AV_OPT_TYPE_FLOAT, {.dbl = 0.75}, 0.0, 1.0, DYNAMIC },
    { "desaturation_exponent", "Desaturation exponent", OFFSET(desat_exp), AV_OPT_TYPE_FLOAT, {.dbl = 1.5}, 0.0, 10.0, DYNAMIC },
    { "desaturation_base", "Desaturation base", OFFSET(desat_base), AV_OPT_TYPE_FLOAT, {.dbl = 0.18}, 0.0, 10.0, DYNAMIC },
    { "max_boost", "Tone-mapping maximum boost", OFFSET(max_boost), AV_OPT_TYPE_FLOAT, {.dbl = 1.0}, 1.0, 10.0, DYNAMIC },
    { "gamut_warning", "Highlight out-of-gamut colors", OFFSET(gamut_warning), AV_OPT_TYPE_BOOL, {.i64 = 0}, 0, 1, DYNAMIC },
    { "gamut_clipping", "Enable colorimetric gamut clipping", OFFSET(gamut_clipping), AV_OPT_TYPE_BOOL, {.i64 = 1}, 0, 1, DYNAMIC },

    { "dithering", "Dither method to use", OFFSET(dithering), AV_OPT_TYPE_INT, {.i64 = PL_DITHER_BLUE_NOISE}, -1, PL_DITHER_METHOD_COUNT - 1, DYNAMIC, "dither" },
        { "none", "Disable dithering", 0, AV_OPT_TYPE_CONST, {.i64 = -1}, 0, 0, STATIC, "dither" },
        { "blue", "Blue noise", 0, AV_OPT_TYPE_CONST, {.i64 = PL_DITHER_BLUE_NOISE}, 0, 0, STATIC, "dither" },
        { "ordered", "Ordered LUT", 0, AV_OPT_TYPE_CONST, {.i64 = PL_DITHER_ORDERED_LUT}, 0, 0, STATIC, "dither" },
        { "ordered_fixed", "Fixed function ordered", 0, AV_OPT_TYPE_CONST, {.i64 = PL_DITHER_ORDERED_FIXED}, 0, 0, STATIC, "dither" },
        { "white", "White noise", 0, AV_OPT_TYPE_CONST, {.i64 = PL_DITHER_WHITE_NOISE}, 0, 0, STATIC, "dither" },
    { "dither_lut_size", "Dithering LUT size", OFFSET(dither_lut_size), AV_OPT_TYPE_INT, {.i64 = 6}, 1, 8, STATIC },
    { "dither_temporal", "Enable temporal dithering", OFFSET(dither_temporal), AV_OPT_TYPE_BOOL, {.i64 = 0}, 0, 1, DYNAMIC },

    { "cones", "Colorblindness adaptation model", OFFSET(cones), AV_OPT_TYPE_FLAGS, {.i64 = 0}, 0, PL_CONE_LMS, DYNAMIC, "cone" },
        { "l", "L cone", 0, AV_OPT_TYPE_CONST, {.i64 = PL_CONE_L}, 0, 0, STATIC, "cone" },
        { "m", "M cone", 0, AV_OPT_TYPE_CONST, {.i64 = PL_CONE_M}, 0, 0, STATIC, "cone" },
        { "s", "S cone", 0, AV_OPT_TYPE_CONST, {.i64 = PL_CONE_S}, 0, 0, STATIC, "cone" },
    { "cone-strength", "Colorblindness adaptation strength", OFFSET(cone_str), AV_OPT_TYPE_FLOAT, {.dbl = 0.0}, 0.0, 10.0, DYNAMIC },

    { "custom_shader_path", "Path to custom user shader (mpv .hook format)", OFFSET(shader_path), AV_OPT_TYPE_STRING, .flags = STATIC },
    { "custom_shader_bin", "Custom user shader as binary (mpv .hook format)", OFFSET(shader_bin), AV_OPT_TYPE_BINARY, .flags = STATIC },

    /* Performance/quality tradeoff options */
    { "skip_aa", "Skip anti-aliasing", OFFSET(skip_aa), AV_OPT_TYPE_BOOL, {.i64 = 0}, 0, 0, DYNAMIC },
    { "polar_cutoff", "Polar LUT cutoff", OFFSET(polar_cutoff), AV_OPT_TYPE_FLOAT, {.i64 = 0}, 0.0, 1.0, DYNAMIC },
    { "disable_linear", "Disable linear scaling", OFFSET(disable_linear), AV_OPT_TYPE_BOOL, {.i64 = 0}, 0, 1, DYNAMIC },
    { "disable_builtin", "Disable built-in scalers", OFFSET(disable_builtin), AV_OPT_TYPE_BOOL, {.i64 = 0}, 0, 1, DYNAMIC },
    { "force_3dlut", "Force the use of a full 3DLUT", OFFSET(force_3dlut), AV_OPT_TYPE_BOOL, {.i64 = 0}, 0, 1, DYNAMIC },
    { "force_dither", "Force dithering", OFFSET(force_dither), AV_OPT_TYPE_BOOL, {.i64 = 0}, 0, 1, DYNAMIC },
    { "disable_fbos", "Force-disable FBOs", OFFSET(disable_fbos), AV_OPT_TYPE_BOOL, {.i64 = 0}, 0, 1, DYNAMIC },
    { NULL },
};

AVFILTER_DEFINE_CLASS(libplacebo);

static const AVFilterPad libplacebo_inputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_VIDEO,
        .filter_frame = &filter_frame,
    },
    { NULL }
};

static const AVFilterPad libplacebo_outputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_VIDEO,
        .config_props = &config_props,
    },
    { NULL }
};

AVFilter ff_vf_libplacebo = {
    .name           = "libplacebo",
    .description    = NULL_IF_CONFIG_SMALL("Apply various GPU filters from libplacebo"),
    .priv_size      = sizeof(LibplaceboContext),
    .priv_class     = &libplacebo_class,
    .inputs         = libplacebo_inputs,
    .outputs        = libplacebo_outputs,
    .init           = &init,
    .uninit         = &uninit,
    .query_formats  = &query_formats,
    .process_command = &ff_filter_process_command,
};
