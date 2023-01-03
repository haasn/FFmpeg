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

#include "libavutil/common.h"
#include "libavutil/dovi_meta.h"
#include "libavutil/opt.h"

#include "avfilter.h"
#include "internal.h"
#include "video.h"

typedef struct DvReshapeContext {
    const AVClass *class;
} DvReshapeContext;

static const AVOption dvreshape_options[] = {
};

AVFILTER_DEFINE_CLASS(dvreshape);

static av_cold void dvreshape_uninit(AVFilterContext *avctx)
{
    DvReshapeContext *s = avctx->priv;
    (void) s;
}

static av_cold int dvreshape_init(AVFilterContext *avctx)
{
    DvReshapeContext *s = avctx->priv;
    (void) s;
    return 0;
}

static int dvreshape_apply_c(AVFrame *out, AVFrame *in,
                             const AVDOVIMetadata *data)
{
    return 0;
}

static int dvreshape_filter_frame(AVFilterLink *inlink, AVFrame *in)
{
    AVFilterLink *outlink = inlink->dst->outputs[0];
    DvReshapeContext *s = inlink->dst->priv;
    AVFrameSideData *sd;
    AVFrame *out;
    int ret = 0;
    (void) s;

    if (!(sd = av_frame_get_side_data(in, AV_FRAME_DATA_DOVI_METADATA)))
        return ff_filter_frame(outlink, in);

    out = ff_get_video_buffer(outlink, outlink->w, outlink->h);
    if (!out) {
        av_frame_free(&in);
        return AVERROR(ENOMEM);
    }
    av_frame_copy_props(out, in);
    av_frame_remove_side_data(out, AV_FRAME_DATA_DOVI_RPU_BUFFER);
    av_frame_remove_side_data(out, AV_FRAME_DATA_DOVI_METADATA);

    ret = dvreshape_apply_c(out, in, (const AVDOVIMetadata *) sd->data);
    if (ret < 0) {
        av_frame_free(&out);
        return ret;
    }

    return ff_filter_frame(outlink, out);
}

static const AVFilterPad dvreshape_inputs[] = {
    {
        .name           = "default",
        .type           = AVMEDIA_TYPE_VIDEO,
        .filter_frame   = dvreshape_filter_frame,
    },
};

static const AVFilterPad dvreshape_outputs[] = {
    {
        .name           = "default",
        .type           = AVMEDIA_TYPE_VIDEO,
    },
};

static const enum AVPixelFormat pix_fmts[] = {
    // TODO: add more
    AV_PIX_FMT_YUV420P10, AV_PIX_FMT_NONE
};


const AVFilter ff_vf_dvreshape = {
    .name           = "dvreshape",
    .description    = NULL_IF_CONFIG_SMALL("Apply DV color reshaping"),
    .priv_size      = sizeof(DvReshapeContext),
    .priv_class     = &dvreshape_class,
    .init           = &dvreshape_init,
    .uninit         = &dvreshape_uninit,
    FILTER_INPUTS(dvreshape_inputs),
    FILTER_OUTPUTS(dvreshape_outputs),
    FILTER_PIXFMTS_ARRAY(pix_fmts),
};
