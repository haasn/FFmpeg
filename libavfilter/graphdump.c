/*
 * Filter graphs to bad ASCII-art
 * Copyright (c) 2012 Nicolas George
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

#include <string.h>

#include "libavutil/channel_layout.h"
#include "libavutil/bprint.h"
#include "libavutil/mem.h"
#include "libavutil/opt.h"
#include "libavutil/pixdesc.h"
#include "avfilter.h"
#include "filters.h"

enum {
    FMT_NONE = 0,
    FMT_PRETTY,
    FMT_COMPLEX,
    FMT_NB,
};

typedef struct GraphDumpOptions {
    const AVClass *class;

    int format;
} GraphDumpOptions;

#define OFFSET(x) offsetof(GraphDumpOptions, x)
static const AVOption graph_dump_options[] = {
    { "format",     "set the output format", OFFSET(format), AV_OPT_TYPE_INT, { .i64 = FMT_NONE }, 0, FMT_NB - 1, .unit = "format" },
    {     "none",   "don't produce any output",         0, AV_OPT_TYPE_CONST, {.i64 = FMT_NONE},    .unit = "format" },
    {     "pretty", "pretty printed ASCII art graph",   0, AV_OPT_TYPE_CONST, {.i64 = FMT_PRETTY},  .unit = "format" },
    {     "complex","complex filter graph",             0, AV_OPT_TYPE_CONST, {.i64 = FMT_COMPLEX}, .unit = "format" },
    { NULL },
};

AVFILTER_DEFINE_CLASS(graph_dump);

static int print_link_prop(AVBPrint *buf, AVFilterLink *link)
{
    const char *format;
    AVBPrint dummy_buffer;

    if (!buf) {
        buf = &dummy_buffer;
        av_bprint_init(buf, 0, AV_BPRINT_SIZE_COUNT_ONLY);
    }
    switch (link->type) {
        case AVMEDIA_TYPE_VIDEO:
            format = av_x_if_null(av_get_pix_fmt_name(link->format), "?");
            av_bprintf(buf, "[%dx%d %d:%d %s]", link->w, link->h,
                    link->sample_aspect_ratio.num,
                    link->sample_aspect_ratio.den,
                    format);
            break;

        case AVMEDIA_TYPE_AUDIO:
            format = av_x_if_null(av_get_sample_fmt_name(link->format), "?");
            av_bprintf(buf, "[%dHz %s:",
                       (int)link->sample_rate, format);
            av_channel_layout_describe_bprint(&link->ch_layout, buf);
            av_bprint_chars(buf, ']', 1);
            break;

        default:
            av_bprintf(buf, "?");
            break;
    }
    return buf->len;
}

static void dump_pretty(AVBPrint *buf, AVFilterGraph *graph)
{
    unsigned i, j, x, e;

    for (i = 0; i < graph->nb_filters; i++) {
        AVFilterContext *filter = graph->filters[i];
        unsigned max_src_name = 0, max_dst_name = 0;
        unsigned max_in_name  = 0, max_out_name = 0;
        unsigned max_in_fmt   = 0, max_out_fmt  = 0;
        unsigned width, height, in_indent;
        unsigned lname = strlen(filter->name);
        unsigned ltype = strlen(filter->filter->name);

        for (j = 0; j < filter->nb_inputs; j++) {
            AVFilterLink *l = filter->inputs[j];
            unsigned ln = strlen(l->src->name) + 1 + strlen(l->srcpad->name);
            max_src_name = FFMAX(max_src_name, ln);
            max_in_name = FFMAX(max_in_name, strlen(l->dstpad->name));
            max_in_fmt = FFMAX(max_in_fmt, print_link_prop(NULL, l));
        }
        for (j = 0; j < filter->nb_outputs; j++) {
            AVFilterLink *l = filter->outputs[j];
            unsigned ln = strlen(l->dst->name) + 1 + strlen(l->dstpad->name);
            max_dst_name = FFMAX(max_dst_name, ln);
            max_out_name = FFMAX(max_out_name, strlen(l->srcpad->name));
            max_out_fmt = FFMAX(max_out_fmt, print_link_prop(NULL, l));
        }
        in_indent = max_src_name + max_in_name + max_in_fmt;
        in_indent += in_indent ? 4 : 0;
        width = FFMAX(lname + 2, ltype + 4);
        height = FFMAX3(2, filter->nb_inputs, filter->nb_outputs);
        av_bprint_chars(buf, ' ', in_indent);
        av_bprintf(buf, "+");
        av_bprint_chars(buf, '-', width);
        av_bprintf(buf, "+\n");
        for (j = 0; j < height; j++) {
            unsigned in_no  = j - (height - filter->nb_inputs ) / 2;
            unsigned out_no = j - (height - filter->nb_outputs) / 2;

            /* Input link */
            if (in_no < filter->nb_inputs) {
                AVFilterLink *l = filter->inputs[in_no];
                e = buf->len + max_src_name + 2;
                av_bprintf(buf, "%s:%s", l->src->name, l->srcpad->name);
                av_bprint_chars(buf, '-', e - buf->len);
                e = buf->len + max_in_fmt + 2 +
                    max_in_name - strlen(l->dstpad->name);
                print_link_prop(buf, l);
                av_bprint_chars(buf, '-', e - buf->len);
                av_bprintf(buf, "%s", l->dstpad->name);
            } else {
                av_bprint_chars(buf, ' ', in_indent);
            }

            /* Filter */
            av_bprintf(buf, "|");
            if (j == (height - 2) / 2) {
                x = (width - lname) / 2;
                av_bprintf(buf, "%*s%-*s", x, "", width - x, filter->name);
            } else if (j == (height - 2) / 2 + 1) {
                x = (width - ltype - 2) / 2;
                av_bprintf(buf, "%*s(%s)%*s", x, "", filter->filter->name,
                        width - ltype - 2 - x, "");
            } else {
                av_bprint_chars(buf, ' ', width);
            }
            av_bprintf(buf, "|");

            /* Output link */
            if (out_no < filter->nb_outputs) {
                AVFilterLink *l = filter->outputs[out_no];
                unsigned ln = strlen(l->dst->name) + 1 +
                              strlen(l->dstpad->name);
                e = buf->len + max_out_name + 2;
                av_bprintf(buf, "%s", l->srcpad->name);
                av_bprint_chars(buf, '-', e - buf->len);
                e = buf->len + max_out_fmt + 2 +
                    max_dst_name - ln;
                print_link_prop(buf, l);
                av_bprint_chars(buf, '-', e - buf->len);
                av_bprintf(buf, "%s:%s", l->dst->name, l->dstpad->name);
            }
            av_bprintf(buf, "\n");
        }
        av_bprint_chars(buf, ' ', in_indent);
        av_bprintf(buf, "+");
        av_bprint_chars(buf, '-', width);
        av_bprintf(buf, "+\n");
        av_bprintf(buf, "\n");
    }
}

/* Assign a unique ID to each link by keeping track of them in an array */
static int get_link_id(AVFilterLink ***links, int *nb_links, AVFilterLink *link)
{
    int ret;
    for (int i = 0; i < *nb_links; i++) {
        if ((*links)[i] == link)
            return i;
    }

    ret = av_dynarray_add_nofree(links, nb_links, link);
    return ret ? ret : *nb_links - 1;
}

static const char *get_filter_name(const AVFilterContext *filter)
{
    /* Reuse the filter instance name if present */
    return strchr(filter->name, '@') ? filter->name : filter->filter->name;
}

static void print_link_label(AVBPrint *buf, AVFilterLink *link, int id)
{
    if (link->srcpad->label)
        av_bprintf(buf, "%s", link->srcpad->label);
    else if (link->dstpad->label)
        av_bprintf(buf, "%s", link->dstpad->label);
    else
        av_bprintf(buf, "L%d", id);
}

static int dump_complex(AVBPrint *buf, AVFilterGraph *graph)
{
    /* Keep track of seen filter links to assign a unique ID to each */
    AVFilterLink **links = NULL;
    int nb_links = 0;
    int ret = AVERROR(ENOMEM);
    char *filter_opts = NULL;

    for (int i = 0; i < graph->nb_filters; i++) {
        AVFilterContext *filter = graph->filters[i];
        if (i == 0)
            av_bprintf(buf, "Filter graph:\n");

        ret = av_opt_serialize(filter, AV_OPT_FLAG_FILTERING_PARAM,
                               AV_OPT_SERIALIZE_SKIP_DEFAULTS |
                               AV_OPT_SERIALIZE_SEARCH_CHILDREN,
                               &filter_opts, '=', ':');
        if (ret < 0)
            goto fail;

        av_bprintf(buf, "  ");
        for (int j = 0; j < filter->nb_inputs; j++) {
            AVFilterLink *link = filter->inputs[j];
            ret = get_link_id(&links, &nb_links, link);
            if (ret < 0)
                goto fail;
            av_bprintf(buf, "[");
            print_link_label(buf, link, ret);
            av_bprintf(buf, "] ");
        }

        av_bprintf(buf, "%s", get_filter_name(filter));
        if (filter_opts && filter_opts[0])
            av_bprintf(buf, "=%s", filter_opts);
        av_freep(&filter_opts);

        for (int j = 0; j < filter->nb_outputs; j++) {
            AVFilterLink *link = filter->outputs[j];
            ret = get_link_id(&links, &nb_links, link);
            if (ret < 0)
                goto fail;
            av_bprintf(buf, " [");
            print_link_label(buf, link, ret);
            av_bprintf(buf, "]");
        }
        av_bprintf(buf, ";\n");
    }

    /* Dump a summary of all seen links */
    for (int i = 0; i < nb_links; i++) {
        AVFilterLink *link = links[i];
        if (i == 0)
            av_bprintf(buf, "Filter links:\n");
        av_bprintf(buf, "  [");
        print_link_label(buf, link, i);
        av_bprintf(buf, ": %s -> %s] ", get_filter_name(link->src),
                   get_filter_name(link->dst));

        switch (link->type) {
        case AVMEDIA_TYPE_VIDEO:
            av_bprintf(buf, "%s %dx%d [SAR %d:%d] csp:%s range:%s\n",
                       av_get_pix_fmt_name(link->format), link->w, link->h,
                       link->sample_aspect_ratio.num, link->sample_aspect_ratio.den,
                       av_color_space_name(link->colorspace),
                       av_color_range_name(link->color_range));
            break;
        case AVMEDIA_TYPE_AUDIO:
            av_bprintf(buf, "%s %dHz ",
                       av_get_sample_fmt_name(link->format), link->sample_rate);
            av_channel_layout_describe_bprint(&link->ch_layout, buf);
            av_bprintf(buf, "\n");
            break;
        default:
            av_bprintf(buf, "unknown\n");
            continue;
        }
    }

    ret = 0;
fail:
    av_free(links);
    av_free(filter_opts);
    return ret;
}


char *avfilter_graph_dump(AVFilterGraph *graph, const char *options)
{
    AVBPrint buf;
    char *dump = NULL;
    int ret;

    GraphDumpOptions opts = {
        .class = &graph_dump_class,
    };

    static const char *shorthand[] = {
        "format", NULL,
    };

    av_opt_set_defaults(&opts);
    ret = av_opt_set_from_string(&opts, options, shorthand, "=", ":");
    if (ret < 0)
        return NULL;

    av_bprint_init(&buf, 0, AV_BPRINT_SIZE_AUTOMATIC);
    switch (opts.format) {
    case FMT_NONE:
        break;
    case FMT_PRETTY:
        dump_pretty(&buf, graph);
        break;
    case FMT_COMPLEX:
        ret = dump_complex(&buf, graph);
        if (ret < 0) {
            av_bprint_finalize(&buf, NULL);
            return NULL;
        }
        break;
    }

    av_bprint_finalize(&buf, &dump);
    return dump;
}
