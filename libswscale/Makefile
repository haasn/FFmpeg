NAME = swscale
DESC = FFmpeg image rescaling library

HEADERS = swscale.h                                                     \
          version.h                                                     \
          version_major.h                                               \

OBJS = alphablend.o                                     \
       cms.o                                            \
       csputils.o                                       \
       hscale.o                                         \
       hscale_fast_bilinear.o                           \
       format.o                                         \
       gamma.o                                          \
       graph.o                                          \
       half2float.o                                     \
       input.o                                          \
       lut3d.o                                          \
       ops.o                                            \
       ops_c_i8.o                                       \
       ops_c_i16.o                                      \
       ops_c_i32.o                                      \
       ops_c_f32.o                                      \
       options.o                                        \
       output.o                                         \
       rgb2rgb.o                                        \
       slice.o                                          \
       swscale.o                                        \
       swscale_unscaled.o                               \
       utils.o                                          \
       version.o                                        \
       yuv2rgb.o                                        \
       vscale.o                                         \

OPS-CFLAGS = -ffinite-math-only \
             -fno-signed-zeros  \
             -fno-trapping-math

$(SUBDIR)ops_c_%.o: CFLAGS += $(OPS-CFLAGS)

# Objects duplicated from other libraries for shared builds
SHLIBOBJS                    += log2_tab.o

# Windows resource file
SHLIBOBJS-$(HAVE_GNU_WINDRES) += swscaleres.o

TESTPROGS = colorspace                                                  \
            floatimg_cmp                                                \
            pixdesc_query                                               \
            swscale                                                     \
