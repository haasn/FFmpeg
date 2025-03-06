$(SUBDIR)x86/swscale_mmx.o: CFLAGS += $(NOREDZONE_FLAGS)

OBJS                            += x86/rgb2rgb.o                        \
                                   x86/swscale.o                        \
                                   x86/ops_avx2_i8.o                    \
                                   x86/ops_avx2_i16.o                   \
                                   x86/ops_avx2_i32.o                   \
                                   x86/ops_avx2_f32.o                   \
                                   x86/yuv2rgb.o                        \

AVX2-OPS-CFLAGS = $(OPS-CFLAGS) -mavx2 -mfma
$(SUBDIR)x86/ops_avx2_%.o: CFLAGS += $(AVX2-OPS-CFLAGS)

MMX-OBJS                        += x86/hscale_fast_bilinear_simd.o      \

OBJS-$(CONFIG_XMM_CLOBBER_TEST) += x86/w64xmmtest.o

X86ASM-OBJS                     += x86/input.o                          \
                                   x86/output.o                         \
                                   x86/scale.o                          \
                                   x86/scale_avx2.o                          \
                                   x86/range_convert.o                  \
                                   x86/rgb_2_rgb.o                      \
                                   x86/yuv_2_rgb.o                      \
                                   x86/yuv2yuvX.o                       \
