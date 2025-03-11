OBJS += riscv/rgb2rgb.o \
        riscv/swscale.o
RV-OBJS += riscv/rgb2rgb_rvb.o
RVV-OBJS += riscv/input_rvv.o \
            riscv/ops_rvv_i8.o \
            riscv/ops_rvv_i16.o \
            riscv/ops_rvv_i32.o \
            riscv/ops_rvv_f32.o \
            riscv/range_rvv.o \
            riscv/rgb2rgb_rvv.o

RVV-OPS-CFLAGS = $(OPS-CFLAGS) -march=rv64gcv
$(SUBDIR)riscv/ops_rvv_%.o: CFLAGS += $(RVV-OPS-CFLAGS)

SHLIBOBJS += riscv/cpu_common.o
