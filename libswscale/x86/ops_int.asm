;******************************************************************************
;* Copyright (c) 2025 Niklas Haas
;*
;* This file is part of FFmpeg.
;*
;* FFmpeg is free software; you can redistribute it and/or
;* modify it under the terms of the GNU Lesser General Public
;* License as published by the Free Software Foundation; either
;* version 2.1 of the License, or (at your option) any later version.
;*
;* FFmpeg is distributed in the hope that it will be useful,
;* but WITHOUT ANY WARRANTY; without even the implied warranty of
;* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;* Lesser General Public License for more details.
;*
;* You should have received a copy of the GNU Lesser General Public
;* License along with FFmpeg; if not, write to the Free Software
;* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;******************************************************************************

%include "ops_common.asm"

SECTION_RODATA

expand16_shuf: db  0,  0,  2,  2,  4,  4,  6,  6,  8,  8, 10, 10, 12, 12, 14, 14
expand32_shuf: db  0,  0,  0,  0,  4,  4,  4,  4,  8,  8,  8,  8, 12, 12, 12, 12
read8_unpack2: db  0,  2,  4,  6,  8, 10, 12, 14,  1,  3,  5,  7,  9, 11, 13, 15
read8_unpack3: db  0,  3,  6,  9,  1,  4,  7, 10,  2,  5,  8, 11, -1, -1, -1, -1
write8_pack2:  db  0,  8,  1,  9,  2, 10,  3, 11,  4, 12,  5, 13,  6, 14,  7, 15
pack8_shuf4:   db  0,  4,  8, 12,  1,  5,  9, 13,  2,  6, 10, 14,  3,  7, 11, 15

SECTION .text

;---------------------------------------------------------
; Planar reads / writes

%macro read_planar 1 ; elems
op read_planar%1
            mov r2, [execq + SwsOpExec.in0]
IF %1 > 1,  mov r3, [execq + SwsOpExec.in1]
IF %1 > 2,  mov r4, [execq + SwsOpExec.in2]
IF %1 > 3,  mov r5, [execq + SwsOpExec.in3]
            LOAD_CONT r6
            movu mx, [r2]
IF %1 > 1,  movu my, [r3]
IF %1 > 2,  movu mz, [r4]
IF %1 > 3,  movu mw, [r5]
%if V2
            movu mx2, [r2 + mmsize]
IF %1 > 1,  movu my2, [r3 + mmsize]
IF %1 > 2,  movu mz2, [r4 + mmsize]
IF %1 > 3,  movu mw2, [r5 + mmsize]
%endif
            CONTINUE r6
%endmacro

%macro write_planar 1 ; elems
op write_planar%1
            mov r1, [execq + SwsOpExec.out0]
IF %1 > 1,  mov r2, [execq + SwsOpExec.out1]
IF %1 > 2,  mov r3, [execq + SwsOpExec.out2]
IF %1 > 3,  mov r4, [execq + SwsOpExec.out3]
            movu [r1], mx
IF %1 > 1,  movu [r2], my
IF %1 > 2,  movu [r3], mz
IF %1 > 3,  movu [r4], mw
%if V2
            movu [r1 + mmsize], mx2
IF %1 > 1,  movu [r2 + mmsize], my2
IF %1 > 2,  movu [r3 + mmsize], mz2
IF %1 > 3,  movu [r4 + mmsize], mw2
%endif
            END
%endmacro

%macro read8_packed2 0
op read8_packed2
        mov r2, [execq + SwsOpExec.in0]
        VBROADCASTI128 m12, [read8_unpack2]
        LOAD_CONT r3
        movu mx,  [r2 + 0*mmsize]
        movu my,  [r2 + 1*mmsize]
IF V2,  movu mx2, [r2 + 2*mmsize]
IF V2,  movu my2, [r2 + 3*mmsize]
        pshufb m8, mx, m12          ; { X0 Y0 | X1 Y1 }
        pshufb m9, my, m12          ; { X2 Y2 | X3 Y3 }
        unpcklpd mx, m8, m9         ; { X0 X2 | X1 X3 }
        unpckhpd my, m8, m9         ; { Y0 Y2 | Y1 Y3 }
%if V2
        pshufb m8, mx2, m12
        pshufb m9, my2, m12
        unpcklpd mx2, m8, m9
        unpckhpd my2, m8, m9
%endif
%if avx_enabled
        vpermq mx, mx, q3120       ; { X0 X1 | X2 X3 }
        vpermq my, my, q3120       ; { Y0 Y1 | Y2 Y3 }
IF V2,  vpermq mx2, mx2, q3120
IF V2,  vpermq my2, my2, q3120
%endif
        CONTINUE r3
%endmacro

%macro write8_packed2 0
op write8_packed2
        mov r2, [execq + SwsOpExec.out0]
        VBROADCASTI128 m12, [write8_pack2]
%if avx_enabled
        vpermq mx, mx, q3120       ; { X0 X2 | X1 X3 }
        vpermq my, my, q3120       ; { Y0 Y2 | Y1 Y3 }
IF V2,  vpermq mx2, mx2, q3120
IF V2,  vpermq my2, my2, q3120
%endif
        unpcklpd m8, mx, my        ; { X0 Y0 | X1 Y1 }
        unpckhpd m9, mx, my        ; { X2 Y2 | X3 Y3 }
        pshufb mx, m8, m12
        pshufb my, m9, m12
%if V2
        unpcklpd m8, mx2, my2
        unpckhpd m9, mx2, my2
        pshufb mx2, m8, m12
        pshufb my2, m9, m12
%endif
        movu [r2 + 0*mmsize], mx
        movu [r2 + 1*mmsize], my
IF V2,  movu [r2 + 2*mmsize], mx2
IF V2,  movu [r2 + 3*mmsize], my2
        RET
%endmacro

%macro read8_packed_inner 6 ; x, y, z, w, addr, num
            movu xm8,  [%5 + 0*%6]
            movu xm9,  [%5 + 4*%6]
            movu xm10, [%5 + 8*%6]
            movu xm11, [%5 + 12*%6]
%if avx_enabled
            vinserti128 m8,  m8,  [%5 + 16*%6], 1
            vinserti128 m9,  m9,  [%5 + 20*%6], 1
            vinserti128 m10, m10, [%5 + 24*%6], 1
            vinserti128 m11, m11, [%5 + 28*%6], 1
%endif
            pshufb %1, m8,  m12         ; { X0 Y0 Z0 W0 | X4 Y4 Z4 W4 }
            pshufb %2, m9,  m12         ; { X1 Y1 Z1 W1 | X5 Y5 Z5 W5 }
            pshufb %3, m10, m12         ; { X2 Y2 Z2 W2 | X6 Y6 Z6 W6 }
            pshufb %4, m11, m12         ; { X3 Y3 Z3 W3 | X7 Y7 Z7 W7 }
            punpckldq m8,  %1, %2       ; { X0 X1 Y0 Y1 | X4 X5 Y4 Y5 }
            punpckldq m9,  %3, %4       ; { X2 X3 Y2 Y3 | X6 X7 Y6 Y7 }
            punpckhdq m10, %1, %2       ; { Z0 Z1 W0 W1 | Z4 Z5 W4 W5 }
            punpckhdq m11, %3, %4       ; { Z2 Z3 W2 W3 | Z6 Z7 W6 W7 }
            punpcklqdq %1, m8, m9       ; { X0 X1 X2 X3 | X4 X5 X6 X7 }
            punpckhqdq %2, m8, m9       ; { Y0 Y1 Y2 Y3 | Y4 Y5 Y6 Y7 }
            punpcklqdq %3, m10, m11     ; { Z0 Z1 Z2 Z3 | Z4 Z5 Z6 Z7 }
IF %6 > 3,  punpckhqdq %4, m10, m11     ; { W0 W1 W2 W3 | W4 W5 W6 W7 }
%endmacro

%macro read8_packed3 0
op read8_packed3
        mov r2, [execq + SwsOpExec.in0]
        VBROADCASTI128 m12, [read8_unpack3]
        LOAD_CONT r3
        read8_packed_inner mx, my, mz, mw, r2, 3
IF1 V2, read8_packed_inner mx2, my2, mz2, mw2, r2 + 96, 3
        CONTINUE r3
%endmacro

%macro read8_packed4 0
op read8_packed4
        mov r2, [execq + SwsOpExec.in0]
        VBROADCASTI128 m12, [pack8_shuf4]
        LOAD_CONT r3
        read8_packed_inner mx, my, mz, mw, r2, 4
IF1 V2, read8_packed_inner mx2, my2, mz2, mw2, r2 + 128, 4
        CONTINUE r3
%endmacro

%macro write8_packed4_inner 5 ; x, y, z, w, addr
        punpckldq m8,  %1, %2       ; { X0 Y0 X1 Y1 | X4 Y4 X5 Y5 }
        punpckldq m9,  %3, %4       ; { Z0 W0 Z1 W1 | Z4 W4 Z5 W5 }
        punpckhdq m10, %1, %2       ; { X2 Y2 X3 Y3 | X6 Y6 X7 Y7 }
        punpckhdq m11, %3, %4       ; { Z2 W2 Z3 W3 | Z6 W6 Z7 W7 }
        punpcklqdq %1, m8, m9       ; { X0 Y0 Z0 W0 | X4 Y4 Z4 W4 }
        punpckhqdq %2, m8, m9       ; { X1 Y1 Z1 W1 | X5 Y5 Z5 W5 }
        punpcklqdq %3, m10, m11     ; { X2 Y2 Z2 W2 | X6 Y6 Z6 W6 }
        punpckhqdq %4, m10, m11     ; { X3 Y3 Z3 W3 | X7 Y7 Z7 W7 }
        pshufb m8,  %1, m12
        pshufb m9,  %2, m12
        pshufb m10, %3, m12
        pshufb m11, %4, m12
        movu [%5],      xm8
        movu [%5 + 16], xm9
        movu [%5 + 32], xm10
        movu [%5 + 48], xm11
    %if avx_enabled
        vextracti128 [%5 + 64], m8, 1
        vextracti128 [%5 + 80], m9, 1
        vextracti128 [%5 + 96], m10, 1
        vextracti128 [%5 + 112], m11, 1
    %endif
%endmacro

%macro write8_packed4 0
op write8_packed4
        mov r2, [execq + SwsOpExec.out0]
        VBROADCASTI128 m12, [pack8_shuf4]
        write8_packed4_inner mx, my, mz, mw, r2
IF V2,  write8_packed4_inner mx2, my2, mz2, mw2, r2 + 128
        RET
%endmacro

;---------------------------------------------------------
; Generic byte shuffle (packed swizzle, endian swap, etc)

%macro shuffle 0
op shuffle
        VBROADCASTI128 m8, [implq + SwsOpImpl.priv]
        LOAD_CONT r2
IF X,   pshufb mx, m8
IF Y,   pshufb my, m8
IF Z,   pshufb mz, m8
IF W,   pshufb mw, m8
%if V2
IF X,   pshufb mx2, m8
IF Y,   pshufb my2, m8
IF Z,   pshufb mz2, m8
IF W,   pshufb mw2, m8
%endif
        CONTINUE r2
%endmacro

;---------------------------------------------------------
; Clearing

%macro clear_alpha 3 ; idx, vreg, vreg2
op clear_alpha%1
        LOAD_CONT r2
        pcmpeqb %2, %2
IF V2,  mova %3, %2
        CONTINUE r2
%endmacro

%macro clear_zero 3 ; idx, vreg, vreg2
op clear_zero%1
        LOAD_CONT r2
        pxor %2, %2
IF V2,  mova %3, %2
        CONTINUE r2
%endmacro

%macro clear 2 ; suffix, size
op clear%1
        LOAD_CONT r2
IF !X,  vpbroadcast%1 mx, [implq + SwsOpImpl.priv + 0 * %2]
IF !Y,  vpbroadcast%1 my, [implq + SwsOpImpl.priv + 1 * %2]
IF !Z,  vpbroadcast%1 mz, [implq + SwsOpImpl.priv + 2 * %2]
IF !W,  vpbroadcast%1 mw, [implq + SwsOpImpl.priv + 3 * %2]
    %if V2
IF !X,  vpbroadcast%1 mx2, [implq + SwsOpImpl.priv + 0 * %2]
IF !Y,  vpbroadcast%1 my2, [implq + SwsOpImpl.priv + 1 * %2]
IF !Z,  vpbroadcast%1 mz2, [implq + SwsOpImpl.priv + 2 * %2]
IF !W,  vpbroadcast%1 mw2, [implq + SwsOpImpl.priv + 3 * %2]
    %endif
        CONTINUE r2
%endmacro

%macro clear_funcs 2 ; suffix, size
        decl_pattern 1, 1, 1, 0, clear %1, %2
        decl_pattern 0, 1, 1, 1, clear %1, %2
        decl_pattern 0, 0, 1, 1, clear %1, %2
        decl_pattern 1, 0, 0, 1, clear %1, %2
        decl_pattern 1, 1, 0, 0, clear %1, %2
        decl_pattern 0, 1, 0, 1, clear %1, %2
        decl_pattern 1, 0, 1, 0, clear %1, %2
        decl_pattern 1, 0, 0, 0, clear %1, %2
        decl_pattern 0, 1, 0, 0, clear %1, %2
        decl_pattern 0, 0, 1, 0, clear %1, %2
%endmacro

;---------------------------------------------------------
; Swizzling and duplicating

; mA := mB, mB := mC, ... mX := mA
%macro vrotate 2-* ; A, B, C, ...
    %rep %0
        %assign rot_a %1 + 4
        %assign rot_b %2 + 4
        mova m%1, m%2
        IF V2, mova m%[rot_a], m%[rot_b]
    %rotate 1
    %endrep
    %undef rot_a
    %undef rot_b
%endmacro

%macro swizzle_funcs 0
op swizzle_3012
    LOAD_CONT r2
    vrotate 8, 0, 3, 2, 1
    CONTINUE r2

op swizzle_3021
    LOAD_CONT r2
    vrotate 8, 0, 3, 1
    CONTINUE r2

op swizzle_2103
    LOAD_CONT r2
    vrotate 8, 0, 2
    CONTINUE r2

op swizzle_3210
    LOAD_CONT r2
    vrotate 8, 0, 3
    vrotate 8, 1, 2
    CONTINUE r2

op swizzle_3102
    LOAD_CONT r2
    vrotate 8, 0, 3, 2
    CONTINUE r2

op swizzle_3201
    LOAD_CONT r2
    vrotate 8, 0, 3, 1, 2
    CONTINUE r2

op swizzle_1203
    LOAD_CONT r2
    vrotate 8, 0, 1, 2
    CONTINUE r2

op swizzle_1023
    LOAD_CONT r2
    vrotate 8, 0, 1
    CONTINUE r2

op swizzle_2013
    LOAD_CONT r2
    vrotate 8, 0, 2, 1
    CONTINUE r2

op swizzle_2310
    LOAD_CONT r2
    vrotate 8, 0, 2, 1, 3
    CONTINUE r2

op swizzle_2130
    LOAD_CONT r2
    vrotate 8, 0, 2, 3
    CONTINUE r2

op swizzle_1230
    LOAD_CONT r2
    vrotate 8, 0, 1, 2, 3
    CONTINUE r2

op swizzle_1320
    LOAD_CONT r2
    vrotate 8, 0, 1, 3
    CONTINUE r2

op swizzle_0213
    LOAD_CONT r2
    vrotate 8, 1, 2
    CONTINUE r2

op swizzle_0231
    LOAD_CONT r2
    vrotate 8, 1, 2, 3
    CONTINUE r2

op swizzle_0312
    LOAD_CONT r2
    vrotate 8, 1, 3, 2
    CONTINUE r2

op swizzle_3120
    LOAD_CONT r2
    vrotate 8, 0, 3
    CONTINUE r2

op swizzle_0321
    LOAD_CONT r2
    vrotate 8, 1, 3
    CONTINUE r2

op swizzle_0003
    LOAD_CONT r2
    mova my, mx
    mova mz, mx
%if V2
    mova my2, mx2
    mova mz2, mx2
%endif
    CONTINUE r2

op swizzle_0001
    LOAD_CONT r2
    mova mw, my
    mova mz, mx
    mova my, mx
%if V2
    mova mw2, my2
    mova mz2, mx2
    mova my2, mx2
%endif
    CONTINUE r2

op swizzle_3000
    LOAD_CONT r2
    mova my, mx
    mova mz, mx
    mova mx, mw
    mova mw, my
%if V2
    mova my2, mx2
    mova mz2, mx2
    mova mx2, mw2
    mova mw2, my2
%endif
    CONTINUE r2

op swizzle_1000
    LOAD_CONT r2
    mova mz, mx
    mova mw, mx
    mova mx, my
    mova my, mz
%if V2
    mova mz2, mx2
    mova mw2, mx2
    mova mx2, my2
    mova my2, mz2
%endif
    CONTINUE r2
%endmacro

;---------------------------------------------------------
; Pixel type conversions

%macro conv8to16 1 ; type
op %1_U8_U16
        LOAD_CONT r2
%if V2
IF X,   vextracti128 xmx2, mx, 1
IF Y,   vextracti128 xmy2, my, 1
IF Z,   vextracti128 xmz2, mz, 1
IF W,   vextracti128 xmw2, mw, 1
IF X,   pmovzxbw mx2, xmx2
IF Y,   pmovzxbw my2, xmy2
IF Z,   pmovzxbw mz2, xmz2
IF W,   pmovzxbw mw2, xmw2
%endif ; V2
IF X,   pmovzxbw mx, xmx
IF Y,   pmovzxbw my, xmy
IF Z,   pmovzxbw mz, xmz
IF W,   pmovzxbw mw, xmw
%ifidn %1, expand
    VBROADCASTI128 m8, [expand16_shuf]
    %if V2
IF X,   pshufb mx2, m8
IF Y,   pshufb my2, m8
IF Z,   pshufb mz2, m8
IF W,   pshufb mw2, m8
    %endif
IF X,   pshufb mx, m8
IF Y,   pshufb my, m8
IF Z,   pshufb mz, m8
IF W,   pshufb mw, m8
%endif ; expand
        CONTINUE r2
%endmacro

%macro conv16to8 0
op convert_U16_U8
        LOAD_CONT r2
%if V2
        ; this code technically works for the !V2 case as well, but slower
IF X,   packuswb mx, mx2
IF Y,   packuswb my, my2
IF Z,   packuswb mz, mz2
IF W,   packuswb mw, mw2
IF X,   vpermq mx, mx, q3120
IF Y,   vpermq my, my, q3120
IF Z,   vpermq mz, mz, q3120
IF W,   vpermq mw, mw, q3120
%else
IF X,   vextracti128  xm8, mx, 1
IF Y,   vextracti128  xm9, my, 1
IF Z,   vextracti128 xm10, mz, 1
IF W,   vextracti128 xm11, mw, 1
IF X,   packuswb xmx, xm8
IF Y,   packuswb xmy, xm9
IF Z,   packuswb xmz, xm10
IF W,   packuswb xmw, xm11
%endif
        CONTINUE r2
%endmacro

%macro conv8to32 1 ; type
op %1_U8_U32
        LOAD_CONT r2
IF X,   vpsrldq xmx2, xmx, 8
IF Y,   vpsrldq xmy2, xmy, 8
IF Z,   vpsrldq xmz2, xmz, 8
IF W,   vpsrldq xmw2, xmw, 8
IF X,   pmovzxbd mx, xmx
IF Y,   pmovzxbd my, xmy
IF Z,   pmovzxbd mz, xmz
IF W,   pmovzxbd mw, xmw
IF X,   pmovzxbd mx2, xmx2
IF Y,   pmovzxbd my2, xmy2
IF Z,   pmovzxbd mz2, xmz2
IF W,   pmovzxbd mw2, xmw2
%ifidn %1, expand
        VBROADCASTI128 m8, [expand32_shuf]
IF X,   pshufb mx, m8
IF Y,   pshufb my, m8
IF Z,   pshufb mz, m8
IF W,   pshufb mw, m8
IF X,   pshufb mx2, m8
IF Y,   pshufb my2, m8
IF Z,   pshufb mz2, m8
IF W,   pshufb mw2, m8
%endif ; expand
        CONTINUE r2
%endmacro

%macro conv32to8 0
op convert_U32_U8
        LOAD_CONT r2
IF X,   packusdw mx, mx2
IF Y,   packusdw my, my2
IF Z,   packusdw mz, mz2
IF W,   packusdw mw, mw2
IF X,   vextracti128 xmx2, mx, 1
IF Y,   vextracti128 xmy2, my, 1
IF Z,   vextracti128 xmz2, mz, 1
IF W,   vextracti128 xmw2, mw, 1
IF X,   packuswb xmx, xmx2
IF Y,   packuswb xmy, xmy2
IF Z,   packuswb xmz, xmz2
IF W,   packuswb xmw, xmw2
IF X,   vpshufd xmx, xmx, q3120
IF Y,   vpshufd xmy, xmy, q3120
IF Z,   vpshufd xmz, xmz, q3120
IF W,   vpshufd xmw, xmw, q3120
        CONTINUE r2
%endmacro

%macro conv16to32 0
op convert_U16_U32
        LOAD_CONT r2
IF X,   vextracti128 xmx2, mx, 1
IF Y,   vextracti128 xmy2, my, 1
IF Z,   vextracti128 xmz2, mz, 1
IF W,   vextracti128 xmw2, mw, 1
IF X,   pmovzxwd mx, xmx
IF Y,   pmovzxwd my, xmy
IF Z,   pmovzxwd mz, xmz
IF W,   pmovzxwd mw, xmw
IF X,   pmovzxwd mx2, xmx2
IF Y,   pmovzxwd my2, xmy2
IF Z,   pmovzxwd mz2, xmz2
IF W,   pmovzxwd mw2, xmw2
        CONTINUE r2
%endmacro

%macro conv32to16 0
op convert_U32_U16
        LOAD_CONT r2
IF X,   packusdw mx, mx2
IF Y,   packusdw my, my2
IF Z,   packusdw mz, mz2
IF W,   packusdw mw, mw2
IF X,   vpermq mx, mx, q3120
IF Y,   vpermq my, my, q3120
IF Z,   vpermq mz, mz, q3120
IF W,   vpermq mw, mw, q3120
        CONTINUE r2
%endmacro

;---------------------------------------------------------
; Shifting

%macro lshift16 0
op lshift16
        vmovq xm8, [implq + SwsOpImpl.priv]
        LOAD_CONT r2
IF X,   psllw mx, xm8
IF Y,   psllw my, xm8
IF Z,   psllw mz, xm8
IF W,   psllw mw, xm8
%if V2
IF X,   psllw mx2, xm8
IF Y,   psllw my2, xm8
IF Z,   psllw mz2, xm8
IF W,   psllw mw2, xm8
%endif
        CONTINUE r2
%endmacro

%macro rshift16 0
op rshift16
        vmovq xm8, [implq + SwsOpImpl.priv]
        LOAD_CONT r2
IF X,   psrlw mx, xm8
IF Y,   psrlw my, xm8
IF Z,   psrlw mz, xm8
IF W,   psrlw mw, xm8
%if V2
IF X,   psrlw mx2, xm8
IF Y,   psrlw my2, xm8
IF Z,   psrlw mz2, xm8
IF W,   psrlw mw2, xm8
%endif
        CONTINUE r2
%endmacro

;---------------------------------------------------------
; Function instantiations

%macro funcs_u8 0
    read_planar 1
    read_planar 2
    read_planar 3
    read_planar 4
    read8_packed2
    read8_packed3
    read8_packed4
    write_planar 1
    write_planar 2
    write_planar 3
    write_planar 4
    write8_packed2
    write8_packed4

    clear_alpha 0, mx, mx2
    clear_alpha 1, my, my2
    clear_alpha 3, mw, mw2
    clear_zero  0, mx, mx2
    clear_zero  1, my, my2
    clear_zero  3, mw, mw2
    clear_funcs b, 1
    swizzle_funcs
    decl_common_patterns shuffle
%endmacro

%macro funcs_u16 0
    decl_common_patterns conv8to16 convert
    decl_common_patterns conv8to16 expand
    decl_common_patterns conv16to8
    decl_common_patterns lshift16
    decl_common_patterns rshift16
    clear_funcs w, 2
%endmacro

INIT_XMM ssse3
decl_v2 0, funcs_u8
decl_v2 1, funcs_u8

INIT_YMM avx2
decl_v2 0, funcs_u8
decl_v2 1, funcs_u8
decl_v2 0, funcs_u16
decl_v2 1, funcs_u16

INIT_YMM avx2
decl_common_patterns conv8to32 convert
decl_common_patterns conv8to32 expand
decl_common_patterns conv32to8
decl_common_patterns conv16to32
decl_common_patterns conv32to16
