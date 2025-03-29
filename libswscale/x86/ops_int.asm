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

expand16_shuf: db  0,  0,  2,  2,  4,  4,  6,  6,  8,  8, 10, 10, 12, 12, 14, 14, \
                  16, 16, 18, 18, 20, 20, 22, 22, 24, 24, 26, 26, 28, 28, 30, 30
expand32_shuf: db  0,  0,  0,  0,  4,  4,  4,  4,  8,  8,  8,  8, 12, 12, 12, 12, \
                  16, 16, 16, 16, 20, 20, 20, 20, 24, 24, 24, 24, 28, 28, 28, 28

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

%macro read8_packed 0
op read8_packed2
        mov r2, [execq + SwsOpExec.in0]
        LOAD_CONT r3
        movu mx, [r2]               ; YAYA low
        movu mw, [r2 + mmsize]      ; YAYA high
IF V2,  movu mx2, [r2 + 2*mmsize]   ; YAYA low
IF V2,  movu mw2, [r2 + 3*mmsize]   ; YAYA high
        pcmpeqb mz, mz, mz          ; FFFF
        psrlw mz, mz, 8             ; F0F0
        pand m6, mx, mz             ; Y0Y0 low
        pand m8, mw, mz             ; Y0Y0 high
        psrlw my, mx, 8             ; A0A0 low
        psrlw mw, mw, 8             ; A0A0 high
        packuswb mx, m6, m8         ; YYYY low+high
        packuswb my, my, mw         ; AAAA low+high
%if V2
        pand m6, mx2, mz            ; Y0Y0 low
        pand m8, mw2, mz            ; Y0Y0 high
        psrlw my2, mx2, 8           ; A0A0 low
        psrlw mw2, mw2, 8           ; A0A0 high
        packuswb mx2, m6, m8        ; YYYY low+high
        packuswb my2, my2, mw2      ; AAAA low+high
%endif
%if avx_enabled
        vpermq mx, mx, q3120
        vpermq my, my, q3120
IF V2,  vpermq mx2, mx2, q3120
IF V2,  vpermq my2, my2, q3120
%endif
    CONTINUE r3
%endmacro

;---------------------------------------------------------
; Clearing

%macro clear_alpha 3 ; idx, vreg, vreg2
op clear_alpha%1
        LOAD_CONT r2
        pcmpeqb %2, %2, %2
IF V2,  mova %3, %2
        CONTINUE r2
%endmacro

;---------------------------------------------------------
; Swizzling and duplicating

; mA := mB, mB := mC, ... mX := mA
%macro vrotate 2-* ; A, B, C, ...
    %rep %0
        %assign a %1 + 4
        %assign b %2 + 4
        mova m%1, m%2
        IF V2, mova m%[a], m%[b]
    %rotate 1
    %endrep
%endmacro

%macro swizzle_funcs 0
op swizzle_3012
    LOAD_CONT r2
    vrotate 8, 0, 3, 2, 1
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
    %if V2
IF X,   pshufb mx2, mx2, [expand16_shuf]
IF Y,   pshufb my2, my2, [expand16_shuf]
IF Z,   pshufb mz2, mz2, [expand16_shuf]
IF W,   pshufb mw2, mw2, [expand16_shuf]
    %endif
IF X,   pshufb mx, mx, [expand16_shuf]
IF Y,   pshufb my, my, [expand16_shuf]
IF Z,   pshufb mz, mz, [expand16_shuf]
IF W,   pshufb mw, mw, [expand16_shuf]
%endif ; expand
        CONTINUE r2
%endmacro

%macro conv16to8 0
op convert_U16_U8
        LOAD_CONT r2
%if V2
        ; this code technically works for the !V2 case as well, but slower
IF X,   packuswb ymx, ymx, ymx2
IF Y,   packuswb ymy, ymy, ymy2
IF Z,   packuswb ymz, ymz, ymz2
IF W,   packuswb ymw, ymw, ymw2
IF X,   vpermq ymx, ymx, q3120
IF Y,   vpermq ymy, ymy, q3120
IF Z,   vpermq ymz, ymz, q3120
IF W,   vpermq ymw, ymw, q3120
%else
IF X,   vextracti128  xm8, ymx, 1
IF Y,   vextracti128  xm9, ymy, 1
IF Z,   vextracti128 xm10, ymz, 1
IF W,   vextracti128 xm11, ymw, 1
IF X,   packuswb xmx, xmx, xm8
IF Y,   packuswb xmy, xmy, xm9
IF Z,   packuswb xmz, xmz, xm10
IF W,   packuswb xmw, xmw, xm11
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
IF X,   pmovzxbd ymx, xmx
IF Y,   pmovzxbd ymy, xmy
IF Z,   pmovzxbd ymz, xmz
IF W,   pmovzxbd ymw, xmw
IF X,   pmovzxbd ymx2, xmx2
IF Y,   pmovzxbd ymy2, xmy2
IF Z,   pmovzxbd ymz2, xmz2
IF W,   pmovzxbd ymw2, xmw2
%ifidn %1, expand
IF X,   pshufb mx, mx, [expand32_shuf]
IF Y,   pshufb my, my, [expand32_shuf]
IF Z,   pshufb mz, mz, [expand32_shuf]
IF W,   pshufb mw, mw, [expand32_shuf]
IF X,   pshufb mx2, mx2, [expand32_shuf]
IF Y,   pshufb my2, my2, [expand32_shuf]
IF Z,   pshufb mz2, mz2, [expand32_shuf]
IF W,   pshufb mw2, mw2, [expand32_shuf]
%endif ; expand
        CONTINUE r2
%endmacro

%macro conv32to8 0
op convert_U32_U8
        LOAD_CONT r2
IF X,   packusdw ymx, ymx, ymx2
IF Y,   packusdw ymy, ymy, ymy2
IF Z,   packusdw ymz, ymz, ymz2
IF W,   packusdw ymw, ymw, ymw2
IF X,   vextracti128 xmx2, ymx, 1
IF Y,   vextracti128 xmy2, ymy, 1
IF Z,   vextracti128 xmz2, ymz, 1
IF W,   vextracti128 xmw2, ymw, 1
IF X,   packuswb xmx, xmx, xmx2
IF Y,   packuswb xmy, xmy, xmy2
IF Z,   packuswb xmz, xmz, xmz2
IF W,   packuswb xmw, xmw, xmw2
IF X,   vpshufd xmx, xmx, q3120
IF Y,   vpshufd xmy, xmy, q3120
IF Z,   vpshufd xmz, xmz, q3120
IF W,   vpshufd xmw, xmw, q3120
        CONTINUE r2
%endmacro

%macro conv16to32 0
op convert_U16_U32
        LOAD_CONT r2
IF X,   vextracti128 xmx2, ymx, 1
IF Y,   vextracti128 xmy2, ymy, 1
IF Z,   vextracti128 xmz2, ymz, 1
IF W,   vextracti128 xmw2, ymw, 1
IF X,   pmovzxwd ymx, xmx
IF Y,   pmovzxwd ymy, xmy
IF Z,   pmovzxwd ymz, xmz
IF W,   pmovzxwd ymw, xmw
IF X,   pmovzxwd ymx2, xmx2
IF Y,   pmovzxwd ymy2, xmy2
IF Z,   pmovzxwd ymz2, xmz2
IF W,   pmovzxwd ymw2, xmw2
        CONTINUE r2
%endmacro

%macro conv32to16 0
op convert_U32_U16
        LOAD_CONT r2
IF X,   packusdw ymx, ymx, ymx2
IF Y,   packusdw ymy, ymy, ymy2
IF Z,   packusdw ymz, ymz, ymz2
IF W,   packusdw ymw, ymw, ymw2
IF X,   vpermq ymx, ymx, q3120
IF Y,   vpermq ymy, ymy, q3120
IF Z,   vpermq ymz, ymz, q3120
IF W,   vpermq ymw, ymw, q3120
        CONTINUE r2
%endmacro

;---------------------------------------------------------
; Shifting

%macro lshift16 0
op lshift16
        vmovq xm8, [implq + SwsOpImpl.priv]
        LOAD_CONT r2
IF X,   psllw mx, mx, xm8
IF Y,   psllw my, my, xm8
IF Z,   psllw mz, mz, xm8
IF W,   psllw mw, mw, xm8
%if V2
IF X,   psllw mx2, mx2, xm8
IF Y,   psllw my2, my2, xm8
IF Z,   psllw mz2, mz2, xm8
IF W,   psllw mw2, mw2, xm8
%endif
        CONTINUE r2
%endmacro

%macro rshift16 0
op rshift16
        vmovq xm8, [implq + SwsOpImpl.priv]
        LOAD_CONT r2
IF X,   psrlw mx, mx, xm8
IF Y,   psrlw my, my, xm8
IF Z,   psrlw mz, mz, xm8
IF W,   psrlw mw, mw, xm8
%if V2
IF X,   psrlw mx2, mx2, xm8
IF Y,   psrlw my2, my2, xm8
IF Z,   psrlw mz2, mz2, xm8
IF W,   psrlw mw2, mw2, xm8
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
    read8_packed
    write_planar 1
    write_planar 2
    write_planar 3
    write_planar 4
    clear_alpha 0, mx, mx2
    clear_alpha 1, my, my2
    clear_alpha 3, mw, mw2
    swizzle_funcs
%endmacro

%macro funcs_u16 0
    decl_common_patterns conv8to16 convert
    decl_common_patterns conv8to16 expand
    decl_common_patterns conv16to8
    decl_common_patterns lshift16
    decl_common_patterns rshift16
%endmacro

INIT_XMM sse2
decl_v2 0, funcs_u8

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
