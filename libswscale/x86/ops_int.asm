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

%include "libavutil/x86/x86util.asm"

SECTION .text

struc SwsOpExec
    .in0 resq 1
    .in1 resq 1
    .in2 resq 1
    .in3 resq 1
    .out0 resq 1
    .out1 resq 1
    .out2 resq 1
    .out3 resq 1
endstruc

struc SwsOpImpl
    .cont resb 16
    .priv resb 16
    .next resb 0
endstruc

; common macros for declaring operations
%macro DEF_OP_NAME 1 ; name
    %ifdef X
        %define ADD_PAT(name) p %+ X %+ Y %+ Z %+ W %+ _ %+ name
    %else
        %define ADD_PAT(name) name
    %endif

    %ifdef V2
        %if V2
            %define ADD_MUL(name) name %+ _m2
        %else
            %define ADD_MUL(name) name %+ _m1
        %endif
    %else
        %define ADD_MUL(name) name
    %endif

    %xdefine NAME ADD_PAT(ADD_MUL(%1))
    %undef ADD_PAT
    %undef ADD_MUL
%endmacro

%macro op 1 ; name
    DEF_OP_NAME %1
    cglobal NAME, 2, 7, 16, exec, impl
    %undef NAME
%endmacro

%macro decl_v2 2+ ; v2, func
    %xdefine V2 %1
    %2
    %undef V2
%endmacro

%macro decl_pattern 5+ ; X, Y, Z, W, func
    %xdefine X %1
    %xdefine Y %2
    %xdefine Z %3
    %xdefine W %4
    %5
    %undef X
    %undef Y
    %undef Z
    %undef W
%endmacro

%macro decl_common_patterns 1+ ; func
    decl_pattern 1, 0, 0, 0, %1 ; y
    decl_pattern 1, 0, 0, 1, %1 ; ya
    decl_pattern 1, 1, 1, 0, %1 ; yuv
    decl_pattern 1, 1, 1, 1, %1 ; yuva
%endmacro

; common names for the internal vector calling convention
%define  mx    m0
%define  my    m1
%define  mz    m2
%define  mw    m3
%define xmx   xm0
%define xmy   xm1
%define xmz   xm2
%define xmw   xm3
%define ymx   ym0
%define ymy   ym1
%define ymz   ym2
%define ymw   ym3

%define  mx2   m4
%define  my2   m5
%define  mz2   m6
%define  mw2   m7
%define xmx2  xm4
%define xmy2  xm5
%define xmz2  xm6
%define xmw2  xm7
%define ymx2  ym4
%define ymy2  ym5
%define ymz2  ym6
%define ymw2  ym7

; load the next operation kernel
%macro LOAD_CONT 1
    mov %1, [implq + SwsOpImpl.cont]
%endmacro

; tail call into the next operation kernel
%macro CONTINUE 1
    add implq, SwsOpImpl.next
    jmp %1
    annotate_function_size
%endmacro

; return from operations chain after write
%macro END 0
    ; always force a vzeroupper before returning from the last function
    %if !vzeroupper_required
        vzeroupper
    %endif
    RET
%endmacro

; helper for inline conditionals
%macro IF 2+ ; cond, body
    %if %1
        %2
    %endif
%endmacro

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

%macro conv8to16 0
op convert_8to16
        LOAD_CONT r2
%if V2
IF X,   vextracti128 xmx2, ymx, 1
IF Y,   vextracti128 xmy2, ymy, 1
IF Z,   vextracti128 xmz2, ymz, 1
IF W,   vextracti128 xmw2, ymw, 1
IF X,   pmovzxbw ymx2, xmx2
IF Y,   pmovzxbw ymy2, xmy2
IF Z,   pmovzxbw ymz2, xmz2
IF W,   pmovzxbw ymw2, xmw2
%endif
IF X,   pmovzxbw ymx, xmx
IF Y,   pmovzxbw ymy, xmy
IF Z,   pmovzxbw ymz, xmz
IF W,   pmovzxbw ymw, xmw
        CONTINUE r2
%endmacro

%macro conv16to8 0
op convert_16to8
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
    decl_common_patterns conv8to16
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
