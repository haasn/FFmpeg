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

struc SwsOpExec
    .in0 resq 1
    .in1 resq 1
    .in2 resq 1
    .in3 resq 1
    .out0 resq 1
    .out1 resq 1
    .out2 resq 1
    .out3 resq 1
    .in_stride0 resq 1
    .in_stride1 resq 1
    .in_stride2 resq 1
    .in_stride3 resq 1
    .out_stride0 resq 1
    .out_stride1 resq 1
    .out_stride2 resq 1
    .out_stride3 resq 1
    .x resd 1
    .y resd 1
    .width resd 1
    .height resd 1
    .slice_y resd 1
    .slice_h resd 1
    .pixel_bits_in resd 1
    .pixel_bits_out resd 1
endstruc

struc SwsOpImpl
    .cont resb 16
    .priv resb 16
    .next resb 0
endstruc

; common macros for declaring operations
%macro op 1 ; name
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

    cglobal ADD_PAT(ADD_MUL(%1)), 0, 0, 0 ; already allocated by entry point

    %undef ADD_PAT
    %undef ADD_MUL
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

; common names for the internal calling convention
%define mx      m0
%define my      m1
%define mz      m2
%define mw      m3

%define xmx     xm0
%define xmy     xm1
%define xmz     xm2
%define xmw     xm3

%define ymx     ym0
%define ymy     ym1
%define ymz     ym2
%define ymw     ym3

%define mx2     m4
%define my2     m5
%define mz2     m6
%define mw2     m7

%define xmx2    xm4
%define xmy2    xm5
%define xmz2    xm6
%define xmw2    xm7

%define ymx2    ym4
%define ymy2    ym5
%define ymz2    ym6
%define ymw2    ym7

; from entry point signature
%define execq   r0q
%define implq   r1q
%define blocksd r2d
%define linesd  r3d
%define yd      linesd ; inside ops body

; extra registers for free use by kernels, not saved between ops
%define tmp0q   r4q
%define tmp1q   r5q
%define tmp2q   r6q

%define tmp0d   r4d
%define tmp1d   r5d
%define tmp2d   r6d

; pinned static registers for plane pointers
%define  in0q   r7q
%define out0q   r8q
%define  in1q   r9q
%define out1q   r10q
%define  in2q   r11q
%define out2q   r12q
%define  in3q   r13q
%define out3q   r14q

; load the next operation kernel
%macro LOAD_CONT 1 ; reg
    mov %1, [implq + SwsOpImpl.cont]
%endmacro

; tail call into the next operation kernel
%macro CONTINUE 1 ; reg
    add implq, SwsOpImpl.next
    jmp %1
    annotate_function_size
%endmacro

%macro CONTINUE 0
    LOAD_CONT tmp0q
    CONTINUE tmp0q
%endmacro

; helper for inline conditionals
%macro IF 2+ ; cond, body
    %if %1
        %2
    %endif
%endmacro

; alternate name for nested usage to work around some NASM bugs
%macro IF1 2+
    %if %1
        %2
    %endif
%endmacro

; move at least N pixels
%macro MOVSZ 2+ ; size, args
    %if %1 <= 4
        movd %2
    %elif %1 <= 8
        movq %2
    %else
        movu %2
    %endif
%endmacro
