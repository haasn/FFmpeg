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

SECTION .text

;---------------------------------------------------------
; Pixel type conversions

%macro conv8to32f 0
op convert_U8_F32
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
IF X,   vcvtdq2ps mx, mx
IF Y,   vcvtdq2ps my, my
IF Z,   vcvtdq2ps mz, mz
IF W,   vcvtdq2ps mw, mw
IF X,   vcvtdq2ps mx2, mx2
IF Y,   vcvtdq2ps my2, my2
IF Z,   vcvtdq2ps mz2, mz2
IF W,   vcvtdq2ps mw2, mw2
        CONTINUE r2
%endmacro

%macro conv16to32f 0
op convert_U16_F32
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
IF X,   vcvtdq2ps mx, mx
IF Y,   vcvtdq2ps my, my
IF Z,   vcvtdq2ps mz, mz
IF W,   vcvtdq2ps mw, mw
IF X,   vcvtdq2ps mx2, mx2
IF Y,   vcvtdq2ps my2, my2
IF Z,   vcvtdq2ps mz2, mz2
IF W,   vcvtdq2ps mw2, mw2
        CONTINUE r2
%endmacro

%macro conv32fto8 0
op convert_F32_U8
        LOAD_CONT r2
        ; TODO
        CONTINUE r2
%endmacro

%macro conv32fto16 0
op convert_F32_U16
        LOAD_CONT r2
        ; TODO
        CONTINUE r2
%endmacro

INIT_YMM avx2
decl_common_patterns conv8to32f
decl_common_patterns conv16to32f
decl_common_patterns conv32fto8
decl_common_patterns conv32fto16
