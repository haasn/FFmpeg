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
IF X,   pmovzxbd ymx, xmx
IF Y,   pmovzxbd ymy, xmy
IF Z,   pmovzxbd ymz, xmz
IF W,   pmovzxbd ymw, xmw
IF X,   pmovzxbd ymx2, xmx2
IF Y,   pmovzxbd ymy2, xmy2
IF Z,   pmovzxbd ymz2, xmz2
IF W,   pmovzxbd ymw2, xmw2
IF X,   vcvtdq2ps ymx, ymx
IF Y,   vcvtdq2ps ymy, ymy
IF Z,   vcvtdq2ps ymz, ymz
IF W,   vcvtdq2ps ymw, ymw
IF X,   vcvtdq2ps ymx2, ymx2
IF Y,   vcvtdq2ps ymy2, ymy2
IF Z,   vcvtdq2ps ymz2, ymz2
IF W,   vcvtdq2ps ymw2, ymw2
        CONTINUE r2
%endmacro

%macro conv16to32f 0
op convert_U16_F32
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
IF X,   vcvtdq2ps ymx, ymx
IF Y,   vcvtdq2ps ymy, ymy
IF Z,   vcvtdq2ps ymz, ymz
IF W,   vcvtdq2ps ymw, ymw
IF X,   vcvtdq2ps ymx2, ymx2
IF Y,   vcvtdq2ps ymy2, ymy2
IF Z,   vcvtdq2ps ymz2, ymz2
IF W,   vcvtdq2ps ymw2, ymw2
        CONTINUE r2
%endmacro

%macro conv32fto8 0
op convert_F32_U8
        LOAD_CONT r2
        CONTINUE r2
%endmacro

%macro conv32fto16 0
op convert_F32_U16
        LOAD_CONT r2
        CONTINUE r2
%endmacro

INIT_YMM avx2
decl_common_patterns conv8to32f
decl_common_patterns conv16to32f
decl_common_patterns conv32fto8
decl_common_patterns conv32fto16
