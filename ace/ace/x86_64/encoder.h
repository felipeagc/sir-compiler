#pragma once

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
    FE_AX = 0x100, FE_CX, FE_DX, FE_BX, FE_SP, FE_BP, FE_SI, FE_DI,
    FE_R8, FE_R9, FE_R10, FE_R11, FE_R12, FE_R13, FE_R14, FE_R15, FE_IP,
    FE_AH = 0x204, FE_CH, FE_DH, FE_BH,
    FE_ES = 0x300, FE_CS, FE_SS, FE_DS, FE_FS, FE_GS,
    FE_ST0 = 0x400, FE_ST1, FE_ST2, FE_ST3, FE_ST4, FE_ST5, FE_ST6, FE_ST7,
    FE_MM0 = 0x500, FE_MM1, FE_MM2, FE_MM3, FE_MM4, FE_MM5, FE_MM6, FE_MM7,
    FE_XMM0 = 0x600, FE_XMM1, FE_XMM2, FE_XMM3, FE_XMM4, FE_XMM5, FE_XMM6, FE_XMM7,
    FE_XMM8, FE_XMM9, FE_XMM10, FE_XMM11, FE_XMM12, FE_XMM13, FE_XMM14, FE_XMM15,
} FeReg;

typedef int64_t FeOp;

/** Construct a memory operand. Unused parts can be set to 0 and will be
 * ignored. FE_IP can be used as base register, in which case the offset is
 * interpreted as the offset from the /current/ position -- the size of the
 * encoded instruction will be subtracted during encoding. scale must be 1, 2,
 * 4, or 8; but is ignored if  idx == 0. **/
#define FE_MEM(base,sc,idx,off) (INT64_MIN | ((int64_t) ((base) & 0xfff) << 32) | ((int64_t) ((idx) & 0xfff) << 44) | ((int64_t) ((sc) & 0xf) << 56) | ((off) & 0xffffffff))

/** Add segment override prefix. This may or may not generate prefixes for the
 * ignored prefixes ES/CS/DS/SS in 64-bit mode. **/
#define FE_SEG(seg) ((((seg) & 0x7) + 1) << 16)
/** Do not use. **/
#define FE_SEG_MASK 0x70000
/** Overrides address size. **/
#define FE_ADDR32 0x80000
/** Used together with a RIP-relative (conditional) jump, this will force the
 * use of the encoding with the largest distance. Useful for reserving a jump
 * when the target offset is still unknown; if the jump is re-encoded later on,
 * FE_JMPL must be specified there, too, so that the encoding lengths match. **/
#define FE_JMPL 0x100000
/** Do not use. **/
#define FE_MNEM_MASK 0xffff

enum {
#define FE_MNEMONIC(name,value) name = value,
#include "enc_mnems.h"
#undef FE_MNEMONIC
    FE_MNEM_MAX
};

/** Encode a single instruction for 64-bit mode.
 * \param buf Pointer to the buffer for instruction bytes, must have a size of
 *        15 bytes. The pointer is advanced by the number of bytes used for
 *        encoding the specified instruction.
 * \param mnem Mnemonic, optionally or-ed with FE_SEG(), FE_ADDR32, or FE_JMPL.
 * \param operands... Instruction operands. Immediate operands are passed as
 *        plain value; register operands using the FeReg enum; memory operands
 *        using FE_MEM(); and offset operands for RIP-relative jumps/calls are
 *        specified as _address in buf_, e.g. (intptr_t) jmptgt, the address of
 *        buf and the size of the encoded instruction are subtracted internally.
 * \return Zero for success or a negative value in case of an error.
 **/
int fe_enc64(uint8_t** buf, uint64_t mnem, FeOp op0, FeOp op1, FeOp op2, FeOp op3);

#ifdef __cplusplus
}
#endif
/*
Copyright (c) 2018, Alexis Engelke
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
*/
