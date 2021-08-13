#pragma once

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum SIRVMExitCode {
    SIRVMExitCode_Success = 0,
    SIRVMExitCode_StackOverflow,
    SIRVMExitCode_ExecutionError,
} SIRVMExitCode;

typedef enum SIRVMRegister {
    SIRVMRegister_1,
    SIRVMRegister_2,
    SIRVMRegister_3,
    SIRVMRegister_4,
    SIRVMRegister_5,
    SIRVMRegister_6,
    SIRVMRegister_7,
    SIRVMRegister_8,
    SIRVMRegister_BP,
    SIRVMRegister_SP,
    SIRVMRegister_MAX,
} SIRVMRegister;

typedef enum SIRVMOpCode {
    SIRVMOpCode_NOP = 0,

    SIRVMOpCode_Halt,
    SIRVMOpCode_HaltError,

    // Stores bp+offset32 to reg
    SIRVMOpCode_LEASR, // op reg offset32

    // Stores address of global at offset32 to reg
    SIRVMOpCode_LEAGR, // op reg offset32

    // Moves immediate to register
    // op reg imm
    SIRVMOpCode_MovIR8,
    SIRVMOpCode_MovIR16,
    SIRVMOpCode_MovIR32,
    SIRVMOpCode_MovIR64,

    // Moves register to register
    // op reg/reg
    SIRVMOpCode_MovRR8,
    SIRVMOpCode_MovRR16,
    SIRVMOpCode_MovRR32,
    SIRVMOpCode_MovRR64,

    // Moves from register to address stored in register
    // op reg/reg
    SIRVMOpCode_MovRA8,
    SIRVMOpCode_MovRA16,
    SIRVMOpCode_MovRA32,
    SIRVMOpCode_MovRA64,

    // Moves from address stored in register to register
    // op reg/reg
    SIRVMOpCode_MovAR8,
    SIRVMOpCode_MovAR16,
    SIRVMOpCode_MovAR32,
    SIRVMOpCode_MovAR64,

    SIRVMOpCode_JumpI32,
    SIRVMOpCode_CondJumpR8I32,

    SIRVMOpCode_IAdd8,
    SIRVMOpCode_IAdd16,
    SIRVMOpCode_IAdd32,
    SIRVMOpCode_IAdd64,
    SIRVMOpCode_ISub8,
    SIRVMOpCode_ISub16,
    SIRVMOpCode_ISub32,
    SIRVMOpCode_ISub64,
    SIRVMOpCode_IMul8,
    SIRVMOpCode_IMul16,
    SIRVMOpCode_IMul32,
    SIRVMOpCode_IMul64,
    SIRVMOpCode_SDiv8,
    SIRVMOpCode_SDiv16,
    SIRVMOpCode_SDiv32,
    SIRVMOpCode_SDiv64,
    SIRVMOpCode_UDiv8,
    SIRVMOpCode_UDiv16,
    SIRVMOpCode_UDiv32,
    SIRVMOpCode_UDiv64,
    SIRVMOpCode_SRem8,
    SIRVMOpCode_SRem16,
    SIRVMOpCode_SRem32,
    SIRVMOpCode_SRem64,
    SIRVMOpCode_URem8,
    SIRVMOpCode_URem16,
    SIRVMOpCode_URem32,
    SIRVMOpCode_URem64,

    SIRVMOpCode_BitAnd8,
    SIRVMOpCode_BitAnd16,
    SIRVMOpCode_BitAnd32,
    SIRVMOpCode_BitAnd64,
    SIRVMOpCode_BitOr8,
    SIRVMOpCode_BitOr16,
    SIRVMOpCode_BitOr32,
    SIRVMOpCode_BitOr64,
    SIRVMOpCode_BitXor8,
    SIRVMOpCode_BitXor16,
    SIRVMOpCode_BitXor32,
    SIRVMOpCode_BitXor64,

    SIRVMOpCode_AShr8,
    SIRVMOpCode_AShr16,
    SIRVMOpCode_AShr32,
    SIRVMOpCode_AShr64,
    SIRVMOpCode_LShr8,
    SIRVMOpCode_LShr16,
    SIRVMOpCode_LShr32,
    SIRVMOpCode_LShr64,
    SIRVMOpCode_Shl8,
    SIRVMOpCode_Shl16,
    SIRVMOpCode_Shl32,
    SIRVMOpCode_Shl64,

    SIRVMOpCode_FAdd32,
    SIRVMOpCode_FAdd64,
    SIRVMOpCode_FSub32,
    SIRVMOpCode_FSub64,
    SIRVMOpCode_FMul32,
    SIRVMOpCode_FMul64,
    SIRVMOpCode_FDiv32,
    SIRVMOpCode_FDiv64,
    SIRVMOpCode_FRem32,
    SIRVMOpCode_FRem64,
} SIRVMOpCode;

typedef struct SIRVM SIRVM;

SIRVM *SIRVMCreate();
void SIRVMDestroy(SIRVM *vm);
void SIRVMRunTestProgram(SIRVM *vm);
SIRVMExitCode SIRVMRun(SIRVM *vm, void **data, size_t *data_size);
void SIRVMDisassemble(const uint8_t *program, size_t program_size);

#ifdef __cplusplus
}
#endif
