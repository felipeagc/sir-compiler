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

typedef enum SIRVMOpCode {
    SIRVMOpCode_NOP = 0,

    SIRVMOpCode_Halt,
    SIRVMOpCode_HaltError,

    SIRVMOpCode_PushImm8,
    SIRVMOpCode_PushImm16,
    SIRVMOpCode_PushImm32,
    SIRVMOpCode_PushImm64,

    // Pushes value stored in bp+imm_offset
    SIRVMOpCode_PushBPRel8_Imm32,
    SIRVMOpCode_PushBPRel16_Imm32,
    SIRVMOpCode_PushBPRel32_Imm32,
    SIRVMOpCode_PushBPRel64_Imm32,

    // Pops value and stores it to bp+imm_offset
    SIRVMOpCode_PopBPRel8_Imm32,
    SIRVMOpCode_PopBPRel16_Imm32,
    SIRVMOpCode_PopBPRel32_Imm32,
    SIRVMOpCode_PopBPRel64_Imm32,

    SIRVMOpCode_JumpImm32,
    SIRVMOpCode_CondJumpImm32,

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

#ifdef __cplusplus
}
#endif
