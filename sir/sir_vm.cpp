#include "sir_base.hpp"
#include "sir_vm.h"
#include <math.h>

struct SIRVM {
    uint32_t ip;
    uint32_t bp;
    bool running;
    SIRVMExitCode exit_code;
    SIRArray<uint8_t> program;
    SIRArray<uint8_t> stack;
};

SIRVM *SIRVMCreate()
{
    SIRVM *vm = SIRAllocInit(&SIR_MALLOC_ALLOCATOR, SIRVM);
    vm->program = SIRArray<uint8_t>::create(&SIR_MALLOC_ALLOCATOR);
    vm->stack = SIRArray<uint8_t>::create(&SIR_MALLOC_ALLOCATOR);
    vm->stack.reserve(1 << 22);
    return vm;
}

void SIRVMDestroy(SIRVM *vm)
{
    vm->program.destroy();
    vm->stack.destroy();
    SIRFree(&SIR_MALLOC_ALLOCATOR, vm);
}

void SIRVMBuildOpCode(SIRVM *vm, SIRVMOpCode op)
{
    vm->program.push_back((uint8_t)op);
}

void SIRVMBuildI8(SIRVM *vm, uint8_t u8)
{
    vm->program.push_back(u8);
}

void SIRVMBuildI16(SIRVM *vm, uint16_t val)
{
    uint8_t *u8 = (uint8_t *)&val;
    vm->program.push_many({u8, sizeof(val)});
}

void SIRVMBuildI32(SIRVM *vm, uint32_t val)
{
    uint8_t *u8 = (uint8_t *)&val;
    vm->program.push_many({u8, sizeof(val)});
}

void SIRVMBuildI64(SIRVM *vm, uint64_t val)
{
    uint8_t *u8 = (uint8_t *)&val;
    vm->program.push_many({u8, sizeof(val)});
}

void SIRVMBuildF32(SIRVM *vm, float val)
{
    uint8_t *u8 = (uint8_t *)&val;
    vm->program.push_many({u8, sizeof(val)});
}

void SIRVMBuildF64(SIRVM *vm, double val)
{
    uint8_t *u8 = (uint8_t *)&val;
    vm->program.push_many({u8, sizeof(val)});
}

SIR_INLINE void SIRVMPush(SIRVM *vm, void *val, size_t size)
{
    if (vm->stack.len + size > vm->stack.cap) {
        vm->running = false;
        vm->exit_code = SIRVMExitCode_StackOverflow;
        return;
    }
    memcpy(vm->stack.ptr + vm->stack.len, val, size);
    vm->stack.len += size;
}

SIR_INLINE void SIRVMPushI8(SIRVM *vm, uint8_t val)
{
    SIRVMPush(vm, &val, sizeof(val));
}

SIR_INLINE void SIRVMPushI16(SIRVM *vm, uint16_t val)
{
    SIRVMPush(vm, &val, sizeof(val));
}

SIR_INLINE void SIRVMPushI32(SIRVM *vm, uint32_t val)
{
    SIRVMPush(vm, &val, sizeof(val));
}

SIR_INLINE void SIRVMPushI64(SIRVM *vm, uint64_t val)
{
    SIRVMPush(vm, &val, sizeof(val));
}

SIR_INLINE void SIRVMPushF32(SIRVM *vm, float val)
{
    SIRVMPush(vm, &val, sizeof(val));
}

SIR_INLINE void SIRVMPushF64(SIRVM *vm, double val)
{
    SIRVMPush(vm, &val, sizeof(val));
}

SIR_INLINE uint8_t SIRVMPopI8(SIRVM *vm)
{
    SIR_ASSERT(vm->stack.len >= 1);
    vm->stack.len -= 1;
    return *((uint8_t *)&vm->stack.ptr[vm->stack.len]);
}

SIR_INLINE uint16_t SIRVMPopI16(SIRVM *vm)
{
    SIR_ASSERT(vm->stack.len >= 2);
    vm->stack.len -= 2;
    return *((uint16_t *)&vm->stack.ptr[vm->stack.len]);
}

SIR_INLINE uint32_t SIRVMPopI32(SIRVM *vm)
{
    SIR_ASSERT(vm->stack.len >= 4);
    vm->stack.len -= 4;
    return *((uint32_t *)&vm->stack.ptr[vm->stack.len]);
}

SIR_INLINE uint64_t SIRVMPopI64(SIRVM *vm)
{
    SIR_ASSERT(vm->stack.len >= 8);
    vm->stack.len -= 8;
    return *((uint64_t *)&vm->stack.ptr[vm->stack.len]);
}

SIR_INLINE float SIRVMPopF32(SIRVM *vm)
{
    SIR_ASSERT(vm->stack.len >= 4);
    vm->stack.len -= 4;
    return *((float *)&vm->stack.ptr[vm->stack.len]);
}

SIR_INLINE double SIRVMPopF64(SIRVM *vm)
{
    SIR_ASSERT(vm->stack.len >= 8);
    vm->stack.len -= 8;
    return *((double *)&vm->stack.ptr[vm->stack.len]);
}

void dump_hex(const void *data, size_t size)
{
    char ascii[17];
    size_t i, j;
    ascii[16] = '\0';
    for (i = 0; i < size; ++i) {
        printf("%02X ", ((unsigned char *)data)[i]);
        if (((unsigned char *)data)[i] >= ' ' &&
            ((unsigned char *)data)[i] <= '~') {
            ascii[i % 16] = ((unsigned char *)data)[i];
        } else {
            ascii[i % 16] = '.';
        }
        if ((i + 1) % 8 == 0 || i + 1 == size) {
            printf(" ");
            if ((i + 1) % 16 == 0) {
                printf("|  %s \n", ascii);
            } else if (i + 1 == size) {
                ascii[(i + 1) % 16] = '\0';
                if ((i + 1) % 16 <= 8) {
                    printf(" ");
                }
                for (j = (i + 1) % 16; j < 16; ++j) {
                    printf("   ");
                }
                printf("|  %s \n", ascii);
            }
        }
    }
}

void SIRVMRunTestProgram(SIRVM *vm)
{
    SIRVMBuildOpCode(vm, SIRVMOpCode_PushImm32);
    SIRVMBuildI32(vm, 1);
    SIRVMBuildOpCode(vm, SIRVMOpCode_PushImm32);
    SIRVMBuildI32(vm, 2);

    SIRVMBuildOpCode(vm, SIRVMOpCode_IAdd32);

    SIRVMBuildOpCode(vm, SIRVMOpCode_Halt);

    dump_hex(vm->program.ptr, vm->program.len);

    size_t data_size = 0;
    void *data = NULL;
    SIRVMExitCode exit_code = SIRVMRun(vm, &data, &data_size);
    SIR_ASSERT(exit_code == SIRVMExitCode_Success);

    SIR_ASSERT(data_size == 4);
    uint32_t u32 = *(uint32_t *)data;
    SIR_ASSERT(u32 == 3);
}

SIRVMExitCode SIRVMRun(SIRVM *vm, void **data, size_t *data_size)
{
    vm->ip = 0;
    vm->bp = 0;
    vm->stack.len = 0;
    vm->running = true;

    while (vm->running) {
        SIRVMOpCode op = (SIRVMOpCode)vm->program[vm->ip];
        vm->ip += 1;

        switch (op) {
        case SIRVMOpCode_NOP: break;

        case SIRVMOpCode_Halt: {
            vm->running = false;
            vm->exit_code = SIRVMExitCode_Success;
            break;
        }

        case SIRVMOpCode_HaltError: {
            vm->running = false;
            vm->exit_code = SIRVMExitCode_ExecutionError;
            break;
        }

        case SIRVMOpCode_PushImm8: {
            SIRVMPush(vm, &vm->program[vm->ip], 1);
            vm->ip += 1;
            break;
        }
        case SIRVMOpCode_PushImm16: {
            SIRVMPush(vm, &vm->program[vm->ip], 2);
            vm->ip += 2;
            break;
        }
        case SIRVMOpCode_PushImm32: {
            SIRVMPush(vm, &vm->program[vm->ip], 4);
            vm->ip += 4;
            break;
        }
        case SIRVMOpCode_PushImm64: {
            SIRVMPush(vm, &vm->program[vm->ip], 8);
            vm->ip += 8;
            break;
        }

        case SIRVMOpCode_PushBPRel8_Imm32: {
            int32_t offset = *((int32_t *)&vm->program[vm->ip]);
            SIRVMPush(vm, &vm->stack[vm->bp + offset], 1);
            vm->ip += 4;
            break;
        }
        case SIRVMOpCode_PushBPRel16_Imm32: {
            int32_t offset = *((int32_t *)&vm->program[vm->ip]);
            SIRVMPush(vm, &vm->stack[vm->bp + offset], 2);
            vm->ip += 4;
            break;
        }
        case SIRVMOpCode_PushBPRel32_Imm32: {
            int32_t offset = *((int32_t *)&vm->program[vm->ip]);
            SIRVMPush(vm, &vm->stack[vm->bp + offset], 4);
            vm->ip += 4;
            break;
        }
        case SIRVMOpCode_PushBPRel64_Imm32: {
            int32_t offset = *((int32_t *)&vm->program[vm->ip]);
            SIRVMPush(vm, &vm->stack[vm->bp + offset], 8);
            vm->ip += 4;
            break;
        }

        case SIRVMOpCode_PopBPRel8_Imm32: {
            int32_t offset = *((int32_t *)&vm->program[vm->ip]);
            uint8_t val = SIRVMPopI8(vm);
            uint8_t *ptr = (uint8_t *)&vm->stack[vm->bp + offset];
            *ptr = val;
            vm->ip += 4;
            break;
        }
        case SIRVMOpCode_PopBPRel16_Imm32: {
            int32_t offset = *((int32_t *)&vm->program[vm->ip]);
            uint16_t val = SIRVMPopI16(vm);
            uint16_t *ptr = (uint16_t *)&vm->stack[vm->bp + offset];
            *ptr = val;
            vm->ip += 4;
            break;
        }
        case SIRVMOpCode_PopBPRel32_Imm32: {
            int32_t offset = *((int32_t *)&vm->program[vm->ip]);
            uint32_t val = SIRVMPopI32(vm);
            uint32_t *ptr = (uint32_t *)&vm->stack[vm->bp + offset];
            *ptr = val;
            vm->ip += 4;
            break;
        }
        case SIRVMOpCode_PopBPRel64_Imm32: {
            int32_t offset = *((int32_t *)&vm->program[vm->ip]);
            uint64_t val = SIRVMPopI64(vm);
            uint64_t *ptr = (uint64_t *)&vm->stack[vm->bp + offset];
            *ptr = val;
            vm->ip += 4;
            break;
        }

        case SIRVMOpCode_JumpImm32: {
            vm->ip = *((uint32_t *)&vm->program[vm->ip]);
            break;
        }
        case SIRVMOpCode_CondJumpImm32: {
            uint8_t cond = SIRVMPopI8(vm);
            if (cond != 0) {
                vm->ip = *((uint32_t *)&vm->program[vm->ip]);
                break;
            }
            vm->ip += 4;
            break;
        }

        case SIRVMOpCode_IAdd8: {
            uint8_t a = SIRVMPopI8(vm);
            uint8_t b = SIRVMPopI8(vm);
            SIRVMPushI8(vm, a + b);
            break;
        }
        case SIRVMOpCode_IAdd16: {
            uint16_t a = SIRVMPopI16(vm);
            uint16_t b = SIRVMPopI16(vm);
            SIRVMPushI16(vm, a + b);
            break;
        }
        case SIRVMOpCode_IAdd32: {
            uint32_t a = SIRVMPopI32(vm);
            uint32_t b = SIRVMPopI32(vm);
            SIRVMPushI32(vm, a + b);
            break;
        }
        case SIRVMOpCode_IAdd64: {
            uint64_t a = SIRVMPopI64(vm);
            uint64_t b = SIRVMPopI64(vm);
            SIRVMPushI64(vm, a + b);
            break;
        }

        case SIRVMOpCode_ISub8: {
            uint8_t a = SIRVMPopI8(vm);
            uint8_t b = SIRVMPopI8(vm);
            SIRVMPushI8(vm, a - b);
            break;
        }
        case SIRVMOpCode_ISub16: {
            uint16_t a = SIRVMPopI16(vm);
            uint16_t b = SIRVMPopI16(vm);
            SIRVMPushI16(vm, a - b);
            break;
        }
        case SIRVMOpCode_ISub32: {
            uint32_t a = SIRVMPopI32(vm);
            uint32_t b = SIRVMPopI32(vm);
            SIRVMPushI32(vm, a - b);
            break;
        }
        case SIRVMOpCode_ISub64: {
            uint64_t a = SIRVMPopI64(vm);
            uint64_t b = SIRVMPopI64(vm);
            SIRVMPushI64(vm, a - b);
            break;
        }

        case SIRVMOpCode_IMul8: {
            uint8_t a = SIRVMPopI8(vm);
            uint8_t b = SIRVMPopI8(vm);
            SIRVMPushI8(vm, a * b);
            break;
        }
        case SIRVMOpCode_IMul16: {
            uint16_t a = SIRVMPopI16(vm);
            uint16_t b = SIRVMPopI16(vm);
            SIRVMPushI16(vm, a * b);
            break;
        }
        case SIRVMOpCode_IMul32: {
            uint32_t a = SIRVMPopI32(vm);
            uint32_t b = SIRVMPopI32(vm);
            SIRVMPushI32(vm, a * b);
            break;
        }
        case SIRVMOpCode_IMul64: {
            uint64_t a = SIRVMPopI64(vm);
            uint64_t b = SIRVMPopI64(vm);
            SIRVMPushI64(vm, a * b);
            break;
        }

        case SIRVMOpCode_SDiv8: {
            int8_t a = SIRVMPopI8(vm);
            int8_t b = SIRVMPopI8(vm);
            SIRVMPushI8(vm, a / b);
            break;
        }
        case SIRVMOpCode_SDiv16: {
            int16_t a = SIRVMPopI16(vm);
            int16_t b = SIRVMPopI16(vm);
            SIRVMPushI16(vm, a / b);
            break;
        }
        case SIRVMOpCode_SDiv32: {
            int32_t a = SIRVMPopI32(vm);
            int32_t b = SIRVMPopI32(vm);
            SIRVMPushI32(vm, a / b);
            break;
        }
        case SIRVMOpCode_SDiv64: {
            int64_t a = SIRVMPopI64(vm);
            int64_t b = SIRVMPopI64(vm);
            SIRVMPushI64(vm, a / b);
            break;
        }

        case SIRVMOpCode_UDiv8: {
            uint8_t a = SIRVMPopI8(vm);
            uint8_t b = SIRVMPopI8(vm);
            SIRVMPushI8(vm, a / b);
            break;
        }
        case SIRVMOpCode_UDiv16: {
            uint16_t a = SIRVMPopI16(vm);
            uint16_t b = SIRVMPopI16(vm);
            SIRVMPushI16(vm, a / b);
            break;
        }
        case SIRVMOpCode_UDiv32: {
            uint32_t a = SIRVMPopI32(vm);
            uint32_t b = SIRVMPopI32(vm);
            SIRVMPushI32(vm, a / b);
            break;
        }
        case SIRVMOpCode_UDiv64: {
            uint64_t a = SIRVMPopI64(vm);
            uint64_t b = SIRVMPopI64(vm);
            SIRVMPushI64(vm, a / b);
            break;
        }

        case SIRVMOpCode_SRem8: {
            int8_t a = SIRVMPopI8(vm);
            int8_t b = SIRVMPopI8(vm);
            SIRVMPushI8(vm, a % b);
            break;
        }
        case SIRVMOpCode_SRem16: {
            int16_t a = SIRVMPopI16(vm);
            int16_t b = SIRVMPopI16(vm);
            SIRVMPushI16(vm, a % b);
            break;
        }
        case SIRVMOpCode_SRem32: {
            int32_t a = SIRVMPopI32(vm);
            int32_t b = SIRVMPopI32(vm);
            SIRVMPushI32(vm, a % b);
            break;
        }
        case SIRVMOpCode_SRem64: {
            int64_t a = SIRVMPopI64(vm);
            int64_t b = SIRVMPopI64(vm);
            SIRVMPushI64(vm, a % b);
            break;
        }

        case SIRVMOpCode_URem8: {
            uint8_t a = SIRVMPopI8(vm);
            uint8_t b = SIRVMPopI8(vm);
            SIRVMPushI8(vm, a % b);
            break;
        }
        case SIRVMOpCode_URem16: {
            uint16_t a = SIRVMPopI16(vm);
            uint16_t b = SIRVMPopI16(vm);
            SIRVMPushI16(vm, a % b);
            break;
        }
        case SIRVMOpCode_URem32: {
            uint32_t a = SIRVMPopI32(vm);
            uint32_t b = SIRVMPopI32(vm);
            SIRVMPushI32(vm, a % b);
            break;
        }
        case SIRVMOpCode_URem64: {
            uint64_t a = SIRVMPopI64(vm);
            uint64_t b = SIRVMPopI64(vm);
            SIRVMPushI64(vm, a % b);
            break;
        }

        case SIRVMOpCode_BitAnd8: {
            uint8_t a = SIRVMPopI8(vm);
            uint8_t b = SIRVMPopI8(vm);
            SIRVMPushI8(vm, a & b);
            break;
        }
        case SIRVMOpCode_BitAnd16: {
            uint16_t a = SIRVMPopI16(vm);
            uint16_t b = SIRVMPopI16(vm);
            SIRVMPushI16(vm, a & b);
            break;
        }
        case SIRVMOpCode_BitAnd32: {
            uint32_t a = SIRVMPopI32(vm);
            uint32_t b = SIRVMPopI32(vm);
            SIRVMPushI32(vm, a & b);
            break;
        }
        case SIRVMOpCode_BitAnd64: {
            uint64_t a = SIRVMPopI64(vm);
            uint64_t b = SIRVMPopI64(vm);
            SIRVMPushI64(vm, a & b);
            break;
        }

        case SIRVMOpCode_BitOr8: {
            uint8_t a = SIRVMPopI8(vm);
            uint8_t b = SIRVMPopI8(vm);
            SIRVMPushI8(vm, a | b);
            break;
        }
        case SIRVMOpCode_BitOr16: {
            uint16_t a = SIRVMPopI16(vm);
            uint16_t b = SIRVMPopI16(vm);
            SIRVMPushI16(vm, a | b);
            break;
        }
        case SIRVMOpCode_BitOr32: {
            uint32_t a = SIRVMPopI32(vm);
            uint32_t b = SIRVMPopI32(vm);
            SIRVMPushI32(vm, a | b);
            break;
        }
        case SIRVMOpCode_BitOr64: {
            uint64_t a = SIRVMPopI64(vm);
            uint64_t b = SIRVMPopI64(vm);
            SIRVMPushI64(vm, a | b);
            break;
        }

        case SIRVMOpCode_BitXor8: {
            uint8_t a = SIRVMPopI8(vm);
            uint8_t b = SIRVMPopI8(vm);
            SIRVMPushI8(vm, a ^ b);
            break;
        }
        case SIRVMOpCode_BitXor16: {
            uint16_t a = SIRVMPopI16(vm);
            uint16_t b = SIRVMPopI16(vm);
            SIRVMPushI16(vm, a ^ b);
            break;
        }
        case SIRVMOpCode_BitXor32: {
            uint32_t a = SIRVMPopI32(vm);
            uint32_t b = SIRVMPopI32(vm);
            SIRVMPushI32(vm, a ^ b);
            break;
        }
        case SIRVMOpCode_BitXor64: {
            uint64_t a = SIRVMPopI64(vm);
            uint64_t b = SIRVMPopI64(vm);
            SIRVMPushI64(vm, a ^ b);
            break;
        }

        case SIRVMOpCode_AShr8: {
            int8_t a = SIRVMPopI8(vm);
            int8_t b = SIRVMPopI8(vm);
            SIRVMPushI8(vm, a >> b);
            break;
        }
        case SIRVMOpCode_AShr16: {
            int16_t a = SIRVMPopI16(vm);
            int16_t b = SIRVMPopI16(vm);
            SIRVMPushI16(vm, a >> b);
            break;
        }
        case SIRVMOpCode_AShr32: {
            int32_t a = SIRVMPopI32(vm);
            int32_t b = SIRVMPopI32(vm);
            SIRVMPushI32(vm, a >> b);
            break;
        }
        case SIRVMOpCode_AShr64: {
            int64_t a = SIRVMPopI64(vm);
            int64_t b = SIRVMPopI64(vm);
            SIRVMPushI64(vm, a >> b);
            break;
        }

        case SIRVMOpCode_LShr8: {
            uint8_t a = SIRVMPopI8(vm);
            uint8_t b = SIRVMPopI8(vm);
            SIRVMPushI8(vm, a >> b);
            break;
        }
        case SIRVMOpCode_LShr16: {
            uint16_t a = SIRVMPopI16(vm);
            uint16_t b = SIRVMPopI16(vm);
            SIRVMPushI16(vm, a >> b);
            break;
        }
        case SIRVMOpCode_LShr32: {
            uint32_t a = SIRVMPopI32(vm);
            uint32_t b = SIRVMPopI32(vm);
            SIRVMPushI32(vm, a >> b);
            break;
        }
        case SIRVMOpCode_LShr64: {
            uint64_t a = SIRVMPopI64(vm);
            uint64_t b = SIRVMPopI64(vm);
            SIRVMPushI64(vm, a >> b);
            break;
        }

        case SIRVMOpCode_Shl8: {
            uint8_t a = SIRVMPopI8(vm);
            uint8_t b = SIRVMPopI8(vm);
            SIRVMPushI8(vm, a << b);
            break;
        }
        case SIRVMOpCode_Shl16: {
            uint16_t a = SIRVMPopI16(vm);
            uint16_t b = SIRVMPopI16(vm);
            SIRVMPushI16(vm, a << b);
            break;
        }
        case SIRVMOpCode_Shl32: {
            uint32_t a = SIRVMPopI32(vm);
            uint32_t b = SIRVMPopI32(vm);
            SIRVMPushI32(vm, a << b);
            break;
        }
        case SIRVMOpCode_Shl64: {
            uint64_t a = SIRVMPopI64(vm);
            uint64_t b = SIRVMPopI64(vm);
            SIRVMPushI64(vm, a << b);
            break;
        }

        case SIRVMOpCode_FAdd32: {
            float a = SIRVMPopF32(vm);
            float b = SIRVMPopF32(vm);
            SIRVMPushF32(vm, a + b);
            break;
        }
        case SIRVMOpCode_FAdd64: {
            double a = SIRVMPopF64(vm);
            double b = SIRVMPopF64(vm);
            SIRVMPushF64(vm, a + b);
            break;
        }

        case SIRVMOpCode_FSub32: {
            float a = SIRVMPopF32(vm);
            float b = SIRVMPopF32(vm);
            SIRVMPushF32(vm, a - b);
            break;
        }
        case SIRVMOpCode_FSub64: {
            double a = SIRVMPopF64(vm);
            double b = SIRVMPopF64(vm);
            SIRVMPushF64(vm, a - b);
            break;
        }

        case SIRVMOpCode_FMul32: {
            float a = SIRVMPopF32(vm);
            float b = SIRVMPopF32(vm);
            SIRVMPushF32(vm, a * b);
            break;
        }
        case SIRVMOpCode_FMul64: {
            double a = SIRVMPopF64(vm);
            double b = SIRVMPopF64(vm);
            SIRVMPushF64(vm, a * b);
            break;
        }

        case SIRVMOpCode_FDiv32: {
            float a = SIRVMPopF32(vm);
            float b = SIRVMPopF32(vm);
            SIRVMPushF32(vm, a / b);
            break;
        }
        case SIRVMOpCode_FDiv64: {
            double a = SIRVMPopF64(vm);
            double b = SIRVMPopF64(vm);
            SIRVMPushF64(vm, a / b);
            break;
        }

        case SIRVMOpCode_FRem32: {
            float a = SIRVMPopF32(vm);
            float b = SIRVMPopF32(vm);
            SIRVMPushF32(vm, fmodf(a, b));
            break;
        }
        case SIRVMOpCode_FRem64: {
            double a = SIRVMPopF64(vm);
            double b = SIRVMPopF64(vm);
            SIRVMPushF64(vm, fmod(a, b));
            break;
        }
        }
    }

    *data = vm->stack.ptr;
    *data_size = vm->stack.len;
    return vm->exit_code;
}
