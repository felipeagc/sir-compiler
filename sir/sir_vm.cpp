#include "sir_base.hpp"
#include "sir_vm.h"
#include <math.h>

struct SIRVM {
    uint32_t ip;
    bool running;
    SIRVMExitCode exit_code;
    SIRArray<uint8_t> program;
    SIRArray<uint8_t> globals;
    uint8_t *stack;
    size_t stack_size;
    uint64_t registers[SIRVMRegister_MAX];
};

SIRVM *SIRVMCreate()
{
    SIRVM *vm = SIRAllocInit(&SIR_MALLOC_ALLOCATOR, SIRVM);
    vm->program = SIRArray<uint8_t>::create(&SIR_MALLOC_ALLOCATOR);
    vm->stack_size = 1 << 22;
    vm->stack = SIRAllocSlice(&SIR_MALLOC_ALLOCATOR, uint8_t, vm->stack_size);
    return vm;
}

void SIRVMDestroy(SIRVM *vm)
{
    vm->program.destroy();
    SIRFree(&SIR_MALLOC_ALLOCATOR, vm->stack);
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
    SIRVMBuildOpCode(vm, SIRVMOpCode_MovIR64);
    SIRVMBuildI8(vm, SIRVMRegister_1);
    SIRVMBuildI64(vm, 8);

    SIRVMBuildOpCode(vm, SIRVMOpCode_ISub64);
    SIRVMBuildI8(vm, (SIRVMRegister_BP << 4) | SIRVMRegister_1);

    SIRVMBuildOpCode(vm, SIRVMOpCode_LEASR);
    SIRVMBuildI8(vm, SIRVMRegister_1);
    SIRVMBuildI32(vm, 0);

    SIRVMBuildOpCode(vm, SIRVMOpCode_MovIR64);
    SIRVMBuildI8(vm, SIRVMRegister_2);
    SIRVMBuildI64(vm, 123);

    SIRVMBuildOpCode(vm, SIRVMOpCode_MovRA64);
    SIRVMBuildI8(vm, (SIRVMRegister_2 << 4) | SIRVMRegister_1);

    SIRVMBuildOpCode(vm, SIRVMOpCode_Halt);

    dump_hex(vm->program.ptr, vm->program.len);

    size_t data_size = 0;
    void *data = NULL;
    SIRVMExitCode exit_code = SIRVMRun(vm, &data, &data_size);
    SIR_ASSERT(exit_code == SIRVMExitCode_Success);

    printf("stack_end = %zu\n", (uint64_t)vm->stack + vm->stack_size);
    printf("bp        = %zu\n", vm->registers[SIRVMRegister_BP]);
    printf("r1        = %zu\n", vm->registers[SIRVMRegister_1]);

    uint64_t value = *(uint64_t *)vm->registers[SIRVMRegister_BP];
    printf("result    = %zu\n", value);
    SIR_ASSERT(value == 123);

    SIR_ASSERT(
        vm->registers[SIRVMRegister_1] == vm->registers[SIRVMRegister_BP]);

    /* SIR_ASSERT(data_size == 4); */
    /* uint32_t u32 = *(uint32_t *)data; */
    /* SIR_ASSERT(u32 == 3); */
}

SIRVMExitCode SIRVMRun(SIRVM *vm, void **data, size_t *data_size)
{
    vm->ip = 0;
    vm->running = true;

    vm->registers[SIRVMRegister_BP] = (uint64_t)(vm->stack + vm->stack_size);

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

        case SIRVMOpCode_LEASR: {
            uint8_t reg = vm->program[vm->ip++] & 0x0f;
            uint32_t stack_offset = *((uint32_t *)&vm->program[vm->ip]);
            vm->ip += 4;

            vm->registers[reg] = vm->registers[SIRVMRegister_BP] + stack_offset;
            break;
        }

        case SIRVMOpCode_LEAGR: {
            uint8_t reg = vm->program[vm->ip++] & 0x0f;
            uint32_t global_offset = *((uint32_t *)&vm->program[vm->ip]);
            vm->ip += 4;

            vm->registers[reg] = (uint64_t)&vm->globals[global_offset];
            break;
        }

        case SIRVMOpCode_MovIR8: {
            uint8_t reg = vm->program[vm->ip++] & 0x0f;
            uint8_t imm = *((uint8_t *)&vm->program[vm->ip]);
            vm->ip += 1;

            *(uint8_t *)&vm->registers[reg] = imm;
            break;
        }
        case SIRVMOpCode_MovIR16: {
            uint8_t reg = vm->program[vm->ip++] & 0x0f;
            uint16_t imm = *((uint16_t *)&vm->program[vm->ip]);
            vm->ip += 2;

            *(uint16_t *)&vm->registers[reg] = imm;
            break;
        }
        case SIRVMOpCode_MovIR32: {
            uint8_t reg = vm->program[vm->ip++] & 0x0f;
            uint32_t imm = *((uint32_t *)&vm->program[vm->ip]);
            vm->ip += 4;

            *(uint32_t *)&vm->registers[reg] = imm;
            break;
        }
        case SIRVMOpCode_MovIR64: {
            uint8_t reg = vm->program[vm->ip++] & 0x0f;
            uint64_t imm = *((uint64_t *)&vm->program[vm->ip]);
            vm->ip += 8;

            *(uint64_t *)&vm->registers[reg] = imm;
            break;
        }

        case SIRVMOpCode_MovRR8: {
            uint8_t ext = vm->program[vm->ip++];
            uint8_t reg_a = (ext >> 4) & 0x0f;
            uint8_t reg_b = ext & 0x0f;

            uint8_t *reg_a_ptr = (uint8_t *)&vm->registers[reg_a];
            uint8_t *reg_b_ptr = (uint8_t *)&vm->registers[reg_b];
            *reg_b_ptr = *reg_a_ptr;
            break;
        }
        case SIRVMOpCode_MovRR16: {
            uint8_t ext = vm->program[vm->ip++];
            uint8_t reg_a = (ext >> 4) & 0x0f;
            uint8_t reg_b = ext & 0x0f;

            uint16_t *reg_a_ptr = (uint16_t *)&vm->registers[reg_a];
            uint16_t *reg_b_ptr = (uint16_t *)&vm->registers[reg_b];
            *reg_b_ptr = *reg_a_ptr;
            break;
        }
        case SIRVMOpCode_MovRR32: {
            uint8_t ext = vm->program[vm->ip++];
            uint8_t reg_a = (ext >> 4) & 0x0f;
            uint8_t reg_b = ext & 0x0f;

            uint32_t *reg_a_ptr = (uint32_t *)&vm->registers[reg_a];
            uint32_t *reg_b_ptr = (uint32_t *)&vm->registers[reg_b];
            *reg_b_ptr = *reg_a_ptr;
            break;
        }
        case SIRVMOpCode_MovRR64: {
            uint8_t ext = vm->program[vm->ip++];
            uint8_t reg_a = (ext >> 4) & 0x0f;
            uint8_t reg_b = ext & 0x0f;

            uint64_t *reg_a_ptr = (uint64_t *)&vm->registers[reg_a];
            uint64_t *reg_b_ptr = (uint64_t *)&vm->registers[reg_b];
            *reg_b_ptr = *reg_a_ptr;
            break;
        }

        case SIRVMOpCode_MovRA8: {
            uint8_t ext = vm->program[vm->ip++];
            uint8_t reg_a = (ext >> 4) & 0x0f;
            uint8_t reg_b = ext & 0x0f;

            uint8_t val = *(uint8_t *)&vm->registers[reg_a];
            uint8_t *ptr = (uint8_t *)vm->registers[reg_b];
            *ptr = val;
            break;
        }
        case SIRVMOpCode_MovRA16: {
            uint8_t ext = vm->program[vm->ip++];
            uint8_t reg_a = (ext >> 4) & 0x0f;
            uint8_t reg_b = ext & 0x0f;

            uint16_t val = *(uint16_t *)&vm->registers[reg_a];
            uint16_t *ptr = (uint16_t *)vm->registers[reg_b];
            *ptr = val;
            break;
        }
        case SIRVMOpCode_MovRA32: {
            uint8_t ext = vm->program[vm->ip++];
            uint8_t reg_a = (ext >> 4) & 0x0f;
            uint8_t reg_b = ext & 0x0f;

            uint32_t val = *(uint32_t *)&vm->registers[reg_a];
            uint32_t *ptr = (uint32_t *)vm->registers[reg_b];
            *ptr = val;
            break;
        }
        case SIRVMOpCode_MovRA64: {
            uint8_t ext = vm->program[vm->ip++];
            uint8_t reg_a = (ext >> 4) & 0x0f;
            uint8_t reg_b = ext & 0x0f;

            uint64_t val = *(uint64_t *)&vm->registers[reg_a];
            uint64_t *ptr = (uint64_t *)vm->registers[reg_b];
            *ptr = val;
            break;
        }

        case SIRVMOpCode_MovAR8: {
            uint8_t ext = vm->program[vm->ip++];
            uint8_t reg_a = (ext >> 4) & 0x0f;
            uint8_t reg_b = ext & 0x0f;

            uint8_t val = *(uint8_t *)vm->registers[reg_a];
            uint8_t *ptr = (uint8_t *)&vm->registers[reg_b];
            *ptr = val;
            break;
        }
        case SIRVMOpCode_MovAR16: {
            uint8_t ext = vm->program[vm->ip++];
            uint8_t reg_a = (ext >> 4) & 0x0f;
            uint8_t reg_b = ext & 0x0f;

            uint16_t val = *(uint16_t *)vm->registers[reg_a];
            uint16_t *ptr = (uint16_t *)&vm->registers[reg_b];
            *ptr = val;
            break;
        }
        case SIRVMOpCode_MovAR32: {
            uint8_t ext = vm->program[vm->ip++];
            uint8_t reg_a = (ext >> 4) & 0x0f;
            uint8_t reg_b = ext & 0x0f;

            uint32_t val = *(uint32_t *)vm->registers[reg_a];
            uint32_t *ptr = (uint32_t *)&vm->registers[reg_b];
            *ptr = val;
            break;
        }
        case SIRVMOpCode_MovAR64: {
            uint8_t ext = vm->program[vm->ip++];
            uint8_t reg_a = (ext >> 4) & 0x0f;
            uint8_t reg_b = ext & 0x0f;

            uint64_t val = *(uint64_t *)vm->registers[reg_a];
            uint64_t *ptr = (uint64_t *)&vm->registers[reg_b];
            *ptr = val;
            break;
        }

        case SIRVMOpCode_JumpImm32: {
            vm->ip = *((uint32_t *)&vm->program[vm->ip]);
            break;
        }
        case SIRVMOpCode_CondReg_JumpImm32: {
            uint8_t reg = vm->program[vm->ip++] & 0x0f;
            uint64_t cond = vm->registers[reg];
            if (cond != 0) {
                vm->ip = *((uint32_t *)&vm->program[vm->ip]);
                break;
            }
            vm->ip += 4;
            break;
        }

#define REG_BINOP(OPCODE, TYPE, OP)                                            \
    case OPCODE: {                                                             \
        uint8_t ext = vm->program[vm->ip++];                                   \
        uint8_t reg_a = (ext >> 4) & 0x0f;                                     \
        uint8_t reg_b = ext & 0x0f;                                            \
        TYPE res = (*(TYPE *)&vm->registers[reg_a])OP(TYPE)(                   \
            *(TYPE *)&vm->registers[reg_b]);                                   \
        vm->registers[reg_a] = *(uint64_t *)&res;                              \
        break;                                                                 \
    }

            REG_BINOP(SIRVMOpCode_IAdd8, uint8_t, +);
            REG_BINOP(SIRVMOpCode_IAdd16, uint16_t, +);
            REG_BINOP(SIRVMOpCode_IAdd32, uint32_t, +);
            REG_BINOP(SIRVMOpCode_IAdd64, uint64_t, +);

            REG_BINOP(SIRVMOpCode_ISub8, uint8_t, -)
            REG_BINOP(SIRVMOpCode_ISub16, uint16_t, -)
            REG_BINOP(SIRVMOpCode_ISub32, uint32_t, -)
            REG_BINOP(SIRVMOpCode_ISub64, uint64_t, -)

            REG_BINOP(SIRVMOpCode_IMul8, uint8_t, *)
            REG_BINOP(SIRVMOpCode_IMul16, uint16_t, *)
            REG_BINOP(SIRVMOpCode_IMul32, uint32_t, *)
            REG_BINOP(SIRVMOpCode_IMul64, uint64_t, *)

            REG_BINOP(SIRVMOpCode_SDiv8, int8_t, /)
            REG_BINOP(SIRVMOpCode_SDiv16, int16_t, /)
            REG_BINOP(SIRVMOpCode_SDiv32, int32_t, /)
            REG_BINOP(SIRVMOpCode_SDiv64, int64_t, /)

            REG_BINOP(SIRVMOpCode_UDiv8, uint8_t, /)
            REG_BINOP(SIRVMOpCode_UDiv16, uint16_t, /)
            REG_BINOP(SIRVMOpCode_UDiv32, uint32_t, /)
            REG_BINOP(SIRVMOpCode_UDiv64, uint64_t, /)

            REG_BINOP(SIRVMOpCode_SRem8, int8_t, %)
            REG_BINOP(SIRVMOpCode_SRem16, int16_t, %)
            REG_BINOP(SIRVMOpCode_SRem32, int32_t, %)
            REG_BINOP(SIRVMOpCode_SRem64, int64_t, %)

            REG_BINOP(SIRVMOpCode_URem8, uint8_t, %)
            REG_BINOP(SIRVMOpCode_URem16, uint16_t, %)
            REG_BINOP(SIRVMOpCode_URem32, uint32_t, %)
            REG_BINOP(SIRVMOpCode_URem64, uint64_t, %)

            REG_BINOP(SIRVMOpCode_BitAnd8, uint8_t, &)
            REG_BINOP(SIRVMOpCode_BitAnd16, uint16_t, &)
            REG_BINOP(SIRVMOpCode_BitAnd32, uint32_t, &)
            REG_BINOP(SIRVMOpCode_BitAnd64, uint64_t, &)

            REG_BINOP(SIRVMOpCode_BitOr8, uint8_t, |)
            REG_BINOP(SIRVMOpCode_BitOr16, uint16_t, |)
            REG_BINOP(SIRVMOpCode_BitOr32, uint32_t, |)
            REG_BINOP(SIRVMOpCode_BitOr64, uint64_t, |)

            REG_BINOP(SIRVMOpCode_BitXor8, uint8_t, ^)
            REG_BINOP(SIRVMOpCode_BitXor16, uint16_t, ^)
            REG_BINOP(SIRVMOpCode_BitXor32, uint32_t, ^)
            REG_BINOP(SIRVMOpCode_BitXor64, uint64_t, ^)

            REG_BINOP(SIRVMOpCode_AShr8, int8_t, >>)
            REG_BINOP(SIRVMOpCode_AShr16, int16_t, >>)
            REG_BINOP(SIRVMOpCode_AShr32, int32_t, >>)
            REG_BINOP(SIRVMOpCode_AShr64, int64_t, >>)

            REG_BINOP(SIRVMOpCode_LShr8, uint8_t, >>)
            REG_BINOP(SIRVMOpCode_LShr16, uint16_t, >>)
            REG_BINOP(SIRVMOpCode_LShr32, uint32_t, >>)
            REG_BINOP(SIRVMOpCode_LShr64, uint64_t, >>)

            REG_BINOP(SIRVMOpCode_Shl8, uint8_t, <<)
            REG_BINOP(SIRVMOpCode_Shl16, uint16_t, <<)
            REG_BINOP(SIRVMOpCode_Shl32, uint32_t, <<)
            REG_BINOP(SIRVMOpCode_Shl64, uint64_t, <<)

            REG_BINOP(SIRVMOpCode_FAdd32, float, +)
            REG_BINOP(SIRVMOpCode_FAdd64, double, +)

            REG_BINOP(SIRVMOpCode_FSub32, float, -)
            REG_BINOP(SIRVMOpCode_FSub64, double, -)

            REG_BINOP(SIRVMOpCode_FMul32, float, *)
            REG_BINOP(SIRVMOpCode_FMul64, double, *)

            REG_BINOP(SIRVMOpCode_FDiv32, float, /)
            REG_BINOP(SIRVMOpCode_FDiv64, double, /)

        case SIRVMOpCode_FRem32: {
            uint8_t ext = vm->program[vm->ip++];
            uint8_t reg_a = (ext >> 4) & 0xff;
            uint8_t reg_b = ext & 0xff;
            float res = fmodf(
                (*(float *)&vm->registers[reg_a]),
                (*(float *)&vm->registers[reg_b]));
            vm->registers[reg_a] = *(uint64_t *)&res;
            break;
        }
        case SIRVMOpCode_FRem64: {
            uint8_t ext = vm->program[vm->ip++];
            uint8_t reg_a = (ext >> 4) & 0xff;
            uint8_t reg_b = ext & 0xff;
            double res = fmod(
                (*(double *)&vm->registers[reg_a]),
                (*(double *)&vm->registers[reg_b]));
            vm->registers[reg_a] = *(uint64_t *)&res;
            break;
        }
        }
    }

    *data = NULL;
    *data_size = 0;
    return vm->exit_code;
}
