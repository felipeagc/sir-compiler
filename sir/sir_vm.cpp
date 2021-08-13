#include "sir_base.hpp"
#include "sir_vm.h"
#include <math.h>

const char *SIRVMRegisterToString(SIRVMRegister reg)
{
    switch (reg) {
    case SIRVMRegister_1: return "r1";
    case SIRVMRegister_2: return "r2";
    case SIRVMRegister_3: return "r3";
    case SIRVMRegister_4: return "r4";
    case SIRVMRegister_5: return "r5";
    case SIRVMRegister_6: return "r6";
    case SIRVMRegister_7: return "r7";
    case SIRVMRegister_8: return "r8";
    case SIRVMRegister_BP: return "bp";
    case SIRVMRegister_SP: return "sp";
    case SIRVMRegister_MAX: return "<unknown>";
    }

    return "<unknown>";
}

const char *SIRVMOpCodeToString(SIRVMOpCode op)
{
    switch (op) {
    case SIRVMOpCode_NOP: return "nop";
    case SIRVMOpCode_Halt: return "halt";
    case SIRVMOpCode_HaltError: return "halt_error";
    case SIRVMOpCode_LEASR: return "leasr";
    case SIRVMOpCode_LEAGR: return "leagr";

    case SIRVMOpCode_MovIR8: return "mov_ir8";
    case SIRVMOpCode_MovIR16: return "mov_ir16";
    case SIRVMOpCode_MovIR32: return "mov_ir32";
    case SIRVMOpCode_MovIR64: return "mov_ir64";

    case SIRVMOpCode_MovRR8: return "mov_rr8";
    case SIRVMOpCode_MovRR16: return "mov_rr16";
    case SIRVMOpCode_MovRR32: return "mov_rr32";
    case SIRVMOpCode_MovRR64: return "mov_rr64";

    case SIRVMOpCode_MovAR8: return "mov_ar8";
    case SIRVMOpCode_MovAR16: return "mov_ar16";
    case SIRVMOpCode_MovAR32: return "mov_ar32";
    case SIRVMOpCode_MovAR64: return "mov_ar64";

    case SIRVMOpCode_MovRA8: return "mov_ra8";
    case SIRVMOpCode_MovRA16: return "mov_ra16";
    case SIRVMOpCode_MovRA32: return "mov_ra32";
    case SIRVMOpCode_MovRA64: return "mov_ra64";

    case SIRVMOpCode_JumpI32: return "jmp_i32";
    case SIRVMOpCode_CondJumpR8I32: return "condjmp_r8_i32";

    case SIRVMOpCode_IAdd8: return "iadd8";
    case SIRVMOpCode_IAdd16: return "iadd16";
    case SIRVMOpCode_IAdd32: return "iadd32";
    case SIRVMOpCode_IAdd64: return "iadd64";

    case SIRVMOpCode_ISub8: return "isub8";
    case SIRVMOpCode_ISub16: return "isub16";
    case SIRVMOpCode_ISub32: return "isub32";
    case SIRVMOpCode_ISub64: return "isub64";

    case SIRVMOpCode_IMul8: return "imul8";
    case SIRVMOpCode_IMul16: return "imul16";
    case SIRVMOpCode_IMul32: return "imul32";
    case SIRVMOpCode_IMul64: return "imul64";

    case SIRVMOpCode_SDiv8: return "sdiv8";
    case SIRVMOpCode_SDiv16: return "sdiv16";
    case SIRVMOpCode_SDiv32: return "sdiv32";
    case SIRVMOpCode_SDiv64: return "sdiv64";

    case SIRVMOpCode_UDiv8: return "udiv8";
    case SIRVMOpCode_UDiv16: return "udiv16";
    case SIRVMOpCode_UDiv32: return "udiv32";
    case SIRVMOpCode_UDiv64: return "udiv64";

    case SIRVMOpCode_SRem8: return "srem8";
    case SIRVMOpCode_SRem16: return "srem16";
    case SIRVMOpCode_SRem32: return "srem32";
    case SIRVMOpCode_SRem64: return "srem64";

    case SIRVMOpCode_URem8: return "urem8";
    case SIRVMOpCode_URem16: return "urem16";
    case SIRVMOpCode_URem32: return "urem32";
    case SIRVMOpCode_URem64: return "urem64";

    case SIRVMOpCode_BitAnd8: return "bitand8";
    case SIRVMOpCode_BitAnd16: return "bitand16";
    case SIRVMOpCode_BitAnd32: return "bitand32";
    case SIRVMOpCode_BitAnd64: return "bitand64";

    case SIRVMOpCode_BitOr8: return "bitor8";
    case SIRVMOpCode_BitOr16: return "bitor16";
    case SIRVMOpCode_BitOr32: return "bitor32";
    case SIRVMOpCode_BitOr64: return "bitor64";

    case SIRVMOpCode_BitXor8: return "bitxor8";
    case SIRVMOpCode_BitXor16: return "bitxor16";
    case SIRVMOpCode_BitXor32: return "bitxor32";
    case SIRVMOpCode_BitXor64: return "bitxor64";

    case SIRVMOpCode_AShr8: return "ashr8";
    case SIRVMOpCode_AShr16: return "ashr16";
    case SIRVMOpCode_AShr32: return "ashr32";
    case SIRVMOpCode_AShr64: return "ashr64";

    case SIRVMOpCode_LShr8: return "lshr8";
    case SIRVMOpCode_LShr16: return "lshr16";
    case SIRVMOpCode_LShr32: return "lshr32";
    case SIRVMOpCode_LShr64: return "lshr64";

    case SIRVMOpCode_Shl8: return "shl8";
    case SIRVMOpCode_Shl16: return "shl16";
    case SIRVMOpCode_Shl32: return "shl32";
    case SIRVMOpCode_Shl64: return "shl64";

    case SIRVMOpCode_FAdd32: return "fadd32";
    case SIRVMOpCode_FAdd64: return "fadd64";

    case SIRVMOpCode_FSub32: return "fsub32";
    case SIRVMOpCode_FSub64: return "fsub64";

    case SIRVMOpCode_FMul32: return "fmul32";
    case SIRVMOpCode_FMul64: return "fmul64";

    case SIRVMOpCode_FDiv32: return "fdiv32";
    case SIRVMOpCode_FDiv64: return "fdiv64";

    case SIRVMOpCode_FRem32: return "frem32";
    case SIRVMOpCode_FRem64: return "frem64";
    }
    return "<unknown>";
}

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

    SIRVMDisassemble(vm->program.ptr, vm->program.len);

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
            int32_t stack_offset = *((int32_t *)&vm->program[vm->ip]);
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

        case SIRVMOpCode_JumpI32: {
            vm->ip = *((uint32_t *)&vm->program[vm->ip]);
            break;
        }

        case SIRVMOpCode_CondJumpR8I32: {
            uint8_t reg = vm->program[vm->ip++] & 0x0f;
            uint8_t cond = *(uint8_t *)&vm->registers[reg];
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

void SIRVMDisassemble(const uint8_t *program, size_t program_size)
{
    size_t ip = 0;

    while (ip < program_size) {
        size_t inst_pos = ip;
        SIRVMOpCode op = (SIRVMOpCode)program[ip++];

        printf("%zu:  %s ", inst_pos, SIRVMOpCodeToString(op));

        switch (op) {
        case SIRVMOpCode_NOP:
        case SIRVMOpCode_Halt:
        case SIRVMOpCode_HaltError: break;

        case SIRVMOpCode_LEASR: {
            SIRVMRegister reg = (SIRVMRegister)(program[ip++] & 0x0f);
            int32_t stack_offset = *((int32_t *)&program[ip]);
            ip += 4;
            if (stack_offset >= 0) {
                printf("bp+%d, %s", stack_offset, SIRVMRegisterToString(reg));
            } else {
                printf("bp-%d, %s", stack_offset, SIRVMRegisterToString(reg));
            }
            break;
        }

        case SIRVMOpCode_LEAGR: {
            SIRVMRegister reg = (SIRVMRegister)(program[ip++] & 0x0f);
            uint32_t global_offset = *((uint32_t *)&program[ip]);
            ip += 4;
            printf("gp+%d, %s", global_offset, SIRVMRegisterToString(reg));
            break;
        }

        case SIRVMOpCode_MovIR8: {
            SIRVMRegister reg = (SIRVMRegister)(program[ip++] & 0x0f);
            uint8_t imm = *((uint8_t *)&program[ip]);
            ip += 1;
            printf("%zu, %s", (uint64_t)imm, SIRVMRegisterToString(reg));
            break;
        }

        case SIRVMOpCode_MovIR16: {
            SIRVMRegister reg = (SIRVMRegister)(program[ip++] & 0x0f);
            uint16_t imm = *((uint16_t *)&program[ip]);
            ip += 2;
            printf("%zu, %s", (uint64_t)imm, SIRVMRegisterToString(reg));
            break;
        }

        case SIRVMOpCode_MovIR32: {
            SIRVMRegister reg = (SIRVMRegister)(program[ip++] & 0x0f);
            uint32_t imm = *((uint32_t *)&program[ip]);
            ip += 4;
            printf("%zu, %s", (uint64_t)imm, SIRVMRegisterToString(reg));
            break;
        }

        case SIRVMOpCode_MovIR64: {
            SIRVMRegister reg = (SIRVMRegister)(program[ip++] & 0x0f);
            uint64_t imm = *((uint64_t *)&program[ip]);
            ip += 8;
            printf("%zu, %s", (uint64_t)imm, SIRVMRegisterToString(reg));
            break;
        }

        case SIRVMOpCode_MovRR8:
        case SIRVMOpCode_MovRR16:
        case SIRVMOpCode_MovRR32:
        case SIRVMOpCode_MovRR64: {
            uint8_t ext = program[ip++];
            SIRVMRegister reg_a = (SIRVMRegister)((ext >> 4) & 0x0f);
            SIRVMRegister reg_b = (SIRVMRegister)(ext & 0x0f);

            printf(
                "%s, %s",
                SIRVMRegisterToString(reg_a),
                SIRVMRegisterToString(reg_b));
            break;
        }

        case SIRVMOpCode_MovRA8:
        case SIRVMOpCode_MovRA16:
        case SIRVMOpCode_MovRA32:
        case SIRVMOpCode_MovRA64: {
            uint8_t ext = program[ip++];
            SIRVMRegister reg_a = (SIRVMRegister)((ext >> 4) & 0x0f);
            SIRVMRegister reg_b = (SIRVMRegister)(ext & 0x0f);

            printf(
                "%s, [%s]",
                SIRVMRegisterToString(reg_a),
                SIRVMRegisterToString(reg_b));
            break;
        }

        case SIRVMOpCode_MovAR8:
        case SIRVMOpCode_MovAR16:
        case SIRVMOpCode_MovAR32:
        case SIRVMOpCode_MovAR64: {
            uint8_t ext = program[ip++];
            SIRVMRegister reg_a = (SIRVMRegister)((ext >> 4) & 0x0f);
            SIRVMRegister reg_b = (SIRVMRegister)(ext & 0x0f);

            printf(
                "[%s], %s",
                SIRVMRegisterToString(reg_a),
                SIRVMRegisterToString(reg_b));
            break;
        }

        case SIRVMOpCode_JumpI32: {
            uint32_t dest = *((uint32_t *)&program[ip]);
            ip += 4;
            printf("%u", dest);
            break;
        }

        case SIRVMOpCode_CondJumpR8I32: {
            SIRVMRegister reg = (SIRVMRegister)(program[ip++] & 0x0f);
            uint32_t dest = *((uint32_t *)&program[ip]);
            ip += 4;
            printf("%s, %u", SIRVMRegisterToString(reg), dest);
            break;
        }

        case SIRVMOpCode_IAdd8:
        case SIRVMOpCode_IAdd16:
        case SIRVMOpCode_IAdd32:
        case SIRVMOpCode_IAdd64:
        case SIRVMOpCode_ISub8:
        case SIRVMOpCode_ISub16:
        case SIRVMOpCode_ISub32:
        case SIRVMOpCode_ISub64:
        case SIRVMOpCode_IMul8:
        case SIRVMOpCode_IMul16:
        case SIRVMOpCode_IMul32:
        case SIRVMOpCode_IMul64:
        case SIRVMOpCode_SDiv8:
        case SIRVMOpCode_SDiv16:
        case SIRVMOpCode_SDiv32:
        case SIRVMOpCode_SDiv64:
        case SIRVMOpCode_UDiv8:
        case SIRVMOpCode_UDiv16:
        case SIRVMOpCode_UDiv32:
        case SIRVMOpCode_UDiv64:
        case SIRVMOpCode_SRem8:
        case SIRVMOpCode_SRem16:
        case SIRVMOpCode_SRem32:
        case SIRVMOpCode_SRem64:
        case SIRVMOpCode_URem8:
        case SIRVMOpCode_URem16:
        case SIRVMOpCode_URem32:
        case SIRVMOpCode_URem64:

        case SIRVMOpCode_BitAnd8:
        case SIRVMOpCode_BitAnd16:
        case SIRVMOpCode_BitAnd32:
        case SIRVMOpCode_BitAnd64:
        case SIRVMOpCode_BitOr8:
        case SIRVMOpCode_BitOr16:
        case SIRVMOpCode_BitOr32:
        case SIRVMOpCode_BitOr64:
        case SIRVMOpCode_BitXor8:
        case SIRVMOpCode_BitXor16:
        case SIRVMOpCode_BitXor32:
        case SIRVMOpCode_BitXor64:

        case SIRVMOpCode_AShr8:
        case SIRVMOpCode_AShr16:
        case SIRVMOpCode_AShr32:
        case SIRVMOpCode_AShr64:
        case SIRVMOpCode_LShr8:
        case SIRVMOpCode_LShr16:
        case SIRVMOpCode_LShr32:
        case SIRVMOpCode_LShr64:
        case SIRVMOpCode_Shl8:
        case SIRVMOpCode_Shl16:
        case SIRVMOpCode_Shl32:
        case SIRVMOpCode_Shl64:

        case SIRVMOpCode_FAdd32:
        case SIRVMOpCode_FAdd64:
        case SIRVMOpCode_FSub32:
        case SIRVMOpCode_FSub64:
        case SIRVMOpCode_FMul32:
        case SIRVMOpCode_FMul64:
        case SIRVMOpCode_FDiv32:
        case SIRVMOpCode_FDiv64:
        case SIRVMOpCode_FRem32:
        case SIRVMOpCode_FRem64: {
            uint8_t ext = program[ip++];
            SIRVMRegister reg_a = (SIRVMRegister)((ext >> 4) & 0x0f);
            SIRVMRegister reg_b = (SIRVMRegister)(ext & 0x0f);
            printf(
                "%s, %s",
                SIRVMRegisterToString(reg_a),
                SIRVMRegisterToString(reg_b));
            break;
        }
        }

        puts("");
    }
}
