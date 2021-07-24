#include "ace_obj.hpp"
#include "x86_64/encoder.h"
#include <Tracy.hpp>

namespace ace {

struct X86_64AsmBuilder;

enum RegisterIndex : uint8_t {
    RegisterIndex_None = 0,

    RegisterIndex_RAX,
    RegisterIndex_RBX,
    RegisterIndex_RCX,
    RegisterIndex_RDX,
    RegisterIndex_RSI,
    RegisterIndex_RDI,
    RegisterIndex_RBP,
    RegisterIndex_RSP,
    RegisterIndex_R8,
    RegisterIndex_R9,
    RegisterIndex_R10,
    RegisterIndex_R11,
    RegisterIndex_R12,
    RegisterIndex_R13,
    RegisterIndex_R14,
    RegisterIndex_R15,

    RegisterIndex_XMM0,
    RegisterIndex_XMM1,
    RegisterIndex_XMM2,
    RegisterIndex_XMM3,
    RegisterIndex_XMM4,
    RegisterIndex_XMM5,
    RegisterIndex_XMM6,
    RegisterIndex_XMM7,
    RegisterIndex_XMM8,
    RegisterIndex_XMM9,
    RegisterIndex_XMM10,
    RegisterIndex_XMM11,
    RegisterIndex_XMM12,
    RegisterIndex_XMM13,
    RegisterIndex_XMM14,
    RegisterIndex_XMM15,

    RegisterIndex_COUNT,
};

static int64_t REGISTERS[] = {
    0,

    FE_AX,    FE_BX,    FE_CX,    FE_DX,    FE_SI,    FE_DI,
    FE_BP,    FE_SP,    FE_R8,    FE_R9,    FE_R10,   FE_R11,
    FE_R12,   FE_R13,   FE_R14,   FE_R15,

    FE_XMM0,  FE_XMM1,  FE_XMM2,  FE_XMM3,  FE_XMM4,  FE_XMM5,
    FE_XMM6,  FE_XMM7,  FE_XMM8,  FE_XMM9,  FE_XMM10, FE_XMM11,
    FE_XMM12, FE_XMM13, FE_XMM14, FE_XMM15,
};

struct MetaFunction;

enum MetaValueKind : uint8_t {
    MetaValueKind_Unknown = 0,
    MetaValueKind_Function,
    MetaValueKind_Block,
    MetaValueKind_IRegister,
    MetaValueKind_FRegister,
    MetaValueKind_IRegisterMemory,
    MetaValueKind_ImmInt,
    MetaValueKind_Global,
};

struct MetaValue {
    union {
        struct {
            uint64_t offset;
        } block;
        MetaFunction *func;
        struct {
            uint64_t u64;
            uint8_t bytes;
        } imm_int;
        struct {
            RegisterIndex index;
            uint8_t bytes;
        } reg;
        struct {
            RegisterIndex base;
            RegisterIndex index;
            int32_t scale;
            int32_t offset;
        } regmem;
        struct {
            size_t offset;
            SectionType section_type;
        } global;
    };
    MetaValueKind kind;
};

struct FuncJumpPatch {
    int64_t instruction;
    size_t instruction_offset;
    InstRef destination_block;
};

struct MetaFunction {
    uint32_t stack_size;
    Array<FuncJumpPatch> jump_patches;
    bool registers_used[RegisterIndex_COUNT];
    int32_t saved_register_stack_offset[RegisterIndex_COUNT];
    Slice<RegisterIndex> caller_saved_registers;
    Slice<RegisterIndex> callee_saved_registers;
    Array<RegisterIndex> temp_int_register_stack;
    SymbolRef symbol_ref{};
    uint32_t temp_stack_space;

    MetaValue push_temporary_int_value(size_t size)
    {
        MetaValue value = {};

        switch (size) {
        case 1:
        case 2:
        case 4:
        case 8: {
            if (this->temp_int_register_stack.len > 0 && size) {
                RegisterIndex reg_index = *this->temp_int_register_stack.last();
                this->temp_int_register_stack.pop();

                value.kind = MetaValueKind_IRegister;
                value.reg.index = reg_index;
                value.reg.bytes = size;
            } else {
                ACE_ASSERT(!"TODO: use stack space");
            }
            break;
        }
        default: {
            ACE_ASSERT(!"TODO: use stack space");
            break;
        }
        }

        return value;
    }

    void pop_temporary_int_value(const MetaValue &value)
    {
        switch (value.kind) {
        case MetaValueKind_IRegister: {
            this->temp_int_register_stack.push_back(value.reg.index);
            break;
        }
        case MetaValueKind_IRegisterMemory: {
            this->temp_int_register_stack.push_back(value.regmem.base);
            break;
        }
        default: break;
        }
    }
};

struct X86_64AsmBuilder : AsmBuilder {
    Module *module;
    ObjectBuilder *obj_builder;
    Array<MetaValue> meta_insts;

    size_t get_code_offset();
    size_t encode_raw(const Slice<uint8_t> &bytes);
    size_t encode(
        uint64_t mnem, FeOp op0 = 0, FeOp op1 = 0, FeOp op2 = 0, FeOp op3 = 0);
    size_t encode_at(
        size_t offset,
        uint64_t mnem,
        FeOp op0 = 0,
        FeOp op1 = 0,
        FeOp op2 = 0,
        FeOp op3 = 0);

    void encode_lea(const MetaValue &source, const MetaValue &dest);
    void encode_move(
        size_t byte_size, const MetaValue &source, const MetaValue &dest);

    size_t encode_direct_call(InstRef func_ref);
    void encode_function_ending(InstRef func_ref);

    MetaValue generate_global(uint32_t flags, Slice<uint8_t> data);
    void generate_function(InstRef global_ref);
    void generate_inst(InstRef func_ref, InstRef inst_ref);

    void move_inst_rvalue(InstRef inst_ref, const MetaValue &dest_value);

    virtual void generate() override;
    virtual void destroy() override;
};

AsmBuilder *create_x86_64_builder(Module *module, ObjectBuilder *obj_builder)
{
    ZoneScoped;

    auto asm_builder = module->arena->alloc_init<X86_64AsmBuilder>();
    asm_builder->module = module;
    asm_builder->obj_builder = obj_builder;

    asm_builder->meta_insts =
        Array<MetaValue>::create(MallocAllocator::get_instance());
    asm_builder->meta_insts.resize(module->insts.len);

    for (size_t i = 0; i < asm_builder->meta_insts.len; ++i) {
        asm_builder->meta_insts[i] = {};
    }

    return asm_builder;
}

ACE_INLINE
MetaValue create_int_register_value(uint8_t bytes, RegisterIndex index)
{
    ZoneScoped;

    MetaValue value = {};
    value.kind = MetaValueKind_IRegister;
    value.reg.bytes = bytes;
    value.reg.index = index;
    return value;
}

ACE_INLINE
MetaValue create_float_register_value(uint8_t bytes, RegisterIndex index)
{
    ZoneScoped;

    MetaValue value = {};
    value.kind = MetaValueKind_FRegister;
    value.reg.bytes = bytes;
    value.reg.index = index;
    return value;
}

ACE_INLINE
MetaValue create_int_register_memory_value(
    RegisterIndex base, int32_t scale, RegisterIndex index, int32_t offset)
{
    ZoneScoped;

    MetaValue value = {};
    value.kind = MetaValueKind_IRegisterMemory;
    value.regmem.base = base;
    value.regmem.scale = scale;
    value.regmem.index = index;
    value.regmem.offset = offset;
    return value;
}

ACE_INLINE
MetaValue create_stack_value(int32_t offset)
{
    ZoneScoped;

    MetaValue value = {};
    value.kind = MetaValueKind_IRegisterMemory;
    value.regmem.base = RegisterIndex_RBP;
    value.regmem.offset = offset;
    return value;
}

ACE_INLINE
MetaValue create_imm_int_value(uint8_t bytes, uint64_t int_value)
{
    ZoneScoped;

    MetaValue value = {};
    value.kind = MetaValueKind_ImmInt;
    switch (bytes) {
    case 1: value.imm_int.u64 = (int8_t)int_value; break;
    case 2: value.imm_int.u64 = (int16_t)int_value; break;
    case 4: value.imm_int.u64 = (int32_t)int_value; break;
    case 8: value.imm_int.u64 = int_value; break;
    }
    value.imm_int.bytes = bytes;
    return value;
}

ACE_INLINE
MetaValue create_global_value(SectionType section_type, size_t offset)
{
    ZoneScoped;

    MetaValue value = {};
    value.kind = MetaValueKind_Global;
    value.global.offset = offset;
    value.global.section_type = section_type;
    return value;
}

ACE_INLINE
size_t X86_64AsmBuilder::get_code_offset()
{
    return this->obj_builder->get_section_size(SectionType_Text);
}

ACE_INLINE
size_t X86_64AsmBuilder::encode_raw(const Slice<uint8_t> &bytes)
{
    ZoneScoped;

    this->obj_builder->add_to_section(SectionType_Text, bytes);
    return bytes.len;
}

ACE_INLINE
size_t
X86_64AsmBuilder::encode(uint64_t mnem, FeOp op0, FeOp op1, FeOp op2, FeOp op3)
{
    ZoneScoped;

    uint8_t temp[15];
    uint8_t *ptr = &temp[0];
    int failed = fe_enc64(&ptr, mnem, op0, op1, op2, op3);
    ACE_ASSERT(!failed);

    size_t inst_len = (size_t)(ptr - temp);
    this->obj_builder->add_to_section(SectionType_Text, {&temp[0], inst_len});
    return inst_len;
}

ACE_INLINE
size_t X86_64AsmBuilder::encode_at(
    size_t offset, uint64_t mnem, FeOp op0, FeOp op1, FeOp op2, FeOp op3)
{
    ZoneScoped;

    uint8_t temp[15];
    uint8_t *ptr = &temp[0];
    int failed = fe_enc64(&ptr, mnem, op0, op1, op2, op3);
    ACE_ASSERT(!failed);

    size_t inst_len = (size_t)(ptr - temp);
    this->obj_builder->set_section_data(
        SectionType_Text, offset, {&temp[0], inst_len});
    return inst_len;
}

ACE_INLINE
size_t X86_64AsmBuilder::encode_direct_call(InstRef func_ref)
{
    ZoneScoped;

    size_t inst_len = this->encode_raw({0xe8, 0x00, 0x00, 0x00, 0x00});

    size_t curr_offset = this->get_code_offset();

    MetaValue *meta_inst = &this->meta_insts[func_ref.id];

    obj_builder->add_procedure_relocation(
        meta_inst->func->symbol_ref, curr_offset - 4, 4);

    return inst_len;
}

void X86_64AsmBuilder::encode_function_ending(InstRef func_ref)
{
    ZoneScoped;

    MetaFunction *meta_func = this->meta_insts[func_ref.id].func;

    // Restore callee saved registers
    for (RegisterIndex reg_index : meta_func->callee_saved_registers) {
        if (meta_func->registers_used[reg_index]) {
            this->encode(
                FE_MOV64rm,
                REGISTERS[reg_index],
                FE_MEM(
                    FE_BP,
                    0,
                    0,
                    meta_func->saved_register_stack_offset[reg_index]));
        }
    }

    // End stack frame
    this->encode(FE_LEAVE, FE_BP);
    this->encode(FE_RET);
}

void X86_64AsmBuilder::encode_lea(
    const MetaValue &source, const MetaValue &dest)
{
    ZoneScoped;

    switch (dest.kind) {
    case MetaValueKind_IRegister: {

        switch (source.kind) {
        case MetaValueKind_IRegisterMemory: {
            int64_t source_mem = FE_MEM(
                REGISTERS[source.regmem.base],
                source.regmem.scale,
                REGISTERS[source.regmem.index],
                source.regmem.offset);
            this->encode(FE_LEA64rm, REGISTERS[dest.reg.index], source_mem);
            break;
        }
        case MetaValueKind_Global: {
            int64_t source_mem = FE_MEM(FE_IP, 0, 0, 0);

            int64_t inst_len =
                this->encode(FE_LEA64rm, REGISTERS[dest.reg.index], source_mem);
            ACE_ASSERT(inst_len > 4);

            int64_t relocation_offset = this->get_code_offset() - 4;

            this->obj_builder->add_data_relocation(
                source.global.section_type,
                source.global.offset,
                relocation_offset,
                4);
            break;
        }
        case MetaValueKind_IRegister: {
            int64_t source_mem = FE_MEM(REGISTERS[source.reg.index], 0, 0, 0);
            this->encode(FE_LEA64rm, REGISTERS[dest.reg.index], source_mem);
            break;
        }
        default: ACE_ASSERT(0);
        }

        break;
    }

    case MetaValueKind_IRegisterMemory: {
        int64_t dest_mem = FE_MEM(
            REGISTERS[dest.regmem.base],
            dest.regmem.scale,
            REGISTERS[dest.regmem.index],
            dest.regmem.offset);

        switch (source.kind) {
        case MetaValueKind_IRegisterMemory: {
            int64_t scratch_register = REGISTERS[RegisterIndex_RAX];

            int64_t source_mem = FE_MEM(
                REGISTERS[source.regmem.base],
                source.regmem.scale,
                REGISTERS[source.regmem.index],
                source.regmem.offset);
            this->encode(FE_LEA64rm, scratch_register, source_mem);
            this->encode(FE_MOV64mr, dest_mem, scratch_register);

            break;
        }
        case MetaValueKind_Global: {
            int64_t scratch_register = REGISTERS[RegisterIndex_RAX];

            int64_t source_mem = FE_MEM(FE_IP, 0, 0, 0);

            int64_t inst_len =
                this->encode(FE_LEA64rm, scratch_register, source_mem);
            ACE_ASSERT(inst_len > 4);

            int64_t relocation_offset = this->get_code_offset() - 4;

            this->encode(FE_MOV64mr, dest_mem, scratch_register);

            this->obj_builder->add_data_relocation(
                source.global.section_type,
                source.global.offset,
                relocation_offset,
                4);

            break;
        }
        default: ACE_ASSERT(0);
        }
        break;
    }

    default: ACE_ASSERT(0);
    }
}

void X86_64AsmBuilder::encode_move(
    size_t byte_size, const MetaValue &source, const MetaValue &dest)
{
    ZoneScoped;

    switch (dest.kind) {
    case MetaValueKind_IRegister: {

        switch (source.kind) {
        case MetaValueKind_IRegister: {
            if (dest.reg.index == source.reg.index) break;

            ACE_ASSERT(dest.reg.bytes == source.reg.bytes);

            int64_t x86_inst;
            switch (byte_size) {
            case 1: x86_inst = FE_MOV8rr; break;
            case 2: x86_inst = FE_MOV16rr; break;
            case 4: x86_inst = FE_MOV32rr; break;
            case 8: x86_inst = FE_MOV64rr; break;
            default: ACE_ASSERT(0);
            }

            this->encode(
                x86_inst,
                REGISTERS[dest.reg.index],
                REGISTERS[source.reg.index]);
            break;
        }
        case MetaValueKind_ImmInt: {
            ACE_ASSERT(dest.reg.bytes == source.imm_int.bytes);

            int64_t x86_inst;
            switch (byte_size) {
            case 1: x86_inst = FE_MOV8ri; break;
            case 2: x86_inst = FE_MOV16ri; break;
            case 4: x86_inst = FE_MOV32ri; break;
            case 8: x86_inst = FE_MOV64ri; break;
            default: ACE_ASSERT(0);
            }

            this->encode(
                x86_inst, REGISTERS[dest.reg.index], source.imm_int.u64);
            break;
        }
        case MetaValueKind_FRegister: {
            ACE_ASSERT(!"unimplemented move");
            break;
        }
        case MetaValueKind_Global: {
            int64_t source_mem = FE_MEM(FE_IP, 0, 0, 0);

            int64_t x86_inst;
            switch (byte_size) {
            case 1: x86_inst = FE_MOV8rm; break;
            case 2: x86_inst = FE_MOV16rm; break;
            case 4: x86_inst = FE_MOV32rm; break;
            case 8: x86_inst = FE_MOV64rm; break;
            default: ACE_ASSERT(0);
            }

            int64_t inst_len =
                this->encode(x86_inst, REGISTERS[dest.reg.index], source_mem);
            ACE_ASSERT(inst_len > 4);

            int64_t relocation_offset = this->get_code_offset() - 4;

            this->obj_builder->add_data_relocation(
                source.global.section_type,
                source.global.offset,
                relocation_offset,
                4);

            break;
        }
        case MetaValueKind_IRegisterMemory: {
            int64_t source_mem = FE_MEM(
                REGISTERS[source.regmem.base],
                source.regmem.scale,
                REGISTERS[source.regmem.index],
                source.regmem.offset);

            int64_t x86_inst;
            switch (byte_size) {
            case 1: x86_inst = FE_MOV8rm; break;
            case 2: x86_inst = FE_MOV16rm; break;
            case 4: x86_inst = FE_MOV32rm; break;
            case 8: x86_inst = FE_MOV64rm; break;
            default: ACE_ASSERT(0);
            }

            this->encode(x86_inst, REGISTERS[dest.reg.index], source_mem);
            break;
        }
        default: ACE_ASSERT(0); break;
        }

        break;
    }
    case MetaValueKind_FRegister: {

        switch (source.kind) {
        case MetaValueKind_IRegister: {
            ACE_ASSERT(!"unimplemented move");
            break;
        }
        case MetaValueKind_IRegisterMemory: {
            int64_t source_mem = FE_MEM(
                REGISTERS[source.regmem.offset],
                source.regmem.scale,
                REGISTERS[source.regmem.index],
                source.regmem.offset);

            int64_t x86_inst;
            switch (byte_size) {
            case 4: x86_inst = FE_SSE_MOVSSrm; break;
            case 8: x86_inst = FE_SSE_MOVSDrm; break;
            default: ACE_ASSERT(0);
            }

            this->encode(x86_inst, REGISTERS[dest.reg.index], source_mem);
            break;
        }
        case MetaValueKind_Global: {
            int64_t source_mem = FE_MEM(FE_IP, 0, 0, 0);

            int64_t x86_inst;
            switch (byte_size) {
            case 4: x86_inst = FE_SSE_MOVSSrm; break;
            case 8: x86_inst = FE_SSE_MOVSDrm; break;
            default: ACE_ASSERT(0);
            }

            int64_t inst_len =
                this->encode(x86_inst, REGISTERS[dest.reg.index], source_mem);
            ACE_ASSERT(inst_len > 4);

            int64_t relocation_offset = this->get_code_offset() - 4;

            this->obj_builder->add_data_relocation(
                source.global.section_type,
                source.global.offset,
                relocation_offset,
                4);

            break;
        }
        case MetaValueKind_FRegister: {
            int64_t x86_inst;
            switch (byte_size) {
            case 4: x86_inst = FE_SSE_MOVSSrr; break;
            case 8: x86_inst = FE_SSE_MOVSDrr; break;
            default: ACE_ASSERT(0);
            }

            this->encode(
                x86_inst,
                REGISTERS[dest.reg.index],
                REGISTERS[source.reg.index]);
            break;
        }
        default: ACE_ASSERT(0); break;
        }

        break;
    }
    case MetaValueKind_IRegisterMemory: {
        int64_t dest_mem = FE_MEM(
            REGISTERS[dest.regmem.base],
            dest.regmem.scale,
            REGISTERS[dest.regmem.index],
            dest.regmem.offset);

        switch (source.kind) {
        case MetaValueKind_IRegister: {
            int64_t x86_inst;
            switch (byte_size) {
            case 1: x86_inst = FE_MOV8mr; break;
            case 2: x86_inst = FE_MOV16mr; break;
            case 4: x86_inst = FE_MOV32mr; break;
            case 8: x86_inst = FE_MOV64mr; break;
            default: ACE_ASSERT(0);
            }

            this->encode(x86_inst, dest_mem, REGISTERS[source.reg.index]);
            break;
        }
        case MetaValueKind_ImmInt: {
            if (byte_size == 8) {
                int64_t scratch_register = REGISTERS[RegisterIndex_RAX];

                int64_t x86_inst1 = FE_MOV64ri;
                int64_t x86_inst2 = FE_MOV64mr;

                this->encode(x86_inst1, scratch_register, source.imm_int.u64);
                this->encode(x86_inst2, dest_mem, scratch_register);
            } else {
                int64_t x86_inst;
                switch (byte_size) {
                case 1: x86_inst = FE_MOV8mi; break;
                case 2: x86_inst = FE_MOV16mi; break;
                case 4: x86_inst = FE_MOV32mi; break;
                default: ACE_ASSERT(0);
                }

                this->encode(x86_inst, dest_mem, source.imm_int.u64);
            }
            break;
        }
        case MetaValueKind_FRegister: {
            int64_t x86_inst;
            switch (byte_size) {
            case 4: x86_inst = FE_SSE_MOVSSmr; break;
            case 8: x86_inst = FE_SSE_MOVSDmr; break;
            default: ACE_ASSERT(0);
            }

            this->encode(x86_inst, dest_mem, REGISTERS[source.reg.index]);
            break;
        }
        case MetaValueKind_Global: {
            int64_t scratch_register = REGISTERS[RegisterIndex_RAX];

            int64_t source_mem = FE_MEM(FE_IP, 0, 0, 0);

            int64_t x86_inst1;
            int64_t x86_inst2;
            switch (byte_size) {
            case 1:
                x86_inst1 = FE_MOV8rm;
                x86_inst2 = FE_MOV8mr;
                break;
            case 2:
                x86_inst1 = FE_MOV16rm;
                x86_inst2 = FE_MOV16mr;
                break;
            case 4:
                x86_inst1 = FE_MOV32rm;
                x86_inst2 = FE_MOV32mr;
                break;
            case 8:
                x86_inst1 = FE_MOV64rm;
                x86_inst2 = FE_MOV64mr;
                break;
            default: ACE_ASSERT(0);
            }

            int64_t inst_len =
                this->encode(x86_inst1, scratch_register, source_mem);
            ACE_ASSERT(inst_len > 4);

            int64_t relocation_offset = this->get_code_offset() - 4;

            this->obj_builder->add_data_relocation(
                source.global.section_type,
                source.global.offset,
                relocation_offset,
                4);

            this->encode(x86_inst2, dest_mem, scratch_register);

            break;
        }
        case MetaValueKind_IRegisterMemory: {
            int64_t scratch_register = REGISTERS[RegisterIndex_RAX];

            int64_t source_mem = FE_MEM(
                REGISTERS[source.regmem.base],
                source.regmem.scale,
                REGISTERS[source.regmem.index],
                source.regmem.offset);

            int64_t x86_inst1;
            int64_t x86_inst2;
            switch (byte_size) {
            case 1:
                x86_inst1 = FE_MOV8rm;
                x86_inst2 = FE_MOV8mr;
                break;
            case 2:
                x86_inst1 = FE_MOV16rm;
                x86_inst2 = FE_MOV16mr;
                break;
            case 4:
                x86_inst1 = FE_MOV32rm;
                x86_inst2 = FE_MOV32mr;
                break;
            case 8:
                x86_inst1 = FE_MOV64rm;
                x86_inst2 = FE_MOV64mr;
                break;
            default: ACE_ASSERT(0);
            }

            this->encode(x86_inst1, scratch_register, source_mem);
            this->encode(x86_inst2, dest_mem, scratch_register);

            break;
        }
        default: ACE_ASSERT(0); break;
        }

        break;
    }
    case MetaValueKind_Global: {
        int64_t dest_mem = FE_MEM(FE_IP, 0, 0, 0);

        switch (source.kind) {
        case MetaValueKind_IRegister: {
            int64_t x86_inst;
            switch (byte_size) {
            case 1: x86_inst = FE_MOV8mr; break;
            case 2: x86_inst = FE_MOV16mr; break;
            case 4: x86_inst = FE_MOV32mr; break;
            case 8: x86_inst = FE_MOV64mr; break;
            default: ACE_ASSERT(0);
            }

            int64_t inst_len =
                this->encode(x86_inst, dest_mem, REGISTERS[source.reg.index]);
            ACE_ASSERT(inst_len > 4);

            int64_t relocation_offset = this->get_code_offset() - 4;

            this->obj_builder->add_data_relocation(
                dest.global.section_type,
                dest.global.offset,
                relocation_offset,
                4);
            break;
        }
        case MetaValueKind_IRegisterMemory: {
            int64_t scratch_register = REGISTERS[RegisterIndex_RAX];

            int64_t source_mem = FE_MEM(
                REGISTERS[source.regmem.base],
                source.regmem.scale,
                REGISTERS[source.regmem.index],
                source.regmem.offset);

            int64_t x86_inst1;
            int64_t x86_inst2;
            switch (byte_size) {
            case 1:
                x86_inst1 = FE_MOV8rm;
                x86_inst2 = FE_MOV8mr;
                break;
            case 2:
                x86_inst1 = FE_MOV16rm;
                x86_inst2 = FE_MOV16mr;
                break;
            case 4:
                x86_inst1 = FE_MOV32rm;
                x86_inst2 = FE_MOV32mr;
                break;
            case 8:
                x86_inst1 = FE_MOV64rm;
                x86_inst2 = FE_MOV64mr;
                break;
            default: ACE_ASSERT(0);
            }

            this->encode(x86_inst1, scratch_register, source_mem);
            size_t inst_len =
                this->encode(x86_inst2, dest_mem, scratch_register);
            ACE_ASSERT(inst_len > 4);

            int64_t relocation_offset = this->get_code_offset() - 4;

            this->obj_builder->add_data_relocation(
                dest.global.section_type,
                dest.global.offset,
                relocation_offset,
                4);

            break;
        }
        case MetaValueKind_ImmInt: {
            if (byte_size == 8) {
                int64_t scratch_register = REGISTERS[RegisterIndex_RAX];

                int64_t x86_inst1 = FE_MOV64ri;
                int64_t x86_inst2 = FE_MOV64mr;

                this->encode(x86_inst1, scratch_register, source.imm_int.u64);

                size_t inst_len =
                    this->encode(x86_inst2, dest_mem, scratch_register);
                ACE_ASSERT(inst_len > 4);

                int64_t relocation_offset = this->get_code_offset() - 4;

                this->obj_builder->add_data_relocation(
                    dest.global.section_type,
                    dest.global.offset,
                    relocation_offset,
                    4);
            } else {
                int64_t x86_inst;
                switch (byte_size) {
                case 1: x86_inst = FE_MOV8mi; break;
                case 2: x86_inst = FE_MOV16mi; break;
                case 4: x86_inst = FE_MOV32mi; break;
                default: ACE_ASSERT(0);
                }

                size_t inst_len =
                    this->encode(x86_inst, dest_mem, source.imm_int.u64);
                ACE_ASSERT(inst_len > 4);

                int64_t relocation_offset =
                    this->get_code_offset() - 4 - byte_size;

                this->obj_builder->add_data_relocation(
                    dest.global.section_type,
                    dest.global.offset,
                    relocation_offset,
                    4);
            }

            break;
        }
        case MetaValueKind_FRegister: {
            ACE_ASSERT(!"unimplemented move");
            break;
        }
        default: ACE_ASSERT(0); break;
        }

        break;
    }
    default: ACE_ASSERT(0); break;
    }
}

void X86_64AsmBuilder::move_inst_rvalue(
    InstRef inst_ref, const MetaValue &dest_value)
{
    Inst inst = inst_ref.get(this->module);
    switch (inst.kind) {
    case InstKind_StackSlot: {
        this->encode_lea(this->meta_insts[inst_ref.id], dest_value);
        break;
    }
    case InstKind_Global: {
        this->encode_lea(this->meta_insts[inst_ref.id], dest_value);
        break;
    }
    case InstKind_PtrCast: {
        this->move_inst_rvalue(inst.ptr_cast.inst_ref, dest_value);
        break;
    }
    default: {
        this->encode_move(
            inst.type->size_of(this->module),
            this->meta_insts[inst_ref.id],
            dest_value);
        break;
    }
    }
}

void X86_64AsmBuilder::generate_inst(InstRef func_ref, InstRef inst_ref)
{
    ZoneScoped;

    MetaFunction *meta_func = this->meta_insts[func_ref.id].func;

    Inst inst = inst_ref.get(this->module);

    switch (inst.kind) {
    case InstKind_Unknown: ACE_ASSERT(0); break;
    case InstKind_Function: ACE_ASSERT(0); break;
    case InstKind_Block: ACE_ASSERT(0); break;
    case InstKind_FunctionParameter: ACE_ASSERT(0); break;
    case InstKind_Global: ACE_ASSERT(0); break;
    case InstKind_StackSlot: ACE_ASSERT(0); break;

    case InstKind_ImmediateInt: {
        this->meta_insts[inst_ref.id] =
            create_imm_int_value(inst.type->int_.bits >> 3, inst.imm_int.u64);

        break;
    }

    case InstKind_ImmediateFloat: {
        uint8_t data[8];
        size_t byte_size = inst.type->float_.bits >> 3;

        switch (byte_size) {
        case 4: *((float *)data) = (float)inst.imm_float.f64; break;
        case 8: *((double *)data) = (double)inst.imm_float.f64; break;
        }

        this->meta_insts[inst_ref.id] = generate_global(
            GlobalFlags_Initialized | GlobalFlags_ReadOnly,
            {&data[0], byte_size});

        break;
    }

    case InstKind_ImmediateBool: {
        this->meta_insts[inst_ref.id] =
            create_imm_int_value(1, inst.imm_bool.value ? 1 : 0);
        break;
    }

    case InstKind_PtrCast: {
        this->meta_insts[inst_ref.id] =
            this->meta_insts[inst.ptr_cast.inst_ref.id];
        break;
    }

    case InstKind_IntCast: {
        ACE_ASSERT(!"unimplemented int cast");
        break;
    }

    case InstKind_Binop: {
        MetaValue dest_value = this->meta_insts[inst_ref.id];

        size_t size = inst.type->size_of(this->module);
        size_t operand_size =
            inst.binop.left_ref.get(this->module).type->size_of(this->module);

        switch (inst.binop.op) {
        case BinaryOperation_Unknown:
        case BinaryOperation_MAX: ACE_ASSERT(0); break;

        case BinaryOperation_IAdd: {
            MetaValue ax_value =
                create_int_register_value(operand_size, RegisterIndex_RAX);
            MetaValue dx_value =
                create_int_register_value(operand_size, RegisterIndex_RDX);

            int64_t x86_inst = 0;
            switch (operand_size) {
            case 1: x86_inst = FE_ADD8rr; break;
            case 2: x86_inst = FE_ADD16rr; break;
            case 4: x86_inst = FE_ADD32rr; break;
            case 8: x86_inst = FE_ADD64rr; break;
            default: ACE_ASSERT(0); break;
            }

            this->move_inst_rvalue(inst.binop.left_ref, ax_value);
            this->move_inst_rvalue(inst.binop.right_ref, dx_value);

            this->encode(x86_inst, FE_AX, FE_DX);
            this->encode_move(size, ax_value, dest_value);

            break;
        }

        case BinaryOperation_ISub: {
            MetaValue ax_value =
                create_int_register_value(operand_size, RegisterIndex_RAX);
            MetaValue dx_value =
                create_int_register_value(operand_size, RegisterIndex_RDX);

            int64_t x86_inst = 0;
            switch (operand_size) {
            case 1: x86_inst = FE_SUB8rr; break;
            case 2: x86_inst = FE_SUB16rr; break;
            case 4: x86_inst = FE_SUB32rr; break;
            case 8: x86_inst = FE_SUB64rr; break;
            default: ACE_ASSERT(0); break;
            }

            this->move_inst_rvalue(inst.binop.left_ref, ax_value);
            this->move_inst_rvalue(inst.binop.right_ref, dx_value);

            this->encode(x86_inst, FE_AX, FE_DX);
            this->encode_move(size, ax_value, dest_value);

            break;
        }

        case BinaryOperation_IMul: {
            MetaValue ax_value =
                create_int_register_value(operand_size, RegisterIndex_RAX);
            MetaValue dx_value =
                create_int_register_value(operand_size, RegisterIndex_RDX);

            int64_t x86_inst = 0;
            switch (operand_size) {
            case 1: x86_inst = FE_IMUL8r; break;
            case 2: x86_inst = FE_IMUL16rr; break;
            case 4: x86_inst = FE_IMUL32rr; break;
            case 8: x86_inst = FE_IMUL64rr; break;
            default: ACE_ASSERT(0); break;
            }

            this->move_inst_rvalue(inst.binop.left_ref, ax_value);
            this->move_inst_rvalue(inst.binop.right_ref, dx_value);

            if (size == 1) {
                this->encode(x86_inst, FE_DX);
                this->encode_move(size, dx_value, dest_value);
            } else {
                this->encode(x86_inst, FE_AX, FE_DX);
                this->encode_move(size, ax_value, dest_value);
            }

            break;
        }

        case BinaryOperation_SDiv: {
            MetaValue ax_value =
                create_int_register_value(operand_size, RegisterIndex_RAX);
            MetaValue cx_value =
                create_int_register_value(operand_size, RegisterIndex_RCX);

            switch (operand_size) {
            case 1: {
                this->move_inst_rvalue(inst.binop.left_ref, ax_value);
                this->encode(FE_MOVSXr32r8, FE_AX, FE_AX);
                break;
            }
            case 2: {
                this->move_inst_rvalue(inst.binop.left_ref, ax_value);
                this->encode(FE_MOVSXr32r16, FE_AX, FE_AX);
                break;
            }
            case 4:
            case 8: {
                this->move_inst_rvalue(inst.binop.left_ref, ax_value);
                break;
            }
            default: ACE_ASSERT(0); break;
            }

            int64_t sep_inst = 0;
            switch (operand_size) {
            case 1:
            case 2:
            case 4: sep_inst = FE_C_SEP32; break;
            case 8: sep_inst = FE_C_SEP64; break;
            default: ACE_ASSERT(0); break;
            }
            this->encode(sep_inst, FE_AX);

            int64_t div_inst = 0;
            switch (operand_size) {
            case 1:
            case 2:
            case 4: div_inst = FE_IDIV32r; break;
            case 8: div_inst = FE_IDIV64r; break;
            default: ACE_ASSERT(0); break;
            }

            switch (operand_size) {
            case 1: {
                this->move_inst_rvalue(inst.binop.right_ref, cx_value);
                this->encode(FE_MOVSXr32r8, FE_CX, FE_CX);
                break;
            }
            case 2: {
                this->move_inst_rvalue(inst.binop.right_ref, cx_value);
                this->encode(FE_MOVSXr32r16, FE_CX, FE_CX);
                break;
            }
            case 4:
            case 8: {
                this->move_inst_rvalue(inst.binop.right_ref, cx_value);
                break;
            }
            default: ACE_ASSERT(0); break;
            }

            this->encode(div_inst, FE_CX);

            this->encode_move(size, ax_value, dest_value);
            break;
        }

        case BinaryOperation_UDiv: {
            MetaValue ax_value =
                create_int_register_value(operand_size, RegisterIndex_RAX);
            MetaValue cx_value =
                create_int_register_value(operand_size, RegisterIndex_RCX);

            switch (operand_size) {
            case 1: {
                this->move_inst_rvalue(inst.binop.left_ref, ax_value);
                this->encode(FE_MOVZXr32r8, FE_AX, FE_AX);
                break;
            }
            case 2: {
                this->move_inst_rvalue(inst.binop.left_ref, ax_value);
                this->encode(FE_MOVZXr32r16, FE_AX, FE_AX);
                break;
            }
            case 4:
            case 8: {
                this->move_inst_rvalue(inst.binop.left_ref, ax_value);
                break;
            }
            default: ACE_ASSERT(0); break;
            }

            switch (operand_size) {
            case 1:
            case 2: {
                this->encode(FE_C_SEP32, FE_AX);
                break;
            }
            default: break;
            }

            int64_t div_inst = 0;
            switch (operand_size) {
            case 1: div_inst = FE_IDIV32r; break;
            case 2: div_inst = FE_IDIV32r; break;
            case 4: div_inst = FE_DIV32r; break;
            case 8: div_inst = FE_DIV64r; break;
            default: ACE_ASSERT(0); break;
            }

            switch (operand_size) {
            case 1: {
                this->move_inst_rvalue(inst.binop.right_ref, cx_value);
                this->encode(FE_MOVZXr32r8, FE_CX, FE_CX);
                break;
            }
            case 2: {
                this->move_inst_rvalue(inst.binop.right_ref, cx_value);
                this->encode(FE_MOVZXr32r16, FE_CX, FE_CX);
                break;
            }
            case 4:
            case 8: {
                this->encode(FE_XOR32rr, FE_DX, FE_DX);
                this->move_inst_rvalue(inst.binop.right_ref, cx_value);
                break;
            }
            default: ACE_ASSERT(0); break;
            }

            this->encode(div_inst, FE_CX);

            this->encode_move(size, ax_value, dest_value);
            break;
        }

        case BinaryOperation_SRem: {
            MetaValue ax_value =
                create_int_register_value(operand_size, RegisterIndex_RAX);
            MetaValue dx_value =
                create_int_register_value(operand_size, RegisterIndex_RDX);
            MetaValue cx_value =
                create_int_register_value(operand_size, RegisterIndex_RCX);

            switch (operand_size) {
            case 1: {
                this->move_inst_rvalue(inst.binop.left_ref, ax_value);
                this->encode(FE_MOVSXr32r8, FE_AX, FE_AX);
                break;
            }
            case 2: {
                this->move_inst_rvalue(inst.binop.left_ref, ax_value);
                this->encode(FE_MOVSXr32r16, FE_AX, FE_AX);
                break;
            }
            case 4:
            case 8: {
                this->move_inst_rvalue(inst.binop.left_ref, ax_value);
                break;
            }
            default: ACE_ASSERT(0); break;
            }

            int64_t sep_inst = 0;
            switch (operand_size) {
            case 1:
            case 2:
            case 4: sep_inst = FE_C_SEP32; break;
            case 8: sep_inst = FE_C_SEP64; break;
            default: ACE_ASSERT(0); break;
            }
            this->encode(sep_inst, FE_AX);

            int64_t div_inst = 0;
            switch (operand_size) {
            case 1:
            case 2:
            case 4: div_inst = FE_IDIV32r; break;
            case 8: div_inst = FE_IDIV64r; break;
            default: ACE_ASSERT(0); break;
            }

            switch (operand_size) {
            case 1: {
                this->move_inst_rvalue(inst.binop.right_ref, cx_value);
                this->encode(FE_MOVSXr32r8, FE_CX, FE_CX);
                break;
            }
            case 2: {
                this->move_inst_rvalue(inst.binop.right_ref, cx_value);
                this->encode(FE_MOVSXr32r16, FE_CX, FE_CX);
                break;
            }
            case 4:
            case 8: {
                this->move_inst_rvalue(inst.binop.right_ref, cx_value);
                break;
            }
            default: ACE_ASSERT(0); break;
            }

            this->encode(div_inst, FE_CX);

            this->encode_move(size, dx_value, dest_value);
            break;
        }

        case BinaryOperation_URem: {
            MetaValue ax_value =
                create_int_register_value(operand_size, RegisterIndex_RAX);
            MetaValue dx_value =
                create_int_register_value(operand_size, RegisterIndex_RDX);
            MetaValue cx_value =
                create_int_register_value(operand_size, RegisterIndex_RCX);

            switch (operand_size) {
            case 1: {
                this->move_inst_rvalue(inst.binop.left_ref, ax_value);
                this->encode(FE_MOVZXr32r8, FE_AX, FE_AX);
                break;
            }
            case 2: {
                this->move_inst_rvalue(inst.binop.left_ref, ax_value);
                this->encode(FE_MOVZXr32r16, FE_AX, FE_AX);
                break;
            }
            case 4:
            case 8: {
                this->move_inst_rvalue(inst.binop.left_ref, ax_value);
                break;
            }
            default: ACE_ASSERT(0); break;
            }

            switch (operand_size) {
            case 1:
            case 2: {
                this->encode(FE_C_SEP32, FE_AX);
                break;
            }
            default: break;
            }

            int64_t div_inst = 0;
            switch (operand_size) {
            case 1: div_inst = FE_IDIV32r; break;
            case 2: div_inst = FE_IDIV32r; break;
            case 4: div_inst = FE_DIV32r; break;
            case 8: div_inst = FE_DIV64r; break;
            default: ACE_ASSERT(0); break;
            }

            switch (operand_size) {
            case 1: {
                this->move_inst_rvalue(inst.binop.right_ref, cx_value);
                this->encode(FE_MOVZXr32r8, FE_CX, FE_CX);
                break;
            }
            case 2: {
                this->move_inst_rvalue(inst.binop.right_ref, cx_value);
                this->encode(FE_MOVZXr32r16, FE_CX, FE_CX);
                break;
            }
            case 4:
            case 8: {
                this->encode(FE_XOR32rr, FE_DX, FE_DX);
                this->move_inst_rvalue(inst.binop.right_ref, cx_value);
                break;
            }
            default: ACE_ASSERT(0); break;
            }

            this->encode(div_inst, FE_CX);

            this->encode_move(size, dx_value, dest_value);
            break;
        }

        case BinaryOperation_FAdd: {
            ACE_ASSERT(!"unimplemented");
            break;
        }

        case BinaryOperation_FSub: {
            ACE_ASSERT(!"unimplemented");
            break;
        }

        case BinaryOperation_FMul: {
            ACE_ASSERT(!"unimplemented");
            break;
        }

        case BinaryOperation_FDiv: {
            ACE_ASSERT(!"unimplemented");
            break;
        }

        case BinaryOperation_FRem: {
            ACE_ASSERT(!"unimplemented");
            break;
        }

        case BinaryOperation_IEQ:
        case BinaryOperation_INE:
        case BinaryOperation_UGT:
        case BinaryOperation_UGE:
        case BinaryOperation_ULT:
        case BinaryOperation_ULE:
        case BinaryOperation_SGT:
        case BinaryOperation_SGE:
        case BinaryOperation_SLT:
        case BinaryOperation_SLE: {
            MetaValue ax_value =
                create_int_register_value(operand_size, RegisterIndex_RAX);
            MetaValue dx_value =
                create_int_register_value(operand_size, RegisterIndex_RDX);

            this->move_inst_rvalue(inst.binop.left_ref, ax_value);
            this->move_inst_rvalue(inst.binop.right_ref, dx_value);

            int64_t x86_inst = 0;
            switch (operand_size) {
            case 1: x86_inst = FE_CMP8rr; break;
            case 2: x86_inst = FE_CMP16rr; break;
            case 4: x86_inst = FE_CMP32rr; break;
            case 8: x86_inst = FE_CMP64rr; break;
            default: ACE_ASSERT(0); break;
            }

            this->encode(x86_inst, FE_AX, FE_DX);

            switch (inst.binop.op) {
            case BinaryOperation_IEQ:
                this->encode(FE_SETZ8r, FE_AX); // sete
                break;
            case BinaryOperation_INE:
                this->encode(FE_SETNZ8r, FE_AX); // setne
                break;
            case BinaryOperation_UGT:
                this->encode(FE_SETA8r, FE_AX); // seta
                break;
            case BinaryOperation_UGE:
                this->encode(FE_SETNC8r, FE_AX); // setae
                break;
            case BinaryOperation_ULT:
                this->encode(FE_SETC8r, FE_AX); // setb
                break;
            case BinaryOperation_ULE:
                this->encode(FE_SETBE8r, FE_AX); // setbe
                break;
            case BinaryOperation_SGT:
                this->encode(FE_SETG8r, FE_AX); // setg
                break;
            case BinaryOperation_SGE:
                this->encode(FE_SETGE8r, FE_AX); // setge
                break;
            case BinaryOperation_SLT:
                this->encode(FE_SETL8r, FE_AX); // setl
                break;
            case BinaryOperation_SLE:
                this->encode(FE_SETLE8r, FE_AX); // setle
                break;
            default: ACE_ASSERT(0); break;
            }

            this->encode(FE_AND8ri, FE_AX, 1);
            this->encode_move(size, ax_value, dest_value);
            break;
        }

        case BinaryOperation_FEQ:
        case BinaryOperation_FNE:
        case BinaryOperation_FGT:
        case BinaryOperation_FGE:
        case BinaryOperation_FLT:
        case BinaryOperation_FLE: {
            ACE_ASSERT(!"unimplemented");
            break;
        }

        case BinaryOperation_And:
        case BinaryOperation_Or:
        case BinaryOperation_Xor: {
            MetaValue ax_value =
                create_int_register_value(operand_size, RegisterIndex_RAX);
            MetaValue dx_value =
                create_int_register_value(operand_size, RegisterIndex_RDX);

            this->move_inst_rvalue(inst.binop.left_ref, ax_value);
            this->move_inst_rvalue(inst.binop.right_ref, dx_value);

            int64_t x86_inst = 0;

            switch (inst.binop.op) {
            case BinaryOperation_And: {
                switch (operand_size) {
                case 1: x86_inst = FE_AND8rr; break;
                case 2: x86_inst = FE_AND16rr; break;
                case 4: x86_inst = FE_AND32rr; break;
                case 8: x86_inst = FE_AND64rr; break;
                default: ACE_ASSERT(0); break;
                }
                break;
            }
            case BinaryOperation_Or: {
                switch (operand_size) {
                case 1: x86_inst = FE_OR8rr; break;
                case 2: x86_inst = FE_OR16rr; break;
                case 4: x86_inst = FE_OR32rr; break;
                case 8: x86_inst = FE_OR64rr; break;
                default: ACE_ASSERT(0); break;
                }
                break;
            }
            case BinaryOperation_Xor: {
                switch (operand_size) {
                case 1: x86_inst = FE_XOR8rr; break;
                case 2: x86_inst = FE_XOR16rr; break;
                case 4: x86_inst = FE_XOR32rr; break;
                case 8: x86_inst = FE_XOR64rr; break;
                default: ACE_ASSERT(0); break;
                }
                break;
            }
            default: ACE_ASSERT(0); break;
            }

            this->encode(x86_inst, FE_AX, FE_DX);
            this->encode_move(size, ax_value, dest_value);
            break;
            break;
        }

        case BinaryOperation_Shl:
        case BinaryOperation_AShr:
        case BinaryOperation_LShr: {
            MetaValue ax_value = create_int_register_value(
                inst.binop.left_ref.get(this->module)
                    .type->size_of(this->module),
                RegisterIndex_RAX);
            MetaValue cx_value = create_int_register_value(
                inst.binop.right_ref.get(this->module)
                    .type->size_of(this->module),
                RegisterIndex_RCX);

            this->move_inst_rvalue(inst.binop.left_ref, ax_value);
            this->move_inst_rvalue(inst.binop.right_ref, cx_value);

            int64_t x86_inst = 0;

            switch (inst.binop.op) {
            case BinaryOperation_Shl: {
                switch (operand_size) {
                case 1: x86_inst = FE_SHL8rr; break;
                case 2: x86_inst = FE_SHL16rr; break;
                case 4: x86_inst = FE_SHL32rr; break;
                case 8: x86_inst = FE_SHL64rr; break;
                default: ACE_ASSERT(0); break;
                }
                break;
            }
            case BinaryOperation_AShr: {
                switch (operand_size) {
                case 1: x86_inst = FE_SAR8rr; break;
                case 2: x86_inst = FE_SAR16rr; break;
                case 4: x86_inst = FE_SAR32rr; break;
                case 8: x86_inst = FE_SAR64rr; break;
                default: ACE_ASSERT(0); break;
                }
                break;
            }
            case BinaryOperation_LShr: {
                switch (operand_size) {
                case 1: x86_inst = FE_SHR8rr; break;
                case 2: x86_inst = FE_SHR16rr; break;
                case 4: x86_inst = FE_SHR32rr; break;
                case 8: x86_inst = FE_SHR64rr; break;
                default: ACE_ASSERT(0); break;
                }
                break;
            }
            default: ACE_ASSERT(0); break;
            }

            this->encode(x86_inst, FE_AX, FE_CX);
            this->encode_move(size, ax_value, dest_value);
            break;
        }
        }
        break;
    }

    case InstKind_ArrayElemPtr: {
        MetaValue ptr_value = create_int_register_value(8, RegisterIndex_RAX);
        this->move_inst_rvalue(inst.array_elem_ptr.accessed_ref, ptr_value);

        size_t index_size = inst.array_elem_ptr.index_ref.get(this->module)
                                .type->size_of(this->module);

        MetaValue index_value =
            create_int_register_value(index_size, RegisterIndex_RCX);
        this->encode_move(
            index_size,
            this->meta_insts[inst.array_elem_ptr.index_ref.id],
            index_value);

        int32_t scale = inst.type->pointer.sub->size_of(this->module);

        MetaValue value_addr = create_int_register_memory_value(
            RegisterIndex_RAX, scale, RegisterIndex_RCX, 0);

        this->encode_lea(value_addr, this->meta_insts[inst_ref.id]);

        break;
    }

    case InstKind_Store: {
        size_t value_size =
            inst.store.value_ref.get(this->module).type->size_of(this->module);

        switch (value_size) {
        case 1:
        case 2:
        case 4:
        case 8: {
            MetaValue stored_value =
                create_int_register_value(value_size, RegisterIndex_RAX);
            this->move_inst_rvalue(inst.store.value_ref, stored_value);

            Inst ptr_inst = inst.store.ptr_ref.get(this->module);

            MetaValue ptr_value = {};
            switch (ptr_inst.kind) {
            case InstKind_Global:
            case InstKind_StackSlot: {
                ptr_value = this->meta_insts[inst.store.ptr_ref.id];
                break;
            }
            default: {
                ptr_value = create_int_register_value(8, RegisterIndex_RCX);
                this->move_inst_rvalue(inst.store.ptr_ref, ptr_value);
                ptr_value = create_int_register_memory_value(
                    ptr_value.reg.index, 0, RegisterIndex_None, 0);
                break;
            }
            }

            this->encode_move(value_size, stored_value, ptr_value);
            break;
        }
        default: {
            ACE_ASSERT(!"TODO: use memcpy");
            break;
        }
        }

        break;
    }

    case InstKind_Load: {
        size_t size = inst.type->size_of(this->module);
        switch (size) {
        case 1:
        case 2:
        case 4:
        case 8: {
            Inst ptr_inst = inst.load.ptr_ref.get(this->module);

            MetaValue ptr_value = {};
            switch (ptr_inst.kind) {
            case InstKind_Global:
            case InstKind_StackSlot: {
                ptr_value = this->meta_insts[inst.store.ptr_ref.id];
                break;
            }
            default: {
                ptr_value = create_int_register_value(8, RegisterIndex_RAX);
                this->move_inst_rvalue(inst.store.ptr_ref, ptr_value);
                ptr_value = create_int_register_memory_value(
                    ptr_value.reg.index, 0, RegisterIndex_None, 0);
                break;
            }
            }

            this->encode_move(size, ptr_value, this->meta_insts[inst_ref.id]);
            break;
        }
        default: {
            ACE_ASSERT(!"TODO: use memcpy");
            break;
        }
        }

        break;
    }

    case InstKind_FuncCall: {
        Function *called_func = inst.func_call.func_ref.get(this->module).func;

        // TODO: save and restore caller-saved registers

        ACE_ASSERT(
            called_func->param_types.len <= inst.func_call.parameters.len);

        switch (called_func->calling_convention) {
        case CallingConvention_SystemV: {
            static RegisterIndex sysv_int_param_regs[6] = {
                RegisterIndex_RDI,
                RegisterIndex_RSI,
                RegisterIndex_RDX,
                RegisterIndex_RCX,
                RegisterIndex_R8,
                RegisterIndex_R9,
            };

            static RegisterIndex sysv_float_param_regs[8] = {
                RegisterIndex_XMM0,
                RegisterIndex_XMM1,
                RegisterIndex_XMM2,
                RegisterIndex_XMM3,
                RegisterIndex_XMM4,
                RegisterIndex_XMM5,
                RegisterIndex_XMM6,
                RegisterIndex_XMM7,
            };

            uint32_t int_count = 0;
            uint32_t float_count = 0;

            // Write parameters to ABI locations
            for (size_t i = 0; i < inst.func_call.parameters.len; ++i) {
                InstRef param_inst_ref = inst.func_call.parameters[i];
                ACE_ASSERT(param_inst_ref.id > 0);

                Inst param_inst = param_inst_ref.get(this->module);

                if (i < called_func->param_types.len) {
                    Type *param_type = called_func->param_types[i];
                    ACE_ASSERT(param_type == param_inst.type);
                }

                MetaValue dest_param_value = {};

                switch (param_inst.type->kind) {
                case TypeKind_Pointer: {
                    dest_param_value = create_int_register_value(
                        8, sysv_int_param_regs[int_count]);
                    int_count++;
                    break;
                }
                case TypeKind_Int: {
                    dest_param_value = create_int_register_value(
                        param_inst.type->int_.bits >> 3,
                        sysv_int_param_regs[int_count]);
                    int_count++;
                    break;
                }
                case TypeKind_Float: {
                    dest_param_value = create_float_register_value(
                        param_inst.type->float_.bits >> 3,
                        sysv_float_param_regs[float_count]);
                    float_count++;
                    break;
                }
                default: {
                    ACE_ASSERT(!"unhandled parameter type");
                    break;
                }
                }

                this->move_inst_rvalue(param_inst_ref, dest_param_value);
                /* this->encode_move( */
                /*     param_inst.type->size_of(this->module), */
                /*     this->meta_insts[param_inst_ref.id], */
                /*     dest_param_value); */
            }

            if (called_func->variadic) {
                // Write the amount of vector registers to %AL
                MetaValue float_count_imm =
                    create_imm_int_value(1, float_count);
                MetaValue float_count_reg =
                    create_int_register_value(1, RegisterIndex_RAX);
                this->encode_move(1, float_count_imm, float_count_reg);
            }

            break;
        }
        }

        this->encode_direct_call(inst.func_call.func_ref);

        // Move returned values to result location

        switch (called_func->calling_convention) {
        case CallingConvention_SystemV: {
            switch (called_func->return_type->kind) {
            case TypeKind_Void: break;

            case TypeKind_Int: {
                MetaValue returned_value = create_int_register_value(
                    called_func->return_type->int_.bits >> 3,
                    RegisterIndex_RAX);
                this->encode_move(
                    returned_value.reg.bytes,
                    returned_value,
                    this->meta_insts[inst_ref.id]);
                break;
            }

            case TypeKind_Pointer: {
                MetaValue returned_value =
                    create_int_register_value(8, RegisterIndex_RAX);
                this->encode_move(
                    8, returned_value, this->meta_insts[inst_ref.id]);
                break;
            }

            case TypeKind_Float: {
                MetaValue returned_value = create_float_register_value(
                    called_func->return_type->float_.bits >> 3,
                    RegisterIndex_XMM0);
                this->encode_move(
                    returned_value.reg.bytes,
                    returned_value,
                    this->meta_insts[inst_ref.id]);
                break;
            }

            default: ACE_ASSERT(!"unhandled return type");
            }
            break;
        }
        }

        break;
    }

    case InstKind_ReturnVoid: {
        this->encode_function_ending(func_ref);
        break;
    }

    case InstKind_ReturnValue: {
        Inst returned_inst = inst.return_value.inst_ref.get(this->module);

        // TODO: ABI compliance
        MetaValue result_location = {};
        switch (returned_inst.type->kind) {
        case TypeKind_Int: {
            result_location = create_int_register_value(
                returned_inst.type->int_.bits >> 3, RegisterIndex_RAX);
            break;
        }
        case TypeKind_Pointer: {
            result_location = create_int_register_value(8, RegisterIndex_RAX);
            break;
        }
        case TypeKind_Float: {
            result_location =
                create_float_register_value(8, RegisterIndex_XMM0);
            break;
        }
        default: ACE_ASSERT(0); break;
        }

        this->encode_move(
            returned_inst.type->size_of(this->module),
            this->meta_insts[inst.return_value.inst_ref.id],
            result_location);

        this->encode_function_ending(func_ref);
        break;
    }

    case InstKind_Jump: {
        FuncJumpPatch patch = {
            .instruction = FE_JMP | FE_JMPL,
            .instruction_offset = this->get_code_offset(),
            .destination_block = inst.jump.block_ref,
        };
        meta_func->jump_patches.push_back(patch);

        this->encode(FE_JMP | FE_JMPL, -patch.instruction_offset);

        break;
    }
    case InstKind_Branch: {
        MetaValue al_value = create_int_register_value(1, RegisterIndex_RAX);

        this->encode_move(
            1, this->meta_insts[inst.branch.cond_inst_ref.id], al_value);

        this->encode(
            FE_TEST8rr,
            REGISTERS[al_value.reg.index],
            REGISTERS[al_value.reg.index]);

        FuncJumpPatch true_patch = {
            .instruction = FE_JNZ | FE_JMPL,
            .instruction_offset = this->get_code_offset(),
            .destination_block = inst.branch.true_block_ref,
        };
        meta_func->jump_patches.push_back(true_patch);
        this->encode(FE_JNZ | FE_JMPL, -true_patch.instruction_offset);

        FuncJumpPatch false_patch = {
            .instruction = FE_JMP | FE_JMPL,
            .instruction_offset = this->get_code_offset(),
            .destination_block = inst.branch.false_block_ref,
        };
        meta_func->jump_patches.push_back(false_patch);
        this->encode(FE_JMP | FE_JMPL, -false_patch.instruction_offset);

        break;
    }
    }
}

MetaValue X86_64AsmBuilder::generate_global(uint32_t flags, Slice<uint8_t> data)
{
    ZoneScoped;

    SectionType section_type = SectionType_Data;
    if (!(flags & GlobalFlags_Initialized)) {
        section_type = SectionType_BSS;
    } else {
        if (flags & GlobalFlags_ReadOnly) {
            section_type = SectionType_ROData;
        } else {
            section_type = SectionType_Data;
        }
    }

    size_t offset = this->obj_builder->get_section_size(section_type);
    this->obj_builder->add_to_section(section_type, data);

    return create_global_value(section_type, offset);
}

void X86_64AsmBuilder::generate_function(InstRef func_ref)
{
    ZoneScoped;

    Inst func_inst = func_ref.get(this->module);
    ACE_ASSERT(func_inst.kind == InstKind_Function);
    Function *func = func_inst.func;
    ACE_ASSERT(func);

    MetaFunction *meta_func = this->module->arena->alloc<MetaFunction>();
    *meta_func = {};

    meta_func->jump_patches = Array<FuncJumpPatch>::create(module->arena);
    meta_func->temp_int_register_stack =
        Array<RegisterIndex>::create(module->arena);

    switch (func->calling_convention) {
    case CallingConvention_SystemV: {
        static RegisterIndex system_v_callee_saved[] = {
            RegisterIndex_RBX,
            RegisterIndex_R12,
            RegisterIndex_R13,
            RegisterIndex_R14,
            RegisterIndex_R15,
        };
        meta_func->callee_saved_registers = {system_v_callee_saved};

        static RegisterIndex system_v_caller_saved[] = {
            RegisterIndex_RAX,
            RegisterIndex_RCX,
            RegisterIndex_RDX,
            RegisterIndex_RSI,
            RegisterIndex_RDI,
            RegisterIndex_R8,
            RegisterIndex_R9,
            RegisterIndex_R11,
        };
        meta_func->caller_saved_registers = {system_v_caller_saved};

        break;
    }
    }

    for (RegisterIndex reg_index : meta_func->caller_saved_registers) {
        meta_func->temp_int_register_stack.push_back(reg_index);
    }

    if (func->blocks.len == 0) {
        // Function without body
        meta_func->symbol_ref = obj_builder->add_symbol(
            func->name, SectionType_None, SymbolType_None, Linkage_External);
    } else {
        // Function with body
        meta_func->symbol_ref = obj_builder->add_symbol(
            func->name,
            SectionType_Text,
            SymbolType_Function,
            Linkage_External);
    }

    MetaValue *func_meta_inst = &this->meta_insts[func_ref.id];
    *func_meta_inst = {};
    func_meta_inst->kind = MetaValueKind_Function;
    func_meta_inst->func = meta_func;

    if (func->blocks.len == 0) {
        return;
    }

    for (InstRef stack_slot_ref : func->stack_slots) {
        Inst stack_slot = stack_slot_ref.get(this->module);
        ACE_ASSERT(stack_slot.type->kind == TypeKind_Pointer);
        uint32_t slot_size =
            stack_slot.type->pointer.sub->size_of(this->module);
        uint32_t slot_align =
            stack_slot.type->pointer.sub->align_of(this->module);
        meta_func->stack_size = ACE_ROUND_UP(slot_align, meta_func->stack_size);
        meta_func->stack_size += slot_size;

        this->meta_insts[stack_slot_ref.id] =
            create_stack_value(-((int32_t)meta_func->stack_size));
    }

    // Create stack space for function parameters
    for (InstRef param_inst_ref : func->param_insts) {
        Inst param_inst = param_inst_ref.get(this->module);

        uint32_t inst_size = param_inst.type->size_of(this->module);
        uint32_t inst_align = param_inst.type->align_of(this->module);

        meta_func->stack_size = ACE_ROUND_UP(inst_align, meta_func->stack_size);
        meta_func->stack_size += inst_size;

        this->meta_insts[param_inst_ref.id] =
            create_stack_value(-((int32_t)meta_func->stack_size));
    }

    // Spill all elegible insts to stack
    for (InstRef block_ref : func->blocks) {
        Inst block = block_ref.get(this->module);
        for (InstRef inst_ref : block.block.inst_refs) {
            Inst inst = inst_ref.get(this->module);

            switch (inst.kind) {
            // Invalid for this stage of generation:
            case InstKind_Unknown:
            case InstKind_Global:
            case InstKind_StackSlot:
            case InstKind_Block:
            case InstKind_Function:
            case InstKind_FunctionParameter: ACE_ASSERT(0); break;

            // Contain no data for spilling:
            case InstKind_Store:
            case InstKind_Jump:
            case InstKind_Branch:
            case InstKind_ReturnVoid:
            case InstKind_ReturnValue: break;

            // Data already stored somewhere else:
            case InstKind_ImmediateInt:
            case InstKind_ImmediateFloat:
            case InstKind_ImmediateBool:
            case InstKind_PtrCast: break;

            // Create stack space for these kinds:
            case InstKind_IntCast:
            case InstKind_Binop:
            case InstKind_ArrayElemPtr:
            case InstKind_Load:
            case InstKind_FuncCall: {
                uint32_t inst_size = inst.type->size_of(this->module);
                uint32_t inst_align = inst.type->align_of(this->module);

                meta_func->stack_size =
                    ACE_ROUND_UP(inst_align, meta_func->stack_size);
                meta_func->stack_size += inst_size;

                this->meta_insts[inst_ref.id] =
                    create_stack_value(-((int32_t)meta_func->stack_size));
                break;
            }
            }
        }
    }

    // TODO: Reserve saved register space

    for (uint32_t i = 0; i < RegisterIndex_COUNT; ++i) {
        if (meta_func->registers_used[i]) {
            meta_func->stack_size = ACE_ROUND_UP(8, meta_func->stack_size);
            meta_func->stack_size += 8;

            meta_func->saved_register_stack_offset[i] =
                -((int32_t)meta_func->stack_size);
        }
    }

    size_t function_start_offset =
        this->obj_builder->get_section_size(SectionType_Text);

    // Begin stack frame
    meta_func->stack_size = ACE_ROUND_UP(0x10, meta_func->stack_size);
    this->encode(FE_PUSHr, FE_BP);
    this->encode(FE_MOV64rr, FE_BP, FE_SP);
    this->encode(FE_SUB64ri, FE_SP, meta_func->stack_size);

    // Move parameters to stack
    switch (func->calling_convention) {
    case CallingConvention_SystemV: {
        static RegisterIndex sysv_int_param_regs[6] = {
            RegisterIndex_RDI,
            RegisterIndex_RSI,
            RegisterIndex_RDX,
            RegisterIndex_RCX,
            RegisterIndex_R8,
            RegisterIndex_R9,
        };

        for (uint32_t i = 0; i < func->param_types.len; ++i) {
            Type *param_type = func->param_types[i];
            InstRef param_inst_ref = func->param_insts[i];
            MetaValue param_meta_inst = this->meta_insts[param_inst_ref.id];

            switch (param_type->kind) {
            case TypeKind_Int: {
                MetaValue param_register_value = create_int_register_value(
                    param_type->int_.bits >> 3, sysv_int_param_regs[i]);

                this->encode_move(
                    param_register_value.reg.bytes,
                    param_register_value,
                    param_meta_inst);
                break;
            }
            case TypeKind_Pointer: {
                MetaValue param_register_value =
                    create_int_register_value(8, sysv_int_param_regs[i]);

                this->encode_move(
                    param_register_value.reg.bytes,
                    param_register_value,
                    param_meta_inst);
                break;
            }
            default: {
                ACE_ASSERT(!"unhandled parameter type");
                break;
            }
            }
        }

        break;
    }
    }

    // Save callee saved registers
    for (RegisterIndex reg_index : meta_func->callee_saved_registers) {
        if (meta_func->registers_used[reg_index]) {
            this->encode(
                FE_MOV64mr,
                FE_MEM(
                    FE_BP,
                    0,
                    0,
                    meta_func->saved_register_stack_offset[reg_index]),
                REGISTERS[reg_index]);
        }
    }

    // Generate blocks
    for (InstRef block_ref : func->blocks) {
        Inst block = block_ref.get(this->module);
        MetaValue *meta_block = &this->meta_insts[block_ref.id];
        *meta_block = {};

        meta_block->kind = MetaValueKind_Block;
        meta_block->block.offset = this->get_code_offset();
        for (InstRef inst_ref : block.block.inst_refs) {
            this->generate_inst(func_ref, inst_ref);
            /* this->encode(FE_NOP); */
        }
    }

    // Patch jumps
    for (const FuncJumpPatch &jump_patch : meta_func->jump_patches) {
        MetaValue *meta_block =
            &this->meta_insts[jump_patch.destination_block.id];
        this->encode_at(
            jump_patch.instruction_offset,
            jump_patch.instruction,
            meta_block->block.offset - jump_patch.instruction_offset);
    }

    size_t function_end_offset =
        this->obj_builder->get_section_size(SectionType_Text);

    size_t function_size = function_end_offset - function_start_offset;

    obj_builder->set_symbol_region(
        meta_func->symbol_ref, function_start_offset, function_size);
}

void X86_64AsmBuilder::generate()
{
    ZoneScoped;

    // Generate globals
    for (InstRef global_ref : this->module->globals) {
        Inst global = global_ref.get(this->module);
        this->meta_insts[global_ref.id] =
            this->generate_global(global.global.flags, global.global.data);
    }

    // Generate functions
    for (InstRef func_ref : this->module->functions) {
        this->generate_function(func_ref);
    }
}

void X86_64AsmBuilder::destroy()
{
    this->meta_insts.destroy();
}

} // namespace ace
