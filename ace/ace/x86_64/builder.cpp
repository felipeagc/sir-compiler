#include "ace_obj.hpp"
#include "x86_64/encoder.h"
#include <Tracy.hpp>

namespace ace {

enum RegisterIndex {
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
    FE_AX,    FE_BX,    FE_CX,    FE_DX,    FE_SI,    FE_DI,
    FE_BP,    FE_SP,    FE_R8,    FE_R9,    FE_R10,   FE_R11,
    FE_R12,   FE_R13,   FE_R14,   FE_R15,

    FE_XMM0,  FE_XMM1,  FE_XMM2,  FE_XMM3,  FE_XMM4,  FE_XMM5,
    FE_XMM6,  FE_XMM7,  FE_XMM8,  FE_XMM9,  FE_XMM10, FE_XMM11,
    FE_XMM12, FE_XMM13, FE_XMM14, FE_XMM15,
};

enum MetaValueKind {
    MetaValueKind_None,
    MetaValueKind_Register,
    MetaValueKind_ImmInt,
    MetaValueKind_Stack,
    MetaValueKind_Global,
};

struct MetaValue {
    MetaValueKind kind;
    Type *type;
    union {
        struct {
            uint64_t u64;
        } imm_int;
        struct {
            double f64;
        } imm_float;
        struct {
            int32_t index;
        } reg;
        struct {
            int32_t offset;
        } stack;
        struct {
            SectionType section_type;
            size_t offset;
        } global;
    };
};

struct MetaStackSlot {
    MetaValue value;
};

struct MetaBlock {
    uint64_t offset;
};

struct MetaInst {
    MetaValue value;
};

struct MetaConst {
    MetaValue value;
    bool is_global;
    GlobalRef global_ref;
};

struct MetaGlobal {
    MetaValue value;
};

struct FuncJumpPatch {
    int64_t instruction;
    size_t instruction_offset;
    BlockRef destination_block;
};

struct MetaFunction {
    uint32_t stack_size;
    Array<MetaStackSlot> stack_slots;
    Array<MetaBlock> blocks;
    Array<MetaInst> insts;
    Array<FuncJumpPatch> jump_patches;
    bool registers_used[RegisterIndex_COUNT];
    int32_t saved_register_stack_offset[RegisterIndex_COUNT];
    Slice<RegisterIndex> callee_saved_registers;
    SymbolRef symbol_ref{};

    void init(Module *module, ObjectBuilder *obj_builder, Function *func)
    {
        ZoneScoped;

        *this = {};

        this->jump_patches = Array<FuncJumpPatch>::create(module->arena);

        this->stack_slots = Array<MetaStackSlot>::create(module->arena);
        this->stack_slots.resize(func->stack_slots.len);

        this->blocks = Array<MetaBlock>::create(module->arena);
        this->blocks.resize(func->blocks.len);

        this->insts = Array<MetaInst>::create(module->arena);
        this->insts.resize(func->insts.len);

        switch (func->calling_convention) {
        case CallingConvention_SystemV: {
            static RegisterIndex system_v_callee_saved[] = {
                RegisterIndex_RBX,
                RegisterIndex_R12,
                RegisterIndex_R13,
                RegisterIndex_R14,
                RegisterIndex_R15,
            };
            this->callee_saved_registers = {system_v_callee_saved};
            break;
        }
        }

        if (func->block_refs.is_empty()) {
            // Function without body
            this->symbol_ref = obj_builder->add_symbol(
                func->name,
                SectionType_None,
                SymbolType_None,
                Linkage_External);
        } else {
            // Function with body
            this->symbol_ref = obj_builder->add_symbol(
                func->name,
                SectionType_Text,
                SymbolType_Function,
                Linkage_External);
        }
    }

    ACE_INLINE
    RegisterIndex get_scratch_int_register_index()
    {
        return RegisterIndex_RAX;
    }

    ACE_INLINE
    RegisterIndex get_scratch_float_register_index()
    {
        return RegisterIndex_XMM0;
    }
};

struct X86_64AsmBuilder : AsmBuilder {
    Module *module;
    ObjectBuilder *obj_builder;
    Array<MetaFunction> meta_funcs;
    Array<MetaGlobal> meta_globals;
    Array<MetaConst> meta_consts;

    size_t get_code_offset();
    size_t encode(
        uint64_t mnem, FeOp op0 = 0, FeOp op1 = 0, FeOp op2 = 0, FeOp op3 = 0);
    size_t encode_at(
        size_t offset,
        uint64_t mnem,
        FeOp op0 = 0,
        FeOp op1 = 0,
        FeOp op2 = 0,
        FeOp op3 = 0);
    size_t encode_direct_call(FunctionRef func_ref);
    void encode_function_ending(FunctionRef func_ref);
    void encode_move(FunctionRef func_ref, MetaValue *source, MetaValue *dest);
    void encode_lea(FunctionRef func_ref, MetaValue *source, MetaValue *dest);
    void encode_move_int_to_register(
        MetaFunction *meta_func,
        MetaValue *source,
        MetaValue *dest,
        size_t size);
    void encode_move_int_to_stack(
        MetaFunction *meta_func,
        MetaValue *source,
        MetaValue *dest,
        size_t size);
    void encode_move_int_to_global(
        MetaFunction *meta_func,
        MetaValue *source,
        MetaValue *dest,
        size_t size);
    void encode_move_float_to_register(
        MetaFunction *meta_func,
        MetaValue *source,
        MetaValue *dest,
        size_t size);
    void encode_move_float_to_stack(
        MetaFunction *meta_func,
        MetaValue *source,
        MetaValue *dest,
        size_t size);
    void encode_move_float_to_global(
        MetaFunction *meta_func,
        MetaValue *source,
        MetaValue *dest,
        size_t size);

    void
    generate_inst(FunctionRef func_ref, BlockRef block_ref, InstRef inst_ref);
    void generate_global(GlobalRef global_ref);
    void generate_function(FunctionRef func_ref);

    virtual void generate() override;
    virtual void destroy() override;
};

AsmBuilder *create_x86_64_builder(Module *module, ObjectBuilder *obj_builder)
{
    ZoneScoped;

    auto asm_builder = module->arena->alloc_init<X86_64AsmBuilder>();
    asm_builder->module = module;
    asm_builder->obj_builder = obj_builder;

    asm_builder->meta_consts =
        Array<MetaConst>::create(MallocAllocator::get_instance());
    asm_builder->meta_consts.resize(module->consts.len);

    for (size_t i = 0; i < module->consts.len; ++i) {
        Const *constant = &module->consts[i];
        MetaConst *meta_const = &asm_builder->meta_consts[i];
        *meta_const = {};

        switch (constant->type->kind) {
        case TypeKind_Int: {
            meta_const->value = MetaValue{
                .kind = MetaValueKind_ImmInt,
                .type = constant->type,
                .imm_int = {constant->u64},
            };
            break;
        }
        case TypeKind_Float: {
            uint8_t data[8];
            size_t byte_size = constant->type->size_of(module);
            switch (constant->type->int_.bits) {
            case 32: *((float *)data) = (float)constant->f64; break;
            case 64: *((double *)data) = (double)constant->f64; break;
            }

            meta_const->is_global = true;
            meta_const->global_ref = module->add_global(
                constant->type,
                GlobalFlags_Initialized | GlobalFlags_ReadOnly,
                {data, byte_size});

            break;
        }
        default: {
            break;
        }
        }
    }

    asm_builder->meta_globals =
        Array<MetaGlobal>::create(MallocAllocator::get_instance());
    asm_builder->meta_globals.resize(module->globals.len);

    for (size_t i = 0; i < module->globals.len; ++i) {
        MetaGlobal *meta_global = &asm_builder->meta_globals[i];
        *meta_global = {};
    }

    asm_builder->meta_funcs =
        Array<MetaFunction>::create(MallocAllocator::get_instance());
    asm_builder->meta_funcs.resize(module->functions.len);

    for (size_t i = 0; i < module->functions.len; ++i) {
        Function *func = &module->functions[i];
        MetaFunction *meta_func = &asm_builder->meta_funcs[i];

        meta_func->init(module, obj_builder, func);
    }

    return asm_builder;
}

ACE_INLINE
MetaValue create_register_value(Type *type, RegisterIndex index)
{
    ZoneScoped;

    MetaValue value = {};
    value.kind = MetaValueKind_Register;
    value.type = type;
    value.reg.index = index;
    return value;
}

ACE_INLINE
MetaValue create_stack_value(Type *type, int32_t offset)
{
    ZoneScoped;

    MetaValue value = {};
    value.kind = MetaValueKind_Stack;
    value.type = type;
    value.stack.offset = offset;
    return value;
}

ACE_INLINE
MetaValue create_imm_int_value(Type *type, uint64_t int_value)
{
    ZoneScoped;

    MetaValue value = {};
    value.kind = MetaValueKind_ImmInt;
    value.type = type;
    value.imm_int.u64 = int_value;
    return value;
}

ACE_INLINE
MetaValue
create_global_value(Type *type, SectionType section_type, size_t offset)
{
    ZoneScoped;

    MetaValue value = {};
    value.kind = MetaValueKind_Global;
    value.type = type;
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
size_t X86_64AsmBuilder::encode_direct_call(FunctionRef func_ref)
{
    ZoneScoped;

    uint8_t code[] = {0xe8, 0x00, 0x00, 0x00, 0x00};
    Slice<uint8_t> code_slice = code;
    this->obj_builder->add_to_section(SectionType_Text, code_slice);

    size_t curr_offset = this->get_code_offset();

    MetaFunction *meta_func = &this->meta_funcs[func_ref.id];

    obj_builder->add_procedure_relocation(
        meta_func->symbol_ref, curr_offset - 4, 4);

    return code_slice.len;
}

void X86_64AsmBuilder::encode_function_ending(FunctionRef func_ref)
{
    ZoneScoped;

    MetaFunction *meta_func = &this->meta_funcs[func_ref.id];

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
    FunctionRef func_ref, MetaValue *source, MetaValue *dest)
{
    ZoneScoped;

    MetaFunction *meta_func = &this->meta_funcs[func_ref.id];

    switch (dest->kind) {
    case MetaValueKind_None:
    case MetaValueKind_ImmInt:
    case MetaValueKind_Global: ACE_ASSERT(0);

    case MetaValueKind_Register: {
        switch (source->kind) {
        case MetaValueKind_None:
        case MetaValueKind_Register:
        case MetaValueKind_ImmInt: ACE_ASSERT(0);

        case MetaValueKind_Stack: {
            int64_t source_mem = FE_MEM(FE_BP, 0, 0, source->stack.offset);
            this->encode(FE_LEA64rm, REGISTERS[dest->reg.index], source_mem);
            break;
        }

        case MetaValueKind_Global: {
            int64_t source_mem = FE_MEM(FE_IP, 0, 0, 0);

            int64_t inst_len = this->encode(
                FE_LEA64rm, REGISTERS[dest->reg.index], source_mem);
            ACE_ASSERT(inst_len > 4);

            int64_t relocation_offset = this->get_code_offset() - 4;

            this->obj_builder->add_data_relocation(
                source->global.section_type,
                source->global.offset,
                relocation_offset,
                4);
            break;
        }
        }
        break;
    }

    case MetaValueKind_Stack: {
        int64_t dest_mem = FE_MEM(FE_BP, 0, 0, dest->stack.offset);

        switch (source->kind) {
        case MetaValueKind_None:
        case MetaValueKind_Register:
        case MetaValueKind_ImmInt: ACE_ASSERT(0);

        case MetaValueKind_Stack: {
            int64_t scratch_register =
                REGISTERS[meta_func->get_scratch_int_register_index()];

            int64_t source_mem = FE_MEM(FE_BP, 0, 0, source->stack.offset);
            this->encode(FE_LEA64rm, scratch_register, source_mem);
            this->encode(FE_MOV64mr, dest_mem, scratch_register);
            break;
        }

        case MetaValueKind_Global: {
            int64_t scratch_register =
                REGISTERS[meta_func->get_scratch_int_register_index()];

            int64_t source_mem = FE_MEM(FE_IP, 0, 0, 0);

            int64_t inst_len =
                this->encode(FE_LEA64rm, scratch_register, source_mem);
            ACE_ASSERT(inst_len > 4);

            int64_t relocation_offset = this->get_code_offset() - 4;

            this->encode(FE_MOV64mr, dest_mem, scratch_register);

            this->obj_builder->add_data_relocation(
                source->global.section_type,
                source->global.offset,
                relocation_offset,
                4);
            break;
        }
        }
        break;
    }
    }
}

void X86_64AsmBuilder::encode_move_int_to_register(
    MetaFunction *meta_func, MetaValue *source, MetaValue *dest, size_t size)
{
    ZoneScoped;
    (void)meta_func;

    ACE_ASSERT(dest->kind == MetaValueKind_Register);

    switch (source->kind) {
    case MetaValueKind_None: ACE_ASSERT(0); break;

    case MetaValueKind_Register: {
        int64_t x86_inst;
        switch (size) {
        case 1: x86_inst = FE_MOV8rr; break;
        case 2: x86_inst = FE_MOV16rr; break;
        case 4: x86_inst = FE_MOV32rr; break;
        case 8: x86_inst = FE_MOV64rr; break;
        default: ACE_ASSERT(0);
        }

        this->encode(
            x86_inst, REGISTERS[dest->reg.index], REGISTERS[source->reg.index]);
        break;
    }

    case MetaValueKind_Stack: {
        int64_t source_mem = FE_MEM(FE_BP, 0, 0, source->stack.offset);

        int64_t x86_inst;
        switch (size) {
        case 1: x86_inst = FE_MOV8rm; break;
        case 2: x86_inst = FE_MOV16rm; break;
        case 4: x86_inst = FE_MOV32rm; break;
        case 8: x86_inst = FE_MOV64rm; break;
        default: ACE_ASSERT(0);
        }

        this->encode(x86_inst, REGISTERS[dest->reg.index], source_mem);
        break;
    }

    case MetaValueKind_Global: {
        int64_t source_mem = FE_MEM(FE_IP, 0, 0, 0);

        int64_t x86_inst;
        switch (size) {
        case 1: x86_inst = FE_MOV8rm; break;
        case 2: x86_inst = FE_MOV16rm; break;
        case 4: x86_inst = FE_MOV32rm; break;
        case 8: x86_inst = FE_MOV64rm; break;
        default: ACE_ASSERT(0);
        }

        int64_t inst_len =
            this->encode(x86_inst, REGISTERS[dest->reg.index], source_mem);
        ACE_ASSERT(inst_len > 4);

        int64_t relocation_offset = this->get_code_offset() - 4;

        this->obj_builder->add_data_relocation(
            source->global.section_type,
            source->global.offset,
            relocation_offset,
            4);

        break;
    }

    case MetaValueKind_ImmInt: {
        int64_t x86_inst;
        switch (size) {
        case 1: x86_inst = FE_MOV8ri; break;
        case 2: x86_inst = FE_MOV16ri; break;
        case 4: x86_inst = FE_MOV32ri; break;
        case 8: x86_inst = FE_MOV64ri; break;
        default: ACE_ASSERT(0);
        }

        this->encode(x86_inst, REGISTERS[dest->reg.index], source->imm_int.u64);
        break;
    }
    }
}

void X86_64AsmBuilder::encode_move_int_to_stack(
    MetaFunction *meta_func, MetaValue *source, MetaValue *dest, size_t size)
{
    ZoneScoped;
    ACE_ASSERT(dest->kind == MetaValueKind_Stack);

    int64_t dest_mem = FE_MEM(FE_BP, 0, 0, dest->stack.offset);

    switch (source->kind) {
    case MetaValueKind_None: ACE_ASSERT(0); break;

    case MetaValueKind_Register: {
        int64_t x86_inst;
        switch (size) {
        case 1: x86_inst = FE_MOV8mr; break;
        case 2: x86_inst = FE_MOV16mr; break;
        case 4: x86_inst = FE_MOV32mr; break;
        case 8: x86_inst = FE_MOV64mr; break;
        default: ACE_ASSERT(0);
        }

        this->encode(x86_inst, dest_mem, REGISTERS[source->reg.index]);
        break;
    }

    case MetaValueKind_Stack: {
        int64_t scratch_register =
            REGISTERS[meta_func->get_scratch_int_register_index()];
        int64_t source_mem = FE_MEM(FE_BP, 0, 0, source->stack.offset);

        int64_t x86_inst1;
        int64_t x86_inst2;
        switch (size) {
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

    case MetaValueKind_ImmInt: {
        int64_t x86_inst;
        switch (size) {
        case 1: x86_inst = FE_MOV8mi; break;
        case 2: x86_inst = FE_MOV16mi; break;
        case 4: x86_inst = FE_MOV32mi; break;
        case 8: x86_inst = FE_MOV64mi; break;
        default: ACE_ASSERT(0);
        }

        this->encode(x86_inst, dest_mem, source->imm_int.u64);
        break;
    }

    case MetaValueKind_Global: {
        int64_t scratch_register =
            REGISTERS[meta_func->get_scratch_int_register_index()];
        int64_t source_mem = FE_MEM(FE_IP, 0, 0, 0);

        int64_t x86_inst1;
        int64_t x86_inst2;
        switch (size) {
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
            source->global.section_type,
            source->global.offset,
            relocation_offset,
            4);

        this->encode(x86_inst2, dest_mem, scratch_register);
        break;
    }
    }
}

void X86_64AsmBuilder::encode_move_int_to_global(
    MetaFunction *meta_func, MetaValue *source, MetaValue *dest, size_t size)
{
    ZoneScoped;
    ACE_ASSERT(dest->kind == MetaValueKind_Global);

    int64_t dest_mem = FE_MEM(FE_IP, 0, 0, 0);

    switch (source->kind) {
    case MetaValueKind_None: ACE_ASSERT(0); break;

    case MetaValueKind_Register: {
        int64_t x86_inst;
        switch (size) {
        case 1: x86_inst = FE_MOV8mr; break;
        case 2: x86_inst = FE_MOV16mr; break;
        case 4: x86_inst = FE_MOV32mr; break;
        case 8: x86_inst = FE_MOV64mr; break;
        default: ACE_ASSERT(0);
        }

        int64_t inst_len =
            this->encode(x86_inst, dest_mem, REGISTERS[source->reg.index]);
        ACE_ASSERT(inst_len > 4);

        int64_t relocation_offset = this->get_code_offset() - 4;

        this->obj_builder->add_data_relocation(
            dest->global.section_type,
            dest->global.offset,
            relocation_offset,
            4);
        break;
    }

    case MetaValueKind_Stack: {
        int64_t scratch_register =
            REGISTERS[meta_func->get_scratch_int_register_index()];
        int64_t source_mem = FE_MEM(FE_BP, 0, 0, source->stack.offset);

        int64_t x86_inst1;
        int64_t x86_inst2;
        switch (size) {
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
        size_t inst_len = this->encode(x86_inst2, dest_mem, scratch_register);
        ACE_ASSERT(inst_len > 4);

        int64_t relocation_offset = this->get_code_offset() - 4;

        this->obj_builder->add_data_relocation(
            dest->global.section_type,
            dest->global.offset,
            relocation_offset,
            4);
        break;
    }

    case MetaValueKind_ImmInt: {
        int64_t scratch_register =
            REGISTERS[meta_func->get_scratch_int_register_index()];

        int64_t x86_inst1;
        int64_t x86_inst2;
        switch (size) {
        case 1:
            x86_inst1 = FE_MOV8ri;
            x86_inst2 = FE_MOV8mr;
            break;
        case 2:
            x86_inst1 = FE_MOV16ri;
            x86_inst2 = FE_MOV16mr;
            break;
        case 4:
            x86_inst1 = FE_MOV32ri;
            x86_inst2 = FE_MOV32mr;
            break;
        case 8:
            x86_inst1 = FE_MOV64ri;
            x86_inst2 = FE_MOV64mr;
            break;
        default: ACE_ASSERT(0);
        }

        this->encode(x86_inst1, scratch_register, source->imm_int.u64);
        size_t inst_len = this->encode(x86_inst2, dest_mem, scratch_register);
        ACE_ASSERT(inst_len > 4);

        int64_t relocation_offset = this->get_code_offset() - 4;

        this->obj_builder->add_data_relocation(
            dest->global.section_type,
            dest->global.offset,
            relocation_offset,
            4);
        break;
    }

    case MetaValueKind_Global: {
        ACE_ASSERT(!"unimplemented");
        break;
    }
    }
}

void X86_64AsmBuilder::encode_move_float_to_register(
    MetaFunction *meta_func, MetaValue *source, MetaValue *dest, size_t size)
{
    ZoneScoped;
    (void)meta_func;

    ACE_ASSERT(dest->kind == MetaValueKind_Register);

    switch (source->kind) {
    case MetaValueKind_ImmInt:
    case MetaValueKind_None: ACE_ASSERT(0); break;

    case MetaValueKind_Register: {
        int64_t x86_inst;
        switch (size) {
        case 4: x86_inst = FE_SSE_MOVSSrr; break;
        case 8: x86_inst = FE_SSE_MOVSDrr; break;
        default: ACE_ASSERT(0);
        }

        this->encode(
            x86_inst, REGISTERS[dest->reg.index], REGISTERS[source->reg.index]);
        break;
    }

    case MetaValueKind_Stack: {
        int64_t source_mem = FE_MEM(FE_BP, 0, 0, source->stack.offset);

        int64_t x86_inst;
        switch (size) {
        case 4: x86_inst = FE_SSE_MOVSSrm; break;
        case 8: x86_inst = FE_SSE_MOVSDrm; break;
        default: ACE_ASSERT(0);
        }

        this->encode(x86_inst, REGISTERS[dest->reg.index], source_mem);
        break;
    }

    case MetaValueKind_Global: {
        int64_t source_mem = FE_MEM(FE_IP, 0, 0, 0);

        int64_t x86_inst;
        switch (size) {
        case 4: x86_inst = FE_SSE_MOVSSrm; break;
        case 8: x86_inst = FE_SSE_MOVSDrm; break;
        default: ACE_ASSERT(0);
        }

        int64_t inst_len =
            this->encode(x86_inst, REGISTERS[dest->reg.index], source_mem);
        ACE_ASSERT(inst_len > 4);

        int64_t relocation_offset = this->get_code_offset() - 4;

        this->obj_builder->add_data_relocation(
            source->global.section_type,
            source->global.offset,
            relocation_offset,
            4);

        break;
    }
    }
}

void X86_64AsmBuilder::encode_move_float_to_stack(
    MetaFunction *meta_func, MetaValue *source, MetaValue *dest, size_t size)
{
    ZoneScoped;
    ACE_ASSERT(dest->kind == MetaValueKind_Stack);

    int64_t dest_mem = FE_MEM(FE_BP, 0, 0, dest->stack.offset);

    switch (source->kind) {
    case MetaValueKind_ImmInt:
    case MetaValueKind_None: ACE_ASSERT(0); break;

    case MetaValueKind_Register: {
        int64_t x86_inst;
        switch (size) {
        case 4: x86_inst = FE_SSE_MOVSSmr; break;
        case 8: x86_inst = FE_SSE_MOVSDmr; break;
        default: ACE_ASSERT(0);
        }

        this->encode(x86_inst, dest_mem, REGISTERS[source->reg.index]);
        break;
    }

    case MetaValueKind_Stack: {
        int64_t scratch_register =
            REGISTERS[meta_func->get_scratch_float_register_index()];
        int64_t source_mem = FE_MEM(FE_BP, 0, 0, source->stack.offset);

        int64_t x86_inst1;
        int64_t x86_inst2;
        switch (size) {
        case 4:
            x86_inst1 = FE_SSE_MOVSSrm;
            x86_inst2 = FE_SSE_MOVSSmr;
            break;
        case 8:
            x86_inst1 = FE_SSE_MOVSDrm;
            x86_inst2 = FE_SSE_MOVSDmr;
            break;
        default: ACE_ASSERT(0);
        }

        this->encode(x86_inst1, scratch_register, source_mem);
        this->encode(x86_inst2, dest_mem, scratch_register);
        break;
    }

    case MetaValueKind_Global: {
        int64_t scratch_register =
            REGISTERS[meta_func->get_scratch_float_register_index()];
        int64_t source_mem = FE_MEM(FE_IP, 0, 0, 0);

        int64_t x86_inst1;
        int64_t x86_inst2;
        switch (size) {
        case 4:
            x86_inst1 = FE_SSE_MOVSSrm;
            x86_inst2 = FE_SSE_MOVSSmr;
            break;
        case 8:
            x86_inst1 = FE_SSE_MOVSDrm;
            x86_inst2 = FE_SSE_MOVSDmr;
            break;
        default: ACE_ASSERT(0);
        }

        int64_t inst_len =
            this->encode(x86_inst1, scratch_register, source_mem);
        ACE_ASSERT(inst_len > 4);

        int64_t relocation_offset = this->get_code_offset() - 4;

        this->obj_builder->add_data_relocation(
            source->global.section_type,
            source->global.offset,
            relocation_offset,
            4);

        this->encode(x86_inst2, dest_mem, scratch_register);
        break;
    }
    }
}

void X86_64AsmBuilder::encode_move_float_to_global(
    MetaFunction *meta_func, MetaValue *source, MetaValue *dest, size_t size)
{
    ZoneScoped;
    ACE_ASSERT(dest->kind == MetaValueKind_Global);

    int64_t dest_mem = FE_MEM(FE_IP, 0, 0, 0);

    switch (source->kind) {
    case MetaValueKind_ImmInt:
    case MetaValueKind_None: ACE_ASSERT(0); break;

    case MetaValueKind_Register: {
        int64_t x86_inst;
        switch (size) {
        case 4: x86_inst = FE_SSE_MOVSSmr; break;
        case 8: x86_inst = FE_SSE_MOVSDmr; break;
        default: ACE_ASSERT(0);
        }

        int64_t inst_len =
            this->encode(x86_inst, dest_mem, REGISTERS[source->reg.index]);
        ACE_ASSERT(inst_len > 4);

        int64_t relocation_offset = this->get_code_offset() - 4;

        this->obj_builder->add_data_relocation(
            dest->global.section_type,
            dest->global.offset,
            relocation_offset,
            4);
        break;
    }

    case MetaValueKind_Stack: {
        int64_t scratch_register =
            REGISTERS[meta_func->get_scratch_float_register_index()];
        int64_t source_mem = FE_MEM(FE_BP, 0, 0, source->stack.offset);

        int64_t x86_inst1;
        int64_t x86_inst2;
        switch (size) {
        case 4:
            x86_inst1 = FE_SSE_MOVSSrm;
            x86_inst2 = FE_SSE_MOVSSmr;
            break;
        case 8:
            x86_inst1 = FE_SSE_MOVSDrm;
            x86_inst2 = FE_SSE_MOVSDmr;
            break;
        default: ACE_ASSERT(0);
        }

        this->encode(x86_inst1, scratch_register, source_mem);
        size_t inst_len = this->encode(x86_inst2, dest_mem, scratch_register);
        ACE_ASSERT(inst_len > 4);

        int64_t relocation_offset = this->get_code_offset() - 4;

        this->obj_builder->add_data_relocation(
            dest->global.section_type,
            dest->global.offset,
            relocation_offset,
            4);
        break;
    }

    case MetaValueKind_Global: {
        int64_t scratch_register =
            REGISTERS[meta_func->get_scratch_float_register_index()];
        int64_t source_mem = FE_MEM(FE_IP, 0, 0, 0);

        int64_t x86_inst1;
        int64_t x86_inst2;
        switch (size) {
        case 4:
            x86_inst1 = FE_SSE_MOVSSrm;
            x86_inst2 = FE_SSE_MOVSSmr;
            break;
        case 8:
            x86_inst1 = FE_SSE_MOVSDrm;
            x86_inst2 = FE_SSE_MOVSDmr;
            break;
        default: ACE_ASSERT(0);
        }

        size_t inst_len = this->encode(x86_inst1, scratch_register, source_mem);
        ACE_ASSERT(inst_len > 4);
        int64_t relocation_offset = this->get_code_offset() - 4;

        this->obj_builder->add_data_relocation(
            source->global.section_type,
            source->global.offset,
            relocation_offset,
            4);

        inst_len = this->encode(x86_inst2, dest_mem, scratch_register);
        ACE_ASSERT(inst_len > 4);

        relocation_offset = this->get_code_offset() - 4;

        this->obj_builder->add_data_relocation(
            dest->global.section_type,
            dest->global.offset,
            relocation_offset,
            4);
        break;
    }
    }
}

void X86_64AsmBuilder::encode_move(
    FunctionRef func_ref, MetaValue *source, MetaValue *dest)
{
    ZoneScoped;

    ACE_ASSERT(source->type);
    ACE_ASSERT(dest->type);
    ACE_ASSERT(source->type == dest->type);

    MetaFunction *meta_func = &this->meta_funcs[func_ref.id];

    uint32_t size = source->type->size_of(this->module);

    switch (source->type->kind) {
    case TypeKind_Pointer:
    case TypeKind_Int: {
        switch (dest->kind) {
        case MetaValueKind_ImmInt:
        case MetaValueKind_None: ACE_ASSERT(0); break;

        case MetaValueKind_Register: {
            this->encode_move_int_to_register(meta_func, source, dest, size);
            break;
        }

        case MetaValueKind_Stack: {
            this->encode_move_int_to_stack(meta_func, source, dest, size);
            break;
        }

        case MetaValueKind_Global: {
            this->encode_move_int_to_global(meta_func, source, dest, size);
            break;
        }
        }
        break;
    }
    case TypeKind_Float: {
        switch (dest->kind) {
        case MetaValueKind_ImmInt:
        case MetaValueKind_None: ACE_ASSERT(0); break;

        case MetaValueKind_Register: {
            this->encode_move_float_to_register(meta_func, source, dest, size);
            break;
        }

        case MetaValueKind_Stack: {
            this->encode_move_float_to_stack(meta_func, source, dest, size);
            break;
        }

        case MetaValueKind_Global: {
            this->encode_move_float_to_global(meta_func, source, dest, size);
            break;
        }
        }
        break;
    }
    default: {
        ACE_ASSERT(!"unsupported type for move");
        break;
    }
    }
}

void X86_64AsmBuilder::generate_inst(
    FunctionRef func_ref, BlockRef block_ref, InstRef inst_ref)
{
    ZoneScoped;

    (void)block_ref;

    Function *func = &this->module->functions[func_ref.id];
    MetaFunction *meta_func = &this->meta_funcs[func_ref.id];
    Inst *inst = &func->insts[inst_ref.id];
    MetaInst *meta_inst = &meta_func->insts[inst_ref.id];

    switch (inst->kind) {
    case InstKind_FunctionParameter: {
        break;
    }
    case InstKind_PtrCast: {
        MetaInst *source_meta_inst =
            &meta_func->insts[inst->ptr_cast.inst_ref.id];
        meta_inst->value = source_meta_inst->value;
        meta_inst->value.type = inst->type;
        break;
    }
    case InstKind_GetConst: {
        MetaConst *meta_const =
            &this->meta_consts[inst->get_const.const_ref.id];
        meta_inst->value = meta_const->value;
        break;
    }
    case InstKind_FuncCall: {
        Function *called_func =
            &this->module->functions[inst->func_call.func_ref.id];

        // TODO: save and restore caller-saved registers

        ACE_ASSERT(
            called_func->param_types.len <= inst->func_call.parameters.len);

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

            for (size_t i = 0; i < inst->func_call.parameters.len; ++i) {
                InstRef param_inst_ref = inst->func_call.parameters[i];
                Inst *param_inst = &func->insts[param_inst_ref.id];
                MetaInst *meta_param_inst =
                    &meta_func->insts[param_inst_ref.id];

                if (i < called_func->param_types.len) {
                    Type *param_type = called_func->param_types[i];
                    ACE_ASSERT(param_type == param_inst->type);
                }

                MetaValue *source_param_value = &meta_param_inst->value;
                MetaValue dest_param_value = {};

                switch (param_inst->type->kind) {
                case TypeKind_Pointer:
                case TypeKind_Int: {
                    dest_param_value = create_register_value(
                        param_inst->type, sysv_int_param_regs[int_count]);
                    int_count++;
                    break;
                }
                case TypeKind_Float: {
                    dest_param_value = create_register_value(
                        param_inst->type, sysv_float_param_regs[float_count]);
                    float_count++;
                    break;
                }
                default: {
                    ACE_ASSERT(!"unhandled parameter type");
                    break;
                }
                }

                this->encode_move(
                    func_ref, source_param_value, &dest_param_value);
            }

            if (called_func->variadic) {
                // Write the amount of vector registers to %AL
                MetaValue float_count_imm =
                    create_imm_int_value(module->i8_type, float_count);
                MetaValue float_count_reg =
                    create_register_value(module->i8_type, RegisterIndex_RAX);
                this->encode_move(func_ref, &float_count_imm, &float_count_reg);
            }

            break;
        }
        }

        this->encode_direct_call(inst->func_call.func_ref);

        switch (called_func->calling_convention) {
        case CallingConvention_SystemV: {
            switch (called_func->return_type->kind) {
            case TypeKind_Void: break;

            case TypeKind_Int:
            case TypeKind_Pointer: {
                MetaValue returned_value = {
                    .kind = MetaValueKind_Register,
                    .type = called_func->return_type,
                    .reg = {.index = RegisterIndex_RAX},
                };

                this->encode_move(func_ref, &returned_value, &meta_inst->value);
                break;
            }

            case TypeKind_Float: {
                MetaValue returned_value = {
                    .kind = MetaValueKind_Register,
                    .type = called_func->return_type,
                    .reg = {.index = RegisterIndex_XMM0},
                };

                this->encode_move(func_ref, &returned_value, &meta_inst->value);
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
        Inst *returned_inst = &func->insts[inst->return_value.inst_ref.id];
        MetaInst *returned_meta_inst =
            &meta_func->insts[inst->return_value.inst_ref.id];

        // TODO: ABI compliance
        MetaValue return_value =
            create_register_value(returned_inst->type, RegisterIndex_RAX);
        this->encode_move(func_ref, &returned_meta_inst->value, &return_value);

        this->encode_function_ending(func_ref);
        break;
    }
    case InstKind_StackStore: {
        MetaStackSlot *meta_stack_slot =
            &meta_func->stack_slots[inst->stack_store.ss_ref.id];
        MetaInst *stored_meta_inst =
            &meta_func->insts[inst->stack_store.inst_ref.id];

        this->encode_move(
            func_ref, &stored_meta_inst->value, &meta_stack_slot->value);

        break;
    }
    case InstKind_StackLoad: {
        MetaStackSlot *meta_stack_slot =
            &meta_func->stack_slots[inst->stack_load.ss_ref.id];

        MetaInst *meta_inst = &meta_func->insts[inst_ref.id];

        this->encode_move(func_ref, &meta_stack_slot->value, &meta_inst->value);

        break;
    }
    case InstKind_GlobalStore: {
        MetaGlobal *meta_global =
            &this->meta_globals[inst->global_store.global_ref.id];
        MetaInst *stored_meta_inst =
            &meta_func->insts[inst->global_store.inst_ref.id];

        this->encode_move(
            func_ref, &stored_meta_inst->value, &meta_global->value);

        break;
    }
    case InstKind_GlobalLoad: {
        MetaGlobal *meta_global =
            &this->meta_globals[inst->global_store.global_ref.id];

        MetaInst *meta_inst = &meta_func->insts[inst_ref.id];

        this->encode_move(func_ref, &meta_global->value, &meta_inst->value);

        break;
    }
    case InstKind_GlobalAddr: {
        MetaGlobal *meta_global =
            &this->meta_globals[inst->global_store.global_ref.id];

        MetaInst *meta_inst = &meta_func->insts[inst_ref.id];

        this->encode_lea(func_ref, &meta_global->value, &meta_inst->value);
        break;
    }
    case InstKind_StackAddr: {
        MetaStackSlot *meta_stack_slot =
            &meta_func->stack_slots[inst->stack_load.ss_ref.id];

        MetaInst *meta_inst = &meta_func->insts[inst_ref.id];

        this->encode_lea(func_ref, &meta_stack_slot->value, &meta_inst->value);
        break;
    }
    case InstKind_Jump: {
        FuncJumpPatch patch = {
            .instruction = FE_JMP | FE_JMPL,
            .instruction_offset = this->get_code_offset(),
            .destination_block = inst->jump.block_ref,
        };
        meta_func->jump_patches.push_back(patch);

        this->encode(FE_JMP | FE_JMPL, -patch.instruction_offset);

        break;
    }
    }
}

void X86_64AsmBuilder::generate_global(GlobalRef global_ref)
{
    ZoneScoped;

    Global *global = &this->module->globals[global_ref.id];
    MetaGlobal *meta_global = &this->meta_globals[global_ref.id];

    SectionType section_type = SectionType_Data;
    if (!(global->flags & GlobalFlags_Initialized)) {
        section_type = SectionType_BSS;
    } else {
        if (global->flags & GlobalFlags_ReadOnly) {
            section_type = SectionType_ROData;
        } else {
            section_type = SectionType_Data;
        }
    }

    size_t offset = this->obj_builder->get_section_size(section_type);
    this->obj_builder->add_to_section(section_type, global->data);

    meta_global->value = {
        .kind = MetaValueKind_Global,
        .type = global->type,
        .global =
            {
                .section_type = section_type,
                .offset = offset,
            },
    };
}

void X86_64AsmBuilder::generate_function(FunctionRef func_ref)
{
    ZoneScoped;

    Function *func = &this->module->functions[func_ref.id];
    MetaFunction *meta_func = &this->meta_funcs[func_ref.id];

    if (func->block_refs.is_empty()) {
        return;
    }

    for (size_t i = 0; i < func->stack_slots.len; ++i) {
        StackSlot *stack_slot = &func->stack_slots[i];

        uint32_t slot_size = stack_slot->type->size_of(this->module);
        uint32_t slot_align = stack_slot->type->align_of(this->module);
        meta_func->stack_size = ACE_ROUND_UP(slot_align, meta_func->stack_size);
        meta_func->stack_size += slot_size;

        meta_func->stack_slots[i].value = create_stack_value(
            func->stack_slots[i].type, -((int32_t)meta_func->stack_size));
    }

    // Create stack space for function parameters
    for (InstRef param_inst_ref : func->param_insts) {
        Inst *inst = &func->insts[param_inst_ref.id];
        MetaInst *meta_inst = &meta_func->insts[param_inst_ref.id];
        *meta_inst = {};

        uint32_t inst_size = inst->type->size_of(this->module);
        uint32_t inst_align = inst->type->align_of(this->module);

        meta_func->stack_size = ACE_ROUND_UP(inst_align, meta_func->stack_size);
        meta_func->stack_size += inst_size;

        meta_inst->value =
            create_stack_value(inst->type, -((int32_t)meta_func->stack_size));
    }

    // Spill all elegible insts to stack
    for (auto &block_ref_node : func->block_refs) {
        BlockRef block_ref = block_ref_node.value;
        Block *block = &func->blocks[block_ref.id];
        for (auto &inst_ref : block->inst_refs) {
            Inst *inst = &func->insts[inst_ref.id];
            MetaInst *meta_inst = &meta_func->insts[inst_ref.id];
            *meta_inst = {};

            if (inst->type) {
                switch (inst->kind) {
                case InstKind_FunctionParameter:
                    ACE_ASSERT(0); // Function parameters are handled above
                    break;

                // Don't spill:
                case InstKind_StackStore:
                case InstKind_GlobalStore:
                case InstKind_Jump:
                case InstKind_ReturnVoid:
                case InstKind_ReturnValue:
                case InstKind_GetConst:
                case InstKind_PtrCast: break;

                case InstKind_GlobalAddr:
                case InstKind_StackAddr:
                case InstKind_StackLoad:
                case InstKind_GlobalLoad:
                case InstKind_FuncCall: {
                    uint32_t inst_size = inst->type->size_of(this->module);
                    uint32_t inst_align = inst->type->align_of(this->module);

                    meta_func->stack_size =
                        ACE_ROUND_UP(inst_align, meta_func->stack_size);
                    meta_func->stack_size += inst_size;

                    meta_inst->value = create_stack_value(
                        inst->type, -((int32_t)meta_func->stack_size));
                    break;
                }
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
            MetaInst *param_meta_inst = &meta_func->insts[param_inst_ref.id];

            switch (param_type->kind) {
            case TypeKind_Int:
            case TypeKind_Pointer: {
                MetaValue param_register_value = {
                    .kind = MetaValueKind_Register,
                    .type = param_type,
                    .reg = {.index = sysv_int_param_regs[i]},
                };

                this->encode_move(
                    func_ref, &param_register_value, &param_meta_inst->value);
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

    for (auto &block_ref_node : func->block_refs) {
        BlockRef block_ref = block_ref_node.value;
        Block *block = &func->blocks[block_ref.id];
        MetaBlock *meta_block = &meta_func->blocks[block_ref.id];
        meta_block->offset = this->get_code_offset();
        for (auto &inst_ref : block->inst_refs) {
            this->generate_inst(func_ref, block_ref, inst_ref);
        }
    }

    for (auto &jump_patch : meta_func->jump_patches) {
        MetaBlock *meta_block =
            &meta_func->blocks[jump_patch.destination_block.id];
        this->encode_at(
            jump_patch.instruction_offset,
            jump_patch.instruction,
            meta_block->offset - jump_patch.instruction_offset);
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

    for (uint32_t i = 0; i < this->module->globals.len; ++i) {
        this->generate_global({i});
    }
    for (size_t i = 0; i < module->consts.len; ++i) {
        MetaConst *meta_const = &this->meta_consts[i];

        if (meta_const->is_global) {
            meta_const->value =
                this->meta_globals[meta_const->global_ref.id].value;
        }

        ACE_ASSERT(meta_const->value.kind != MetaValueKind_None);
    }
    for (uint32_t i = 0; i < this->module->functions.len; ++i) {
        this->generate_function({i});
    }
}

void X86_64AsmBuilder::destroy()
{
    this->meta_funcs.destroy();
    this->meta_globals.destroy();
    this->meta_consts.destroy();
}

} // namespace ace
