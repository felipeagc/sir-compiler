#include "sir_obj.hpp"
#include "x86_64/encoder.h"
#include <Tracy.hpp>

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

enum SizeClass : uint8_t {
    SizeClass_None = 0,
    SizeClass_1,
    SizeClass_2,
    SizeClass_4,
    SizeClass_8,
    SizeClass_COUNT,
};

enum OperandKind : uint8_t {
    OperandKind_None = 0,
    OperandKind_Reg,
    OperandKind_FReg,
    OperandKind_Imm,
    OperandKind_Memory,
    OperandKind_COUNT,
};

enum Mnem : uint8_t {
    Mnem_Unknown = 0,
    Mnem_MOV,
    Mnem_LEA,
    Mnem_COUNT,
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
    MetaValueKind_COUNT,
};

static SizeClass SIZE_CLASSES[9];
static size_t SIZE_CLASS_SIZES[SizeClass_COUNT];

static OperandKind OPERAND_KINDS[MetaValueKind_COUNT];

static int64_t ENCODING_ENTRIES[Mnem_COUNT][OperandKind_COUNT][SizeClass_COUNT]
                               [OperandKind_COUNT][SizeClass_COUNT];

static RegisterIndex SYSV_INT_PARAM_REGS[6] = {
    RegisterIndex_RDI,
    RegisterIndex_RSI,
    RegisterIndex_RDX,
    RegisterIndex_RCX,
    RegisterIndex_R8,
    RegisterIndex_R9,
};

static RegisterIndex SYSV_FLOAT_PARAM_REGS[8] = {
    RegisterIndex_XMM0,
    RegisterIndex_XMM1,
    RegisterIndex_XMM2,
    RegisterIndex_XMM3,
    RegisterIndex_XMM4,
    RegisterIndex_XMM5,
    RegisterIndex_XMM6,
    RegisterIndex_XMM7,
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
            SIRSectionType section_type;
        } global;
    };
    MetaValueKind kind;
    SizeClass size_class;

    SIR_INLINE
    int64_t into_operand() const
    {
        switch (this->kind) {
        case MetaValueKind_Unknown:
        case MetaValueKind_Function:
        case MetaValueKind_Block:
        case MetaValueKind_COUNT: SIR_ASSERT(0); break;
        case MetaValueKind_IRegister:
        case MetaValueKind_FRegister: return REGISTERS[this->reg.index];
        case MetaValueKind_IRegisterMemory:
            return FE_MEM(
                REGISTERS[this->regmem.base],
                this->regmem.scale,
                REGISTERS[this->regmem.index],
                this->regmem.offset);
        case MetaValueKind_Global: return FE_MEM(FE_IP, 0, 0, 0);
        case MetaValueKind_ImmInt: return this->imm_int.u64;
        }

        return 0;
    }

    SIR_INLINE void add_relocation(
        X86_64AsmBuilder *builder,
        OperandKind other_operand_kind = OperandKind_None,
        SizeClass other_operand_size_class = SizeClass_None) const;
};

struct FuncJumpPatch {
    int64_t instruction;
    size_t instruction_offset;
    SIRInstRef destination_block;
};

struct MetaFunction {
    uint32_t stack_size;
    SIRArray<FuncJumpPatch> jump_patches;
    bool registers_used[RegisterIndex_COUNT];
    int32_t saved_register_stack_offset[RegisterIndex_COUNT];
    SIRSlice<RegisterIndex> caller_saved_registers;
    SIRSlice<RegisterIndex> callee_saved_registers;
    SIRArray<RegisterIndex> temp_int_register_stack;
    SIRSymbolRef symbol_ref{};
};

struct X86_64AsmBuilder : SIRAsmBuilder {
    SIRModule *module;
    SIRObjectBuilder *obj_builder;
    SIRArray<MetaValue> meta_insts;

    size_t get_code_offset();
    size_t encode_raw(const SIRSlice<uint8_t> &bytes);
    size_t encode(
        uint64_t mnem, FeOp op0 = 0, FeOp op1 = 0, FeOp op2 = 0, FeOp op3 = 0);
    size_t encode_at(
        size_t offset,
        uint64_t mnem,
        FeOp op0 = 0,
        FeOp op1 = 0,
        FeOp op2 = 0,
        FeOp op3 = 0);

    void encode_mnem(Mnem mnem, const MetaValue &source, const MetaValue &dest);

    size_t encode_direct_call(SIRInstRef func_ref);
    void encode_function_ending(SIRInstRef func_ref);

    MetaValue generate_global(uint32_t flags, SIRSlice<uint8_t> data);
    void generate_function(SIRInstRef global_ref);
    void generate_inst(SIRInstRef func_ref, SIRInstRef inst_ref);

    size_t get_func_call_stack_parameters_size(SIRInstRef func_call_ref);
    void move_inst_rvalue(SIRInstRef inst_ref, const MetaValue &dest_value);
    void encode_memcpy(
        size_t value_size, MetaValue source_value, MetaValue dest_value);

    virtual void generate() override;
    virtual void destroy() override;
};

SIR_INLINE void MetaValue::add_relocation(
    X86_64AsmBuilder *builder,
    OperandKind other_operand_kind,
    SizeClass other_operand_size_class) const
{
    if (this->kind == MetaValueKind_Global) {
        int64_t relocation_offset = builder->get_code_offset() - 4;
        if (other_operand_kind == OperandKind_Imm) {
            relocation_offset -= SIZE_CLASS_SIZES[other_operand_size_class];
        }

        builder->obj_builder->add_data_relocation(
            this->global.section_type,
            this->global.offset,
            relocation_offset,
            4);
    }
}

SIRAsmBuilder *
SIRCreateX86_64Builder(SIRModule *module, SIRObjectBuilder *obj_builder)
{
    ZoneScoped;

    auto asm_builder = module->arena->alloc_init<X86_64AsmBuilder>();
    asm_builder->module = module;
    asm_builder->obj_builder = obj_builder;

    asm_builder->meta_insts =
        SIRArray<MetaValue>::create(SIRMallocAllocator::get_instance());
    asm_builder->meta_insts.resize(module->insts.len);

    for (size_t i = 0; i < asm_builder->meta_insts.len; ++i) {
        asm_builder->meta_insts[i] = {};
    }

    SIZE_CLASSES[1] = SizeClass_1;
    SIZE_CLASSES[2] = SizeClass_2;
    SIZE_CLASSES[4] = SizeClass_4;
    SIZE_CLASSES[8] = SizeClass_8;

    SIZE_CLASS_SIZES[SizeClass_1] = 1;
    SIZE_CLASS_SIZES[SizeClass_2] = 2;
    SIZE_CLASS_SIZES[SizeClass_4] = 4;
    SIZE_CLASS_SIZES[SizeClass_8] = 8;

    OPERAND_KINDS[MetaValueKind_IRegisterMemory] = OperandKind_Memory;
    OPERAND_KINDS[MetaValueKind_IRegister] = OperandKind_Reg;
    OPERAND_KINDS[MetaValueKind_FRegister] = OperandKind_FReg;
    OPERAND_KINDS[MetaValueKind_Global] = OperandKind_Memory;
    OPERAND_KINDS[MetaValueKind_ImmInt] = OperandKind_Imm;

    ENCODING_ENTRIES[Mnem_MOV][OperandKind_Reg][SizeClass_8][OperandKind_Reg]
                    [SizeClass_8] = FE_MOV64rr;
    ENCODING_ENTRIES[Mnem_MOV][OperandKind_Reg][SizeClass_4][OperandKind_Reg]
                    [SizeClass_4] = FE_MOV32rr;
    ENCODING_ENTRIES[Mnem_MOV][OperandKind_Reg][SizeClass_2][OperandKind_Reg]
                    [SizeClass_2] = FE_MOV16rr;
    ENCODING_ENTRIES[Mnem_MOV][OperandKind_Reg][SizeClass_1][OperandKind_Reg]
                    [SizeClass_1] = FE_MOV8rr;

    ENCODING_ENTRIES[Mnem_MOV][OperandKind_Reg][SizeClass_8][OperandKind_Memory]
                    [SizeClass_8] = FE_MOV64rm;
    ENCODING_ENTRIES[Mnem_MOV][OperandKind_Reg][SizeClass_4][OperandKind_Memory]
                    [SizeClass_4] = FE_MOV32rm;
    ENCODING_ENTRIES[Mnem_MOV][OperandKind_Reg][SizeClass_2][OperandKind_Memory]
                    [SizeClass_2] = FE_MOV16rm;
    ENCODING_ENTRIES[Mnem_MOV][OperandKind_Reg][SizeClass_1][OperandKind_Memory]
                    [SizeClass_1] = FE_MOV8rm;

    ENCODING_ENTRIES[Mnem_MOV][OperandKind_Reg][SizeClass_8][OperandKind_Imm]
                    [SizeClass_8] = FE_MOV64ri;
    ENCODING_ENTRIES[Mnem_MOV][OperandKind_Reg][SizeClass_4][OperandKind_Imm]
                    [SizeClass_4] = FE_MOV32ri;
    ENCODING_ENTRIES[Mnem_MOV][OperandKind_Reg][SizeClass_2][OperandKind_Imm]
                    [SizeClass_2] = FE_MOV16ri;
    ENCODING_ENTRIES[Mnem_MOV][OperandKind_Reg][SizeClass_1][OperandKind_Imm]
                    [SizeClass_1] = FE_MOV8ri;

    /* ENCODING_ENTRIES[Mnem_MOV][OperandKind_Memory][SizeClass_8] */
    /*                 [OperandKind_Imm][SizeClass_8] = FE_MOV64mi; */
    ENCODING_ENTRIES[Mnem_MOV][OperandKind_Memory][SizeClass_4][OperandKind_Imm]
                    [SizeClass_4] = FE_MOV32mi;
    ENCODING_ENTRIES[Mnem_MOV][OperandKind_Memory][SizeClass_2][OperandKind_Imm]
                    [SizeClass_2] = FE_MOV16mi;
    ENCODING_ENTRIES[Mnem_MOV][OperandKind_Memory][SizeClass_1][OperandKind_Imm]
                    [SizeClass_1] = FE_MOV8mi;

    ENCODING_ENTRIES[Mnem_MOV][OperandKind_Memory][SizeClass_8][OperandKind_Reg]
                    [SizeClass_8] = FE_MOV64mr;
    ENCODING_ENTRIES[Mnem_MOV][OperandKind_Memory][SizeClass_4][OperandKind_Reg]
                    [SizeClass_4] = FE_MOV32mr;
    ENCODING_ENTRIES[Mnem_MOV][OperandKind_Memory][SizeClass_2][OperandKind_Reg]
                    [SizeClass_2] = FE_MOV16mr;
    ENCODING_ENTRIES[Mnem_MOV][OperandKind_Memory][SizeClass_1][OperandKind_Reg]
                    [SizeClass_1] = FE_MOV8mr;

    ENCODING_ENTRIES[Mnem_MOV][OperandKind_FReg][SizeClass_4][OperandKind_FReg]
                    [SizeClass_4] = FE_SSE_MOVSSrr;
    ENCODING_ENTRIES[Mnem_MOV][OperandKind_FReg][SizeClass_8][OperandKind_FReg]
                    [SizeClass_8] = FE_SSE_MOVSDrr;

    ENCODING_ENTRIES[Mnem_MOV][OperandKind_FReg][SizeClass_4]
                    [OperandKind_Memory][SizeClass_4] = FE_SSE_MOVSSrm;
    ENCODING_ENTRIES[Mnem_MOV][OperandKind_FReg][SizeClass_8]
                    [OperandKind_Memory][SizeClass_8] = FE_SSE_MOVSDrm;

    ENCODING_ENTRIES[Mnem_MOV][OperandKind_Memory][SizeClass_4]
                    [OperandKind_FReg][SizeClass_4] = FE_SSE_MOVSSmr;
    ENCODING_ENTRIES[Mnem_MOV][OperandKind_Memory][SizeClass_8]
                    [OperandKind_FReg][SizeClass_8] = FE_SSE_MOVSDmr;

    ENCODING_ENTRIES[Mnem_LEA][OperandKind_Reg][SizeClass_8][OperandKind_Memory]
                    [SizeClass_1] = FE_LEA64rm;
    ENCODING_ENTRIES[Mnem_LEA][OperandKind_Reg][SizeClass_8][OperandKind_Memory]
                    [SizeClass_2] = FE_LEA64rm;
    ENCODING_ENTRIES[Mnem_LEA][OperandKind_Reg][SizeClass_8][OperandKind_Memory]
                    [SizeClass_4] = FE_LEA64rm;
    ENCODING_ENTRIES[Mnem_LEA][OperandKind_Reg][SizeClass_8][OperandKind_Memory]
                    [SizeClass_8] = FE_LEA64rm;
    ENCODING_ENTRIES[Mnem_LEA][OperandKind_Reg][SizeClass_8][OperandKind_Memory]
                    [SizeClass_None] = FE_LEA64rm;

    ENCODING_ENTRIES[Mnem_LEA][OperandKind_Memory][SizeClass_8]
                    [OperandKind_Memory][SizeClass_1] = FE_LEA64rm;
    ENCODING_ENTRIES[Mnem_LEA][OperandKind_Memory][SizeClass_8]
                    [OperandKind_Memory][SizeClass_2] = FE_LEA64rm;
    ENCODING_ENTRIES[Mnem_LEA][OperandKind_Memory][SizeClass_8]
                    [OperandKind_Memory][SizeClass_4] = FE_LEA64rm;
    ENCODING_ENTRIES[Mnem_LEA][OperandKind_Memory][SizeClass_8]
                    [OperandKind_Memory][SizeClass_8] = FE_LEA64rm;
    ENCODING_ENTRIES[Mnem_LEA][OperandKind_Memory][SizeClass_8]
                    [OperandKind_Memory][SizeClass_None] = FE_LEA64rm;

    return asm_builder;
}

SIR_INLINE
MetaValue create_int_register_value(uint8_t bytes, RegisterIndex index)
{
    ZoneScoped;

    MetaValue value = {};
    value.kind = MetaValueKind_IRegister;
    value.size_class = SIZE_CLASSES[bytes];
    value.reg.bytes = bytes;
    value.reg.index = index;
    return value;
}

SIR_INLINE
MetaValue create_float_register_value(uint8_t bytes, RegisterIndex index)
{
    ZoneScoped;

    MetaValue value = {};
    value.kind = MetaValueKind_FRegister;
    value.size_class = SIZE_CLASSES[bytes];
    value.reg.bytes = bytes;
    value.reg.index = index;
    return value;
}

SIR_INLINE
MetaValue create_int_register_memory_value(
    size_t byte_size,
    RegisterIndex base,
    int32_t scale,
    RegisterIndex index,
    int32_t offset)
{
    ZoneScoped;

    MetaValue value = {};
    value.kind = MetaValueKind_IRegisterMemory;
    switch (byte_size) {
    case 1:
    case 2:
    case 4:
    case 8: value.size_class = SIZE_CLASSES[byte_size]; break;
    default: value.size_class = SizeClass_None; break;
    }
    value.regmem.base = base;
    value.regmem.scale = scale;
    value.regmem.index = index;
    value.regmem.offset = offset;
    return value;
}

SIR_INLINE
MetaValue create_stack_value(size_t byte_size, int32_t offset)
{
    ZoneScoped;

    MetaValue value = {};
    value.kind = MetaValueKind_IRegisterMemory;
    switch (byte_size) {
    case 1:
    case 2:
    case 4:
    case 8: value.size_class = SIZE_CLASSES[byte_size]; break;
    default: value.size_class = SizeClass_None; break;
    }
    value.regmem.base = RegisterIndex_RBP;
    value.regmem.offset = offset;
    return value;
}

SIR_INLINE
MetaValue create_imm_int_value(uint8_t bytes, uint64_t int_value)
{
    ZoneScoped;

    MetaValue value = {};
    value.kind = MetaValueKind_ImmInt;
    value.size_class = SIZE_CLASSES[bytes];
    switch (bytes) {
    case 1: value.imm_int.u64 = (int8_t)int_value; break;
    case 2: value.imm_int.u64 = (int16_t)int_value; break;
    case 4: value.imm_int.u64 = (int32_t)int_value; break;
    case 8: value.imm_int.u64 = int_value; break;
    }
    value.imm_int.bytes = bytes;
    return value;
}

SIR_INLINE
MetaValue create_global_value(
    size_t byte_size, SIRSectionType section_type, size_t offset)
{
    ZoneScoped;

    MetaValue value = {};
    value.kind = MetaValueKind_Global;
    switch (byte_size) {
    case 1:
    case 2:
    case 4:
    case 8: value.size_class = SIZE_CLASSES[byte_size]; break;
    default: value.size_class = SizeClass_None; break;
    }
    value.global.offset = offset;
    value.global.section_type = section_type;
    return value;
}

SIR_INLINE
size_t X86_64AsmBuilder::get_code_offset()
{
    return this->obj_builder->get_section_size(SIRSectionType_Text);
}

SIR_INLINE
size_t X86_64AsmBuilder::encode_raw(const SIRSlice<uint8_t> &bytes)
{
    ZoneScoped;

    this->obj_builder->add_to_section(SIRSectionType_Text, bytes);
    return bytes.len;
}

SIR_INLINE
size_t
X86_64AsmBuilder::encode(uint64_t mnem, FeOp op0, FeOp op1, FeOp op2, FeOp op3)
{
    ZoneScoped;

    uint8_t temp[15];
    uint8_t *ptr = &temp[0];
    int failed = fe_enc64(&ptr, mnem, op0, op1, op2, op3);
    SIR_ASSERT(!failed);

    size_t inst_len = (size_t)(ptr - temp);
    this->obj_builder->add_to_section(
        SIRSectionType_Text, {&temp[0], inst_len});
    return inst_len;
}

SIR_INLINE
size_t X86_64AsmBuilder::encode_at(
    size_t offset, uint64_t mnem, FeOp op0, FeOp op1, FeOp op2, FeOp op3)
{
    ZoneScoped;

    uint8_t temp[15];
    uint8_t *ptr = &temp[0];
    int failed = fe_enc64(&ptr, mnem, op0, op1, op2, op3);
    SIR_ASSERT(!failed);

    size_t inst_len = (size_t)(ptr - temp);
    this->obj_builder->set_section_data(
        SIRSectionType_Text, offset, {&temp[0], inst_len});
    return inst_len;
}

SIR_INLINE
size_t X86_64AsmBuilder::encode_direct_call(SIRInstRef func_ref)
{
    ZoneScoped;

    size_t inst_len = this->encode_raw({0xe8, 0x00, 0x00, 0x00, 0x00});

    size_t curr_offset = this->get_code_offset();

    MetaValue *meta_inst = &this->meta_insts[func_ref.id];

    obj_builder->add_procedure_relocation(
        meta_inst->func->symbol_ref, curr_offset - 4, 4);

    return inst_len;
}

void X86_64AsmBuilder::encode_function_ending(SIRInstRef func_ref)
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

void X86_64AsmBuilder::encode_mnem(
    Mnem mnem, const MetaValue &source, const MetaValue &dest)
{
    ZoneScoped;

    OperandKind source_opkind = OPERAND_KINDS[source.kind];
    OperandKind dest_opkind = OPERAND_KINDS[dest.kind];

    if (source_opkind == OperandKind_Memory &&
        dest_opkind == OperandKind_Memory) {
        int64_t encoding1 =
            ENCODING_ENTRIES[mnem][OperandKind_Reg][dest.size_class]
                            [OperandKind_Memory][source.size_class];
        int64_t encoding2 =
            ENCODING_ENTRIES[Mnem_MOV][OperandKind_Memory][dest.size_class]
                            [OperandKind_Reg][dest.size_class];
        this->encode(encoding1, FE_AX, source.into_operand());
        source.add_relocation(this);
        this->encode(encoding2, dest.into_operand(), FE_AX);
        dest.add_relocation(this);

    } else {
        int64_t encoding = ENCODING_ENTRIES[mnem][dest_opkind][dest.size_class]
                                           [source_opkind][source.size_class];
        this->encode(encoding, dest.into_operand(), source.into_operand());
        source.add_relocation(this, dest_opkind, dest.size_class);
        dest.add_relocation(this, source_opkind, source.size_class);
    }
}

size_t
X86_64AsmBuilder::get_func_call_stack_parameters_size(SIRInstRef func_call_ref)
{
    size_t stack_parameters_size = 0;

    SIRInst func_call = SIRModuleGetInst(this->module, func_call_ref);
    SIRFunction *called_func =
        SIRModuleGetInst(this->module, func_call.func_call.func_ref).func;
    switch (called_func->calling_convention) {
    case SIRCallingConvention_SystemV: {
        uint32_t used_int_regs = 0;
        uint32_t used_float_regs = 0;

        for (uint32_t i = 0; i < called_func->param_types.len; ++i) {
            SIRType *param_type = called_func->param_types[i];

            uint32_t param_align = param_type->align_of(this->module);
            stack_parameters_size =
                SIR_ROUND_UP(param_align, stack_parameters_size);

            switch (param_type->kind) {
            case SIRTypeKind_Pointer: {
                if (used_int_regs < SIR_CARRAY_LENGTH(SYSV_INT_PARAM_REGS)) {
                    used_int_regs++;
                } else {
                    stack_parameters_size += 8;
                }
                break;
            }
            case SIRTypeKind_Int: {
                if (used_int_regs < SIR_CARRAY_LENGTH(SYSV_INT_PARAM_REGS)) {
                    used_int_regs++;
                } else {
                    stack_parameters_size += 8;
                }

                break;
            }
            case SIRTypeKind_Float: {
                if (used_float_regs <
                    SIR_CARRAY_LENGTH(SYSV_FLOAT_PARAM_REGS)) {
                    used_float_regs++;
                } else {
                    stack_parameters_size += 8;
                }
                break;
            }
            default: {
                // TODO: proper ABI handling of other parameter types
                size_t param_size = param_type->size_of(this->module);
                stack_parameters_size += param_size;
                break;
            }
            }
        }

        break;
    }
    }

    stack_parameters_size = SIR_ROUND_UP(16, stack_parameters_size);
    return stack_parameters_size;
}

void X86_64AsmBuilder::move_inst_rvalue(
    SIRInstRef inst_ref, const MetaValue &dest_value)
{
    SIRInst inst = SIRModuleGetInst(this->module, inst_ref);
    switch (inst.kind) {
    case SIRInstKind_StackSlot: {
        this->encode_mnem(Mnem_LEA, this->meta_insts[inst_ref.id], dest_value);
        break;
    }
    case SIRInstKind_Global: {
        this->encode_mnem(Mnem_LEA, this->meta_insts[inst_ref.id], dest_value);
        break;
    }
    case SIRInstKind_PtrCast: {
        this->move_inst_rvalue(inst.ptr_cast.inst_ref, dest_value);
        break;
    }
    default: {
        size_t value_size = inst.type->size_of(this->module);
        switch (value_size) {
        case 1:
        case 2:
        case 4:
        case 8: {
            this->encode_mnem(
                Mnem_MOV, this->meta_insts[inst_ref.id], dest_value);
            break;
        }
        default: {
            this->encode_memcpy(
                value_size, this->meta_insts[inst_ref.id], dest_value);
            break;
        }
        }
        break;
    }
    }
}

void X86_64AsmBuilder::encode_memcpy(
    size_t value_size, MetaValue source_value, MetaValue dest_value)
{
    SIR_ASSERT(source_value.kind == MetaValueKind_IRegisterMemory);
    SIR_ASSERT(dest_value.kind == MetaValueKind_IRegisterMemory);

    size_t value_size_left = value_size;
    while (value_size_left > 0) {
        size_t multiple = 1;
        size_t value_size_tmp = value_size_left;
        while ((value_size_tmp & 1) == 0 && multiple < 8) {
            multiple <<= 1;
            value_size_tmp >>= 1;
        }

        value_size_left -= multiple;

        source_value.size_class = SIZE_CLASSES[multiple];
        dest_value.size_class = SIZE_CLASSES[multiple];

        this->encode_mnem(Mnem_MOV, source_value, dest_value);
        source_value.regmem.offset += multiple;
        dest_value.regmem.offset += multiple;
    }
}

void X86_64AsmBuilder::generate_inst(SIRInstRef func_ref, SIRInstRef inst_ref)
{
    ZoneScoped;

    MetaFunction *meta_func = this->meta_insts[func_ref.id].func;

    SIRInst inst = SIRModuleGetInst(this->module, inst_ref);

    switch (inst.kind) {
    case SIRInstKind_Unknown: SIR_ASSERT(0); break;
    case SIRInstKind_Function: SIR_ASSERT(0); break;
    case SIRInstKind_Block: SIR_ASSERT(0); break;
    case SIRInstKind_FunctionParameter: SIR_ASSERT(0); break;
    case SIRInstKind_Global: SIR_ASSERT(0); break;
    case SIRInstKind_StackSlot: SIR_ASSERT(0); break;

    case SIRInstKind_ImmediateInt: {
        uint8_t data[8];

        size_t byte_size = inst.type->int_.bits >> 3;
        if (byte_size == 8 && inst.imm_int.u64 > UINT32_MAX) {
            *((uint64_t *)data) = inst.imm_int.u64;
            this->meta_insts[inst_ref.id] = generate_global(
                SIRGlobalFlags_Initialized | SIRGlobalFlags_ReadOnly,
                {&data[0], byte_size});
        } else {
            this->meta_insts[inst_ref.id] =
                create_imm_int_value(byte_size, inst.imm_int.u64);
        }

        break;
    }

    case SIRInstKind_ImmediateFloat: {
        uint8_t data[8];
        size_t byte_size = inst.type->float_.bits >> 3;

        switch (byte_size) {
        case 4: *((float *)data) = (float)inst.imm_float.f64; break;
        case 8: *((double *)data) = (double)inst.imm_float.f64; break;
        }

        this->meta_insts[inst_ref.id] = generate_global(
            SIRGlobalFlags_Initialized | SIRGlobalFlags_ReadOnly,
            {&data[0], byte_size});

        break;
    }

    case SIRInstKind_ImmediateBool: {
        this->meta_insts[inst_ref.id] =
            create_imm_int_value(1, inst.imm_bool.value ? 1 : 0);
        break;
    }

    case SIRInstKind_PtrCast: {
        this->meta_insts[inst_ref.id] =
            this->meta_insts[inst.ptr_cast.inst_ref.id];
        break;
    }

    case SIRInstKind_ZExt: {
        MetaValue dest_value = this->meta_insts[inst_ref.id];

        SIRType *source_type =
            SIRModuleGetInst(this->module, inst.trunc.inst_ref).type;
        SIRType *dest_type = inst.type;

        MetaValue source_ax_value = create_int_register_value(
            source_type->size_of(this->module), RegisterIndex_RAX);
        MetaValue ext_ax_value = create_int_register_value(
            dest_type->size_of(this->module), RegisterIndex_RAX);

        this->move_inst_rvalue(inst.trunc.inst_ref, source_ax_value);

        size_t source_bytes = source_type->int_.bits >> 3;
        size_t dest_bytes = dest_type->int_.bits >> 3;

        int64_t x86_inst = 0;
        switch (dest_bytes) {
        case 2: x86_inst = FE_MOVZXr16r8; break;
        case 4: {
            switch (source_bytes) {
            case 1: x86_inst = FE_MOVZXr32r8; break;
            case 2: x86_inst = FE_MOVZXr32r16; break;
            default: SIR_ASSERT(0); break;
            }
            break;
        }
        case 8: {
            switch (source_bytes) {
            case 1: x86_inst = FE_MOVZXr64r8; break;
            case 2: x86_inst = FE_MOVZXr64r16; break;
            case 4: break;
            default: SIR_ASSERT(0); break;
            }
            break;
        }
        default: SIR_ASSERT(0); break;
        }

        if (x86_inst) {
            this->encode(x86_inst, FE_AX, FE_AX);
        }

        this->encode_mnem(Mnem_MOV, ext_ax_value, dest_value);
        break;
    }

    case SIRInstKind_SExt: {
        MetaValue dest_value = this->meta_insts[inst_ref.id];

        SIRType *source_type =
            SIRModuleGetInst(this->module, inst.trunc.inst_ref).type;
        SIRType *dest_type = inst.type;

        MetaValue source_ax_value = create_int_register_value(
            source_type->size_of(this->module), RegisterIndex_RAX);
        MetaValue ext_ax_value = create_int_register_value(
            dest_type->size_of(this->module), RegisterIndex_RAX);

        this->move_inst_rvalue(inst.trunc.inst_ref, source_ax_value);

        size_t source_bytes = source_type->int_.bits >> 3;
        size_t dest_bytes = dest_type->int_.bits >> 3;

        int64_t x86_inst = 0;
        switch (dest_bytes) {
        case 2: x86_inst = FE_MOVSXr16r8; break;
        case 4: {
            switch (source_bytes) {
            case 1: x86_inst = FE_MOVSXr32r8; break;
            case 2: x86_inst = FE_MOVSXr32r16; break;
            default: SIR_ASSERT(0); break;
            }
            break;
        }
        case 8: {
            switch (source_bytes) {
            case 1: x86_inst = FE_MOVSXr64r8; break;
            case 2: x86_inst = FE_MOVSXr64r16; break;
            case 4: x86_inst = FE_MOVSXr64r32; break;
            default: SIR_ASSERT(0); break;
            }
            break;
        }
        default: SIR_ASSERT(0); break;
        }

        this->encode(x86_inst, FE_AX, FE_AX);
        this->encode_mnem(Mnem_MOV, ext_ax_value, dest_value);
        break;
    }

    case SIRInstKind_Trunc: {
        MetaValue dest_value = this->meta_insts[inst_ref.id];

        SIRType *source_type =
            SIRModuleGetInst(this->module, inst.trunc.inst_ref).type;
        SIRType *dest_type = inst.type;

        MetaValue source_ax_value = create_int_register_value(
            source_type->size_of(this->module), RegisterIndex_RAX);

        MetaValue trunc_ax_value = create_int_register_value(
            dest_type->size_of(this->module), RegisterIndex_RAX);
        this->move_inst_rvalue(inst.trunc.inst_ref, source_ax_value);
        this->encode_mnem(Mnem_MOV, trunc_ax_value, dest_value);

        break;
    }

    case SIRInstKind_Binop: {
        MetaValue dest_value = this->meta_insts[inst_ref.id];

        size_t size = inst.type->size_of(this->module);
        size_t operand_size =
            SIRModuleGetInst(this->module, inst.binop.left_ref)
                .type->size_of(this->module);

        switch (inst.binop.op) {
        case SIRBinaryOperation_Unknown:
        case SIRBinaryOperation_MAX: SIR_ASSERT(0); break;

        case SIRBinaryOperation_IAdd: {
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
            default: SIR_ASSERT(0); break;
            }

            this->move_inst_rvalue(inst.binop.left_ref, ax_value);
            this->move_inst_rvalue(inst.binop.right_ref, dx_value);

            this->encode(x86_inst, FE_AX, FE_DX);
            this->encode_mnem(Mnem_MOV, ax_value, dest_value);

            break;
        }

        case SIRBinaryOperation_ISub: {
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
            default: SIR_ASSERT(0); break;
            }

            this->move_inst_rvalue(inst.binop.left_ref, ax_value);
            this->move_inst_rvalue(inst.binop.right_ref, dx_value);

            this->encode(x86_inst, FE_AX, FE_DX);
            this->encode_mnem(Mnem_MOV, ax_value, dest_value);

            break;
        }

        case SIRBinaryOperation_IMul: {
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
            default: SIR_ASSERT(0); break;
            }

            this->move_inst_rvalue(inst.binop.left_ref, ax_value);
            this->move_inst_rvalue(inst.binop.right_ref, dx_value);

            if (size == 1) {
                this->encode(x86_inst, FE_DX);
                this->encode_mnem(Mnem_MOV, dx_value, dest_value);
            } else {
                this->encode(x86_inst, FE_AX, FE_DX);
                this->encode_mnem(Mnem_MOV, ax_value, dest_value);
            }

            break;
        }

        case SIRBinaryOperation_SDiv: {
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
            default: SIR_ASSERT(0); break;
            }

            int64_t sep_inst = 0;
            switch (operand_size) {
            case 1:
            case 2:
            case 4: sep_inst = FE_C_SEP32; break;
            case 8: sep_inst = FE_C_SEP64; break;
            default: SIR_ASSERT(0); break;
            }
            this->encode(sep_inst, FE_AX);

            int64_t div_inst = 0;
            switch (operand_size) {
            case 1:
            case 2:
            case 4: div_inst = FE_IDIV32r; break;
            case 8: div_inst = FE_IDIV64r; break;
            default: SIR_ASSERT(0); break;
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
            default: SIR_ASSERT(0); break;
            }

            this->encode(div_inst, FE_CX);

            this->encode_mnem(Mnem_MOV, ax_value, dest_value);
            break;
        }

        case SIRBinaryOperation_UDiv: {
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
            default: SIR_ASSERT(0); break;
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
            default: SIR_ASSERT(0); break;
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
            default: SIR_ASSERT(0); break;
            }

            this->encode(div_inst, FE_CX);

            this->encode_mnem(Mnem_MOV, ax_value, dest_value);
            break;
        }

        case SIRBinaryOperation_SRem: {
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
            default: SIR_ASSERT(0); break;
            }

            int64_t sep_inst = 0;
            switch (operand_size) {
            case 1:
            case 2:
            case 4: sep_inst = FE_C_SEP32; break;
            case 8: sep_inst = FE_C_SEP64; break;
            default: SIR_ASSERT(0); break;
            }
            this->encode(sep_inst, FE_AX);

            int64_t div_inst = 0;
            switch (operand_size) {
            case 1:
            case 2:
            case 4: div_inst = FE_IDIV32r; break;
            case 8: div_inst = FE_IDIV64r; break;
            default: SIR_ASSERT(0); break;
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
            default: SIR_ASSERT(0); break;
            }

            this->encode(div_inst, FE_CX);

            this->encode_mnem(Mnem_MOV, dx_value, dest_value);
            break;
        }

        case SIRBinaryOperation_URem: {
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
            default: SIR_ASSERT(0); break;
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
            default: SIR_ASSERT(0); break;
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
            default: SIR_ASSERT(0); break;
            }

            this->encode(div_inst, FE_CX);

            this->encode_mnem(Mnem_MOV, dx_value, dest_value);
            break;
        }

        case SIRBinaryOperation_FAdd: {
            SIR_ASSERT(!"unimplemented");
            break;
        }

        case SIRBinaryOperation_FSub: {
            SIR_ASSERT(!"unimplemented");
            break;
        }

        case SIRBinaryOperation_FMul: {
            SIR_ASSERT(!"unimplemented");
            break;
        }

        case SIRBinaryOperation_FDiv: {
            SIR_ASSERT(!"unimplemented");
            break;
        }

        case SIRBinaryOperation_FRem: {
            SIR_ASSERT(!"unimplemented");
            break;
        }

        case SIRBinaryOperation_IEQ:
        case SIRBinaryOperation_INE:
        case SIRBinaryOperation_UGT:
        case SIRBinaryOperation_UGE:
        case SIRBinaryOperation_ULT:
        case SIRBinaryOperation_ULE:
        case SIRBinaryOperation_SGT:
        case SIRBinaryOperation_SGE:
        case SIRBinaryOperation_SLT:
        case SIRBinaryOperation_SLE: {
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
            default: SIR_ASSERT(0); break;
            }

            this->encode(x86_inst, FE_AX, FE_DX);

            switch (inst.binop.op) {
            case SIRBinaryOperation_IEQ:
                this->encode(FE_SETZ8r, FE_AX); // sete
                break;
            case SIRBinaryOperation_INE:
                this->encode(FE_SETNZ8r, FE_AX); // setne
                break;
            case SIRBinaryOperation_UGT:
                this->encode(FE_SETA8r, FE_AX); // seta
                break;
            case SIRBinaryOperation_UGE:
                this->encode(FE_SETNC8r, FE_AX); // setae
                break;
            case SIRBinaryOperation_ULT:
                this->encode(FE_SETC8r, FE_AX); // setb
                break;
            case SIRBinaryOperation_ULE:
                this->encode(FE_SETBE8r, FE_AX); // setbe
                break;
            case SIRBinaryOperation_SGT:
                this->encode(FE_SETG8r, FE_AX); // setg
                break;
            case SIRBinaryOperation_SGE:
                this->encode(FE_SETGE8r, FE_AX); // setge
                break;
            case SIRBinaryOperation_SLT:
                this->encode(FE_SETL8r, FE_AX); // setl
                break;
            case SIRBinaryOperation_SLE:
                this->encode(FE_SETLE8r, FE_AX); // setle
                break;
            default: SIR_ASSERT(0); break;
            }

            MetaValue al_value =
                create_int_register_value(1, RegisterIndex_RAX);
            this->encode(FE_AND8ri, FE_AX, 1);
            this->encode_mnem(Mnem_MOV, al_value, dest_value);
            break;
        }

        case SIRBinaryOperation_FEQ:
        case SIRBinaryOperation_FNE:
        case SIRBinaryOperation_FGT:
        case SIRBinaryOperation_FGE:
        case SIRBinaryOperation_FLT:
        case SIRBinaryOperation_FLE: {
            SIR_ASSERT(!"unimplemented");
            break;
        }

        case SIRBinaryOperation_And:
        case SIRBinaryOperation_Or:
        case SIRBinaryOperation_Xor: {
            MetaValue ax_value =
                create_int_register_value(operand_size, RegisterIndex_RAX);
            MetaValue dx_value =
                create_int_register_value(operand_size, RegisterIndex_RDX);

            this->move_inst_rvalue(inst.binop.left_ref, ax_value);
            this->move_inst_rvalue(inst.binop.right_ref, dx_value);

            int64_t x86_inst = 0;

            switch (inst.binop.op) {
            case SIRBinaryOperation_And: {
                switch (operand_size) {
                case 1: x86_inst = FE_AND8rr; break;
                case 2: x86_inst = FE_AND16rr; break;
                case 4: x86_inst = FE_AND32rr; break;
                case 8: x86_inst = FE_AND64rr; break;
                default: SIR_ASSERT(0); break;
                }
                break;
            }
            case SIRBinaryOperation_Or: {
                switch (operand_size) {
                case 1: x86_inst = FE_OR8rr; break;
                case 2: x86_inst = FE_OR16rr; break;
                case 4: x86_inst = FE_OR32rr; break;
                case 8: x86_inst = FE_OR64rr; break;
                default: SIR_ASSERT(0); break;
                }
                break;
            }
            case SIRBinaryOperation_Xor: {
                switch (operand_size) {
                case 1: x86_inst = FE_XOR8rr; break;
                case 2: x86_inst = FE_XOR16rr; break;
                case 4: x86_inst = FE_XOR32rr; break;
                case 8: x86_inst = FE_XOR64rr; break;
                default: SIR_ASSERT(0); break;
                }
                break;
            }
            default: SIR_ASSERT(0); break;
            }

            this->encode(x86_inst, FE_AX, FE_DX);
            this->encode_mnem(Mnem_MOV, ax_value, dest_value);
            break;
            break;
        }

        case SIRBinaryOperation_Shl:
        case SIRBinaryOperation_AShr:
        case SIRBinaryOperation_LShr: {
            MetaValue ax_value = create_int_register_value(
                SIRModuleGetInst(this->module, inst.binop.left_ref)
                    .type->size_of(this->module),
                RegisterIndex_RAX);
            MetaValue cx_value = create_int_register_value(
                SIRModuleGetInst(this->module, inst.binop.right_ref)
                    .type->size_of(this->module),
                RegisterIndex_RCX);

            this->move_inst_rvalue(inst.binop.left_ref, ax_value);
            this->move_inst_rvalue(inst.binop.right_ref, cx_value);

            int64_t x86_inst = 0;

            switch (inst.binop.op) {
            case SIRBinaryOperation_Shl: {
                switch (operand_size) {
                case 1: x86_inst = FE_SHL8rr; break;
                case 2: x86_inst = FE_SHL16rr; break;
                case 4: x86_inst = FE_SHL32rr; break;
                case 8: x86_inst = FE_SHL64rr; break;
                default: SIR_ASSERT(0); break;
                }
                break;
            }
            case SIRBinaryOperation_AShr: {
                switch (operand_size) {
                case 1: x86_inst = FE_SAR8rr; break;
                case 2: x86_inst = FE_SAR16rr; break;
                case 4: x86_inst = FE_SAR32rr; break;
                case 8: x86_inst = FE_SAR64rr; break;
                default: SIR_ASSERT(0); break;
                }
                break;
            }
            case SIRBinaryOperation_LShr: {
                switch (operand_size) {
                case 1: x86_inst = FE_SHR8rr; break;
                case 2: x86_inst = FE_SHR16rr; break;
                case 4: x86_inst = FE_SHR32rr; break;
                case 8: x86_inst = FE_SHR64rr; break;
                default: SIR_ASSERT(0); break;
                }
                break;
            }
            default: SIR_ASSERT(0); break;
            }

            this->encode(x86_inst, FE_AX, FE_CX);
            this->encode_mnem(Mnem_MOV, ax_value, dest_value);
            break;
        }
        }
        break;
    }

    case SIRInstKind_ArrayElemPtr: {
        MetaValue ptr_value = create_int_register_value(8, RegisterIndex_RAX);
        this->move_inst_rvalue(inst.array_elem_ptr.accessed_ref, ptr_value);

        size_t index_size =
            SIRModuleGetInst(this->module, inst.array_elem_ptr.index_ref)
                .type->size_of(this->module);

        MetaValue index_value =
            create_int_register_value(index_size, RegisterIndex_RCX);
        this->encode_mnem(
            Mnem_MOV,
            this->meta_insts[inst.array_elem_ptr.index_ref.id],
            index_value);

        int32_t scale = inst.type->pointer.sub->size_of(this->module);

        MetaValue value_addr = create_int_register_memory_value(
            8, RegisterIndex_RAX, scale, RegisterIndex_RCX, 0);

        this->encode_mnem(Mnem_LEA, value_addr, this->meta_insts[inst_ref.id]);

        break;
    }

    case SIRInstKind_StructElemPtr: {
        uint32_t field_index = inst.struct_elem_ptr.field_index;
        SIRType *struct_type =
            SIRModuleGetInst(this->module, inst.struct_elem_ptr.accessed_ref)
                .type->pointer.sub;

        uint32_t field_offset = 0;
        for (uint32_t i = 0; i <= field_index; ++i) {
            SIRType *field_type = struct_type->struct_.fields[i];
            uint32_t field_align = field_type->align_of(module);
            field_offset =
                SIR_ROUND_UP(field_align, field_offset); // Add padding

            if (i != field_index) {
                field_offset += field_type->size_of(module);
            }
        }

        MetaValue ptr_mem_value =
            this->meta_insts[inst.struct_elem_ptr.accessed_ref.id];
        if (SIRModuleGetInst(this->module, inst.struct_elem_ptr.accessed_ref)
                .kind != SIRInstKind_StackSlot) {
            this->move_inst_rvalue(
                inst.struct_elem_ptr.accessed_ref,
                create_int_register_value(8, RegisterIndex_RAX));
            ptr_mem_value = create_int_register_memory_value(
                8, RegisterIndex_RAX, 0, RegisterIndex_None, 0);
        }

        SIR_ASSERT(ptr_mem_value.kind == MetaValueKind_IRegisterMemory);
        ptr_mem_value.regmem.offset += field_offset;

        this->encode_mnem(
            Mnem_LEA, ptr_mem_value, this->meta_insts[inst_ref.id]);
        break;
    }

    case SIRInstKind_ExtractArrayElem: {
        size_t value_size = inst.type->size_of(this->module);

        MetaValue accessed_value =
            this->meta_insts[inst.extract_array_elem.accessed_ref.id];
        SIR_ASSERT(accessed_value.kind == MetaValueKind_IRegisterMemory);
        MetaValue ptr_value = create_int_register_value(8, RegisterIndex_RAX);
        this->encode_mnem(Mnem_LEA, accessed_value, ptr_value);

        size_t index_size =
            SIRModuleGetInst(this->module, inst.extract_array_elem.index_ref)
                .type->size_of(this->module);

        MetaValue index_value =
            create_int_register_value(index_size, RegisterIndex_RCX);
        this->encode_mnem(
            Mnem_MOV,
            this->meta_insts[inst.extract_array_elem.index_ref.id],
            index_value);

        MetaValue value_addr = create_int_register_memory_value(
            value_size, RegisterIndex_RAX, value_size, RegisterIndex_RCX, 0);

        switch (value_size) {
        case 1:
        case 2:
        case 4:
        case 8: {
            this->encode_mnem(
                Mnem_MOV, value_addr, this->meta_insts[inst_ref.id]);
            break;
        }
        default: {
            this->encode_memcpy(
                value_size, value_addr, this->meta_insts[inst_ref.id]);
            break;
        }
        }
        break;
    }

    case SIRInstKind_ExtractStructElem: {
        size_t value_size = inst.type->size_of(this->module);

        uint32_t field_index = inst.extract_struct_elem.field_index;
        SIRType *struct_type =
            SIRModuleGetInst(
                this->module, inst.extract_struct_elem.accessed_ref)
                .type;

        uint32_t field_offset = 0;
        for (uint32_t i = 0; i <= field_index; ++i) {
            SIRType *field_type = struct_type->struct_.fields[i];
            uint32_t field_align = field_type->align_of(module);
            field_offset =
                SIR_ROUND_UP(field_align, field_offset); // Add padding

            if (i != field_index) {
                field_offset += field_type->size_of(module);
            }
        }

        MetaValue ptr_mem_value =
            this->meta_insts[inst.extract_struct_elem.accessed_ref.id];
        SIR_ASSERT(ptr_mem_value.kind == MetaValueKind_IRegisterMemory);
        ptr_mem_value.regmem.offset += field_offset;

        switch (value_size) {
        case 1:
        case 2:
        case 4:
        case 8: {
            ptr_mem_value.size_class = SIZE_CLASSES[value_size];
            this->encode_mnem(
                Mnem_MOV, ptr_mem_value, this->meta_insts[inst_ref.id]);
            break;
        }
        default: {
            ptr_mem_value.size_class = SizeClass_None;
            this->encode_memcpy(
                value_size, ptr_mem_value, this->meta_insts[inst_ref.id]);
            break;
        }
        }

        break;
    }

    case SIRInstKind_Store: {
        size_t value_size = SIRModuleGetInst(this->module, inst.store.value_ref)
                                .type->size_of(this->module);

        SIRInst ptr_inst = SIRModuleGetInst(this->module, inst.store.ptr_ref);

        switch (value_size) {
        case 1:
        case 2:
        case 4:
        case 8: {
            MetaValue stored_value =
                create_int_register_value(value_size, RegisterIndex_RAX);
            this->move_inst_rvalue(inst.store.value_ref, stored_value);

            MetaValue ptr_value = {};
            switch (ptr_inst.kind) {
            case SIRInstKind_Global:
            case SIRInstKind_StackSlot: {
                ptr_value = this->meta_insts[inst.store.ptr_ref.id];
                break;
            }
            default: {
                ptr_value = create_int_register_value(8, RegisterIndex_RCX);
                this->move_inst_rvalue(inst.store.ptr_ref, ptr_value);
                ptr_value = create_int_register_memory_value(
                    value_size, ptr_value.reg.index, 0, RegisterIndex_None, 0);
                break;
            }
            }

            this->encode_mnem(Mnem_MOV, stored_value, ptr_value);
            break;
        }
        default: {
            size_t multiple = 1;
            size_t value_size_tmp = value_size;
            while ((value_size_tmp & 1) == 0 && multiple < 8) {
                multiple <<= 1;
                value_size_tmp >>= 1;
            }

            MetaValue source_ptr_mem_value =
                this->meta_insts[inst.store.value_ref.id];
            MetaValue dest_ptr_mem_value =
                this->meta_insts[inst.store.ptr_ref.id];

            if (source_ptr_mem_value.kind != MetaValueKind_IRegisterMemory) {
                MetaValue source_ptr_value =
                    create_int_register_value(8, RegisterIndex_RDX);
                this->encode_mnem(
                    Mnem_LEA,
                    this->meta_insts[inst.store.value_ref.id],
                    source_ptr_value);
                source_ptr_mem_value = create_int_register_memory_value(
                    0, RegisterIndex_RDX, 0, RegisterIndex_None, 0);
            }

            if (dest_ptr_mem_value.kind != MetaValueKind_IRegisterMemory) {
                MetaValue dest_ptr_value =
                    create_int_register_value(8, RegisterIndex_RCX);
                this->encode_mnem(
                    Mnem_LEA,
                    this->meta_insts[inst.store.ptr_ref.id],
                    dest_ptr_value);
                dest_ptr_mem_value = create_int_register_memory_value(
                    0, RegisterIndex_RCX, 0, RegisterIndex_None, 0);
            }

            this->encode_memcpy(
                value_size, source_ptr_mem_value, dest_ptr_mem_value);

            break;
        }
        }

        break;
    }

    case SIRInstKind_Load: {
        size_t value_size = inst.type->size_of(this->module);
        switch (value_size) {
        case 1:
        case 2:
        case 4:
        case 8: {
            SIRInst ptr_inst =
                SIRModuleGetInst(this->module, inst.load.ptr_ref);

            MetaValue ptr_value = {};
            switch (ptr_inst.kind) {
            case SIRInstKind_Global:
            case SIRInstKind_StackSlot: {
                ptr_value = this->meta_insts[inst.store.ptr_ref.id];
                break;
            }
            default: {
                ptr_value = create_int_register_value(8, RegisterIndex_RAX);
                this->move_inst_rvalue(inst.store.ptr_ref, ptr_value);
                ptr_value = create_int_register_memory_value(
                    value_size, ptr_value.reg.index, 0, RegisterIndex_None, 0);
                break;
            }
            }

            this->encode_mnem(
                Mnem_MOV, ptr_value, this->meta_insts[inst_ref.id]);
            break;
        }
        default: {

            MetaValue source_ptr_mem_value =
                this->meta_insts[inst.load.ptr_ref.id];
            if (source_ptr_mem_value.kind != MetaValueKind_IRegisterMemory) {
                MetaValue source_ptr_value =
                    create_int_register_value(8, RegisterIndex_RDX);
                this->encode_mnem(
                    Mnem_LEA,
                    this->meta_insts[inst.load.ptr_ref.id],
                    source_ptr_value);
                source_ptr_mem_value = create_int_register_memory_value(
                    0, RegisterIndex_RDX, 0, RegisterIndex_None, 0);
            }

            MetaValue dest_ptr_mem_value = this->meta_insts[inst_ref.id];
            if (dest_ptr_mem_value.kind != MetaValueKind_IRegisterMemory) {
                MetaValue dest_ptr_value =
                    create_int_register_value(8, RegisterIndex_RCX);
                this->encode_mnem(
                    Mnem_LEA, this->meta_insts[inst_ref.id], dest_ptr_value);
                dest_ptr_mem_value = create_int_register_memory_value(
                    0, RegisterIndex_RCX, 0, RegisterIndex_None, 0);
            }

            this->encode_memcpy(
                value_size, source_ptr_mem_value, dest_ptr_mem_value);

            break;
        }
        }

        break;
    }

    case SIRInstKind_FuncCall: {
        SIRFunction *called_func =
            SIRModuleGetInst(this->module, inst.func_call.func_ref).func;

        // TODO: save and restore caller-saved registers

        SIR_ASSERT(
            called_func->param_types.len <= inst.func_call.parameters.len);

        switch (called_func->calling_convention) {
        case SIRCallingConvention_SystemV: {
            uint32_t used_int_regs = 0;
            uint32_t used_float_regs = 0;

            size_t param_stack_offset = 0;

            // Write parameters to ABI locations
            for (size_t i = 0; i < inst.func_call.parameters.len; ++i) {
                SIRInstRef param_inst_ref = inst.func_call.parameters[i];
                SIR_ASSERT(param_inst_ref.id > 0);

                SIRInst param_inst =
                    SIRModuleGetInst(this->module, param_inst_ref);

                if (i < called_func->param_types.len) {
                    SIRType *param_type = called_func->param_types[i];
                    SIR_ASSERT(param_type == param_inst.type);
                }

                MetaValue dest_param_value = {};

                SIRType *param_type = param_inst.type;
                uint32_t param_align = param_type->align_of(this->module);

                switch (param_type->kind) {
                case SIRTypeKind_Pointer: {
                    if (used_int_regs <
                        SIR_CARRAY_LENGTH(SYSV_INT_PARAM_REGS)) {
                        dest_param_value = create_int_register_value(
                            8, SYSV_INT_PARAM_REGS[used_int_regs++]);
                    } else {
                        param_stack_offset =
                            SIR_ROUND_UP(param_align, param_stack_offset);
                        dest_param_value = create_int_register_memory_value(
                            8,
                            RegisterIndex_RSP,
                            0,
                            RegisterIndex_None,
                            param_stack_offset);
                        param_stack_offset += 8;
                    }
                    break;
                }
                case SIRTypeKind_Int: {
                    if (used_int_regs <
                        SIR_CARRAY_LENGTH(SYSV_INT_PARAM_REGS)) {
                        dest_param_value = create_int_register_value(
                            param_inst.type->int_.bits >> 3,
                            SYSV_INT_PARAM_REGS[used_int_regs++]);
                    } else {
                        param_stack_offset =
                            SIR_ROUND_UP(param_align, param_stack_offset);
                        dest_param_value = create_int_register_memory_value(
                            param_inst.type->int_.bits >> 3,
                            RegisterIndex_RSP,
                            0,
                            RegisterIndex_None,
                            param_stack_offset);
                        param_stack_offset +=
                            8; // pretty sure this should always be 8
                    }
                    break;
                }
                case SIRTypeKind_Float: {
                    if (used_float_regs <
                        SIR_CARRAY_LENGTH(SYSV_FLOAT_PARAM_REGS)) {
                        dest_param_value = create_float_register_value(
                            param_inst.type->float_.bits >> 3,
                            SYSV_FLOAT_PARAM_REGS[used_float_regs++]);
                    } else {
                        param_stack_offset =
                            SIR_ROUND_UP(param_align, param_stack_offset);
                        dest_param_value = create_int_register_memory_value(
                            param_inst.type->float_.bits >> 3,
                            RegisterIndex_RSP,
                            0,
                            RegisterIndex_None,
                            param_stack_offset);
                        param_stack_offset +=
                            8; // pretty sure this should always be 8
                    }
                    break;
                }
                default: {
                    size_t param_size = param_type->size_of(this->module);

                    param_stack_offset =
                        SIR_ROUND_UP(param_align, param_stack_offset);
                    dest_param_value = create_int_register_memory_value(
                        param_size,
                        RegisterIndex_RSP,
                        0,
                        RegisterIndex_None,
                        param_stack_offset);
                    param_stack_offset +=
                        param_size; // TODO: not sure if this should have
                                    // alignment added to it
                    break;
                }
                }

                this->move_inst_rvalue(param_inst_ref, dest_param_value);
            }

            if (called_func->variadic) {
                // Write the amount of vector registers to %AL
                MetaValue float_count_imm =
                    create_imm_int_value(1, used_float_regs);
                MetaValue float_count_reg =
                    create_int_register_value(1, RegisterIndex_RAX);
                this->encode_mnem(Mnem_MOV, float_count_imm, float_count_reg);
            }

            break;
        }
        }

        this->encode_direct_call(inst.func_call.func_ref);

        // Move returned values to result location

        switch (called_func->calling_convention) {
        case SIRCallingConvention_SystemV: {
            switch (called_func->return_type->kind) {
            case SIRTypeKind_Void: break;

            case SIRTypeKind_Int: {
                MetaValue returned_value = create_int_register_value(
                    called_func->return_type->int_.bits >> 3,
                    RegisterIndex_RAX);
                this->encode_mnem(
                    Mnem_MOV, returned_value, this->meta_insts[inst_ref.id]);
                break;
            }

            case SIRTypeKind_Pointer: {
                MetaValue returned_value =
                    create_int_register_value(8, RegisterIndex_RAX);
                this->encode_mnem(
                    Mnem_MOV, returned_value, this->meta_insts[inst_ref.id]);
                break;
            }

            case SIRTypeKind_Float: {
                MetaValue returned_value = create_float_register_value(
                    called_func->return_type->float_.bits >> 3,
                    RegisterIndex_XMM0);
                this->encode_mnem(
                    Mnem_MOV, returned_value, this->meta_insts[inst_ref.id]);
                break;
            }

            default: SIR_ASSERT(!"unhandled return type");
            }
            break;
        }
        }

        break;
    }

    case SIRInstKind_ReturnVoid: {
        this->encode_function_ending(func_ref);
        break;
    }

    case SIRInstKind_ReturnValue: {
        SIRInst returned_inst = SIRModuleGetInst(this->module, inst.return_value.inst_ref);

        // TODO: ABI compliance
        MetaValue result_location = {};
        switch (returned_inst.type->kind) {
        case SIRTypeKind_Int: {
            result_location = create_int_register_value(
                returned_inst.type->int_.bits >> 3, RegisterIndex_RAX);
            break;
        }
        case SIRTypeKind_Pointer: {
            result_location = create_int_register_value(8, RegisterIndex_RAX);
            break;
        }
        case SIRTypeKind_Float: {
            result_location =
                create_float_register_value(8, RegisterIndex_XMM0);
            break;
        }
        default: SIR_ASSERT(0); break;
        }

        this->encode_mnem(
            Mnem_MOV,
            this->meta_insts[inst.return_value.inst_ref.id],
            result_location);

        this->encode_function_ending(func_ref);
        break;
    }

    case SIRInstKind_Jump: {
        FuncJumpPatch patch = {
            .instruction = FE_JMP | FE_JMPL,
            .instruction_offset = this->get_code_offset(),
            .destination_block = inst.jump.block_ref,
        };
        meta_func->jump_patches.push_back(patch);

        this->encode(FE_JMP | FE_JMPL, -patch.instruction_offset);

        break;
    }
    case SIRInstKind_Branch: {
        MetaValue al_value = create_int_register_value(1, RegisterIndex_RAX);

        this->encode_mnem(
            Mnem_MOV, this->meta_insts[inst.branch.cond_inst_ref.id], al_value);

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

MetaValue
X86_64AsmBuilder::generate_global(uint32_t flags, SIRSlice<uint8_t> data)
{
    ZoneScoped;

    SIRSectionType section_type = SIRSectionType_Data;
    if (!(flags & SIRGlobalFlags_Initialized)) {
        section_type = SIRSectionType_BSS;
    } else {
        if (flags & SIRGlobalFlags_ReadOnly) {
            section_type = SIRSectionType_ROData;
        } else {
            section_type = SIRSectionType_Data;
        }
    }

    size_t offset = this->obj_builder->get_section_size(section_type);
    this->obj_builder->add_to_section(section_type, data);

    return create_global_value(data.len, section_type, offset);
}

void X86_64AsmBuilder::generate_function(SIRInstRef func_ref)
{
    ZoneScoped;

    SIRInst func_inst = SIRModuleGetInst(this->module, func_ref);
    SIR_ASSERT(func_inst.kind == SIRInstKind_Function);
    SIRFunction *func = func_inst.func;
    SIR_ASSERT(func);

    MetaFunction *meta_func = this->module->arena->alloc<MetaFunction>();
    *meta_func = {};

    meta_func->jump_patches = SIRArray<FuncJumpPatch>::create(module->arena);
    meta_func->temp_int_register_stack =
        SIRArray<RegisterIndex>::create(module->arena);

    switch (func->calling_convention) {
    case SIRCallingConvention_SystemV: {
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
            func->name,
            SIRSectionType_None,
            SIRSymbolType_None,
            SIRLinkage_External);
    } else {
        // Function with body
        meta_func->symbol_ref = obj_builder->add_symbol(
            func->name,
            SIRSectionType_Text,
            SIRSymbolType_Function,
            SIRLinkage_External);
    }

    MetaValue *func_meta_inst = &this->meta_insts[func_ref.id];
    *func_meta_inst = {};
    func_meta_inst->kind = MetaValueKind_Function;
    func_meta_inst->func = meta_func;

    if (func->blocks.len == 0) {
        return;
    }

    for (SIRInstRef stack_slot_ref : func->stack_slots) {
        SIRInst stack_slot = SIRModuleGetInst(this->module, stack_slot_ref);
        SIR_ASSERT(stack_slot.type->kind == SIRTypeKind_Pointer);
        uint32_t slot_size =
            stack_slot.type->pointer.sub->size_of(this->module);
        uint32_t slot_align =
            stack_slot.type->pointer.sub->align_of(this->module);
        meta_func->stack_size = SIR_ROUND_UP(slot_align, meta_func->stack_size);
        meta_func->stack_size += slot_size;

        this->meta_insts[stack_slot_ref.id] =
            create_stack_value(slot_size, -((int32_t)meta_func->stack_size));
    }

    // Create stack space for function parameters
    for (SIRInstRef param_inst_ref : func->param_insts) {
        SIRInst param_inst = SIRModuleGetInst(this->module, param_inst_ref);

        uint32_t inst_size = param_inst.type->size_of(this->module);
        uint32_t inst_align = param_inst.type->align_of(this->module);

        meta_func->stack_size = SIR_ROUND_UP(inst_align, meta_func->stack_size);
        meta_func->stack_size += inst_size;

        this->meta_insts[param_inst_ref.id] =
            create_stack_value(inst_size, -((int32_t)meta_func->stack_size));
    }

    size_t stack_params_size = 0;

    // Spill all elegible insts to stack
    for (SIRInstRef block_ref : func->blocks) {
        SIRInst block = SIRModuleGetInst(this->module, block_ref);
        for (SIRInstRef inst_ref : block.block.inst_refs) {
            SIRInst inst = SIRModuleGetInst(this->module, inst_ref);

            switch (inst.kind) {
            // Invalid for this stage of generation:
            case SIRInstKind_Unknown:
            case SIRInstKind_Global:
            case SIRInstKind_StackSlot:
            case SIRInstKind_Block:
            case SIRInstKind_Function:
            case SIRInstKind_FunctionParameter: SIR_ASSERT(0); break;

            // Contain no data for spilling:
            case SIRInstKind_Store:
            case SIRInstKind_Jump:
            case SIRInstKind_Branch:
            case SIRInstKind_ReturnVoid:
            case SIRInstKind_ReturnValue: break;

            // Data already stored somewhere else:
            case SIRInstKind_ImmediateInt:
            case SIRInstKind_ImmediateFloat:
            case SIRInstKind_ImmediateBool:
            case SIRInstKind_PtrCast: break;

            // Create stack space for these kinds:
            case SIRInstKind_ZExt:
            case SIRInstKind_SExt:
            case SIRInstKind_Trunc:
            case SIRInstKind_Binop:
            case SIRInstKind_ArrayElemPtr:
            case SIRInstKind_StructElemPtr:
            case SIRInstKind_ExtractArrayElem:
            case SIRInstKind_ExtractStructElem:
            case SIRInstKind_Load:
            case SIRInstKind_FuncCall: {
                uint32_t inst_size = inst.type->size_of(this->module);
                uint32_t inst_align = inst.type->align_of(this->module);

                meta_func->stack_size =
                    SIR_ROUND_UP(inst_align, meta_func->stack_size);
                meta_func->stack_size += inst_size;

                this->meta_insts[inst_ref.id] = create_stack_value(
                    inst_size, -((int32_t)meta_func->stack_size));

                if (inst.kind == SIRInstKind_FuncCall) {
                    size_t func_stack_params_size =
                        this->get_func_call_stack_parameters_size(inst_ref);
                    if (stack_params_size < func_stack_params_size) {
                        stack_params_size = func_stack_params_size;
                    }
                }

                break;
            }
            }
        }
    }

    // Reserve stack space for function call parameters
    meta_func->stack_size += stack_params_size;

    // TODO: Reserve saved register space

    for (uint32_t i = 0; i < RegisterIndex_COUNT; ++i) {
        if (meta_func->registers_used[i]) {
            meta_func->stack_size = SIR_ROUND_UP(8, meta_func->stack_size);
            meta_func->stack_size += 8;

            meta_func->saved_register_stack_offset[i] =
                -((int32_t)meta_func->stack_size);
        }
    }

    size_t function_start_offset =
        this->obj_builder->get_section_size(SIRSectionType_Text);

    // Begin stack frame
    meta_func->stack_size = SIR_ROUND_UP(0x10, meta_func->stack_size);
    this->encode(FE_PUSHr, FE_BP);
    this->encode(FE_MOV64rr, FE_BP, FE_SP);
    this->encode(FE_SUB64ri, FE_SP, meta_func->stack_size);

    // Move parameters to stack
    switch (func->calling_convention) {
    case SIRCallingConvention_SystemV: {
        uint32_t used_int_regs = 0;
        uint32_t used_float_regs = 0;

        uint32_t stack_param_offset = 16;

        for (uint32_t i = 0; i < func->param_types.len; ++i) {
            SIRType *param_type = func->param_types[i];
            SIRInstRef param_inst_ref = func->param_insts[i];
            MetaValue param_meta_inst = this->meta_insts[param_inst_ref.id];

            switch (param_type->kind) {
            case SIRTypeKind_Pointer: {
                MetaValue param_value = {};
                if (used_int_regs < SIR_CARRAY_LENGTH(SYSV_INT_PARAM_REGS)) {
                    param_value = create_int_register_value(
                        8, SYSV_INT_PARAM_REGS[used_int_regs++]);
                } else {
                    param_value = create_int_register_memory_value(
                        8,
                        RegisterIndex_RBP,
                        0,
                        RegisterIndex_None,
                        stack_param_offset);
                    stack_param_offset += 8;
                }

                this->encode_mnem(Mnem_MOV, param_value, param_meta_inst);
                break;
            }
            case SIRTypeKind_Int: {
                MetaValue param_value = {};
                if (used_int_regs < SIR_CARRAY_LENGTH(SYSV_INT_PARAM_REGS)) {
                    param_value = create_int_register_value(
                        param_type->int_.bits >> 3,
                        SYSV_INT_PARAM_REGS[used_int_regs++]);
                } else {
                    param_value = create_int_register_memory_value(
                        param_type->int_.bits >> 3,
                        RegisterIndex_RBP,
                        0,
                        RegisterIndex_None,
                        stack_param_offset);
                    stack_param_offset += 8;
                }

                this->encode_mnem(Mnem_MOV, param_value, param_meta_inst);
                break;
            }
            case SIRTypeKind_Float: {
                MetaValue param_value = {};
                if (used_float_regs <
                    SIR_CARRAY_LENGTH(SYSV_FLOAT_PARAM_REGS)) {
                    param_value = create_float_register_value(
                        param_type->float_.bits >> 3,
                        SYSV_FLOAT_PARAM_REGS[used_float_regs++]);

                } else {
                    param_value = create_int_register_memory_value(
                        param_type->float_.bits >> 3,
                        RegisterIndex_RBP,
                        0,
                        RegisterIndex_None,
                        stack_param_offset);
                    stack_param_offset += 8;
                }

                this->encode_mnem(Mnem_MOV, param_value, param_meta_inst);
                break;
            }
            default: {
                // TODO: proper ABI handling of other parameter types

                size_t param_size = param_type->size_of(this->module);
                MetaValue param_value = create_int_register_memory_value(
                    param_size,
                    RegisterIndex_RBP,
                    0,
                    RegisterIndex_None,
                    stack_param_offset);
                stack_param_offset += param_size;

                this->encode_memcpy(param_size, param_value, param_meta_inst);
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
    for (SIRInstRef block_ref : func->blocks) {
        SIRInst block = SIRModuleGetInst(this->module, block_ref);
        MetaValue *meta_block = &this->meta_insts[block_ref.id];
        *meta_block = {};

        meta_block->kind = MetaValueKind_Block;
        meta_block->block.offset = this->get_code_offset();
        for (SIRInstRef inst_ref : block.block.inst_refs) {
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
        this->obj_builder->get_section_size(SIRSectionType_Text);

    size_t function_size = function_end_offset - function_start_offset;

    obj_builder->set_symbol_region(
        meta_func->symbol_ref, function_start_offset, function_size);
}

void X86_64AsmBuilder::generate()
{
    ZoneScoped;

    // Generate globals
    for (SIRInstRef global_ref : this->module->globals) {
        SIRInst global = SIRModuleGetInst(this->module, global_ref);
        this->meta_insts[global_ref.id] =
            this->generate_global(global.global.flags, global.global.data);
    }

    // Generate functions
    for (SIRInstRef func_ref : this->module->functions) {
        this->generate_function(func_ref);
    }
}

void X86_64AsmBuilder::destroy()
{
    this->meta_insts.destroy();
}
