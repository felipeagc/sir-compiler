#include "sir_obj.hpp"
#include "sir_x64_encoder.h"

struct X64AsmBuilder;

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
    OperandKind_MemoryPtr,
    OperandKind_COUNT,
};

enum Mnem : uint8_t {
    Mnem_Unknown = 0,
    Mnem_MOV,
    Mnem_MOVSX,
    Mnem_MOVZX,
    Mnem_LEA,
    Mnem_ADD,
    Mnem_SUB,
    Mnem_MUL,
    Mnem_DIV,
    Mnem_IDIV,
    Mnem_C_SEP,
    Mnem_AND,
    Mnem_OR,
    Mnem_XOR,
    Mnem_SHL,
    Mnem_SHR,
    Mnem_SAR,
    Mnem_SSE_ADD,
    Mnem_SSE_SUB,
    Mnem_SSE_MUL,
    Mnem_SSE_DIV,
    Mnem_SSE_UCOMIS,
    Mnem_SSE_CVTS,
    Mnem_SSE_CVTSI2S,
    Mnem_SSE_CVTS2SI,
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
    MetaValueKind_IRegisterMemoryPtr,
    MetaValueKind_GlobalPtr,
    MetaValueKind_COUNT,
};

static SizeClass SIZE_CLASSES[9];
static size_t SIZE_CLASS_SIZES[SizeClass_COUNT];

static OperandKind OPERAND_KINDS[MetaValueKind_COUNT];

static int64_t ENCODING_ENTRIES0[Mnem_COUNT];
static int64_t ENCODING_ENTRIES1[Mnem_COUNT][OperandKind_COUNT]
                                [SizeClass_COUNT];
static int64_t ENCODING_ENTRIES2[Mnem_COUNT][OperandKind_COUNT][SizeClass_COUNT]
                                [OperandKind_COUNT][SizeClass_COUNT];
static int64_t ENCODING_ENTRIES3[Mnem_COUNT][OperandKind_COUNT][SizeClass_COUNT]
                                [OperandKind_COUNT][SizeClass_COUNT]
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

static RegisterIndex SYSV_INT_RETURN_REGS[2] = {
    RegisterIndex_RAX,
    RegisterIndex_RDX,
};

static RegisterIndex SYSV_FLOAT_RETURN_REGS[2] = {
    RegisterIndex_XMM0,
    RegisterIndex_XMM1,
};

static RegisterIndex SYSV_CALLEE_SAVED[] = {
    RegisterIndex_RBX,
    RegisterIndex_R12,
    RegisterIndex_R13,
    RegisterIndex_R14,
    RegisterIndex_R15,
};

static RegisterIndex SYSV_CALLER_SAVED[] = {
    RegisterIndex_RAX,
    RegisterIndex_RCX,
    RegisterIndex_RDX,
    RegisterIndex_RSI,
    RegisterIndex_RDI,
    RegisterIndex_R8,
    RegisterIndex_R9,
    RegisterIndex_R11,
};

struct MetaValue {
    union {
        struct {
            uint64_t offset;
        } block;
        MetaFunction *func;
        struct {
            uint64_t u64;
        } imm_int;
        struct {
            RegisterIndex index;
            uint8_t bytes;
        } reg;
        struct {
            int32_t scale;
            int32_t offset;
            RegisterIndex base;
            RegisterIndex index;
        } regmem;
        struct {
            int64_t offset;
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
        case MetaValueKind_IRegisterMemoryPtr:
        case MetaValueKind_IRegisterMemory:
            return FE_MEM(
                REGISTERS[this->regmem.base],
                this->regmem.scale,
                REGISTERS[this->regmem.index],
                this->regmem.offset);
        case MetaValueKind_GlobalPtr:
        case MetaValueKind_Global: return FE_MEM(FE_IP, 0, 0, 0);
        case MetaValueKind_ImmInt: return this->imm_int.u64;
        }

        return 0;
    }

    SIR_INLINE void add_relocation(
        X64AsmBuilder *builder,
        Mnem mnem,
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
    const RegisterIndex *caller_saved_registers;
    size_t caller_saved_registers_len;
    const RegisterIndex *callee_saved_registers;
    size_t callee_saved_registers_len;
    SIRSymbolRef symbol_ref{};
};

struct X64AsmBuilder {
    SIRAsmBuilder vt;
    SIRModule *module;
    SIRObjectBuilder *obj_builder;
    SIRArray<MetaValue> meta_insts;
    SIRInstRef current_block;
    SIRArray<SIRInstRef> current_func_params;
    SIRInstRef current_cond;
    SIRInstRef current_func;

    size_t get_code_offset();
    size_t encode_raw(const uint8_t *bytes, size_t len);
    size_t encode(
        uint64_t mnem, FeOp op0 = 0, FeOp op1 = 0, FeOp op2 = 0, FeOp op3 = 0);
    size_t encode_at(
        size_t offset,
        uint64_t mnem,
        FeOp op0 = 0,
        FeOp op1 = 0,
        FeOp op2 = 0,
        FeOp op3 = 0);

    void encode_mnem0(Mnem mnem);
    void encode_mnem1(Mnem mnem, const MetaValue *op1);
    void
    encode_mnem2(Mnem mnem, const MetaValue *dest, const MetaValue *source);
    void encode_mnem3(
        Mnem mnem,
        const MetaValue *op1,
        const MetaValue *op2,
        const MetaValue *op3);
    void encode_lea2(const MetaValue *dest, const MetaValue *source);

    size_t encode_direct_call(SIRInstRef func_ref);
    void encode_function_ending(SIRInstRef func_ref);

    void move_inst_rvalue(SIRInstRef inst_ref, const MetaValue *dest_value);
    void encode_memcpy(
        size_t value_size, MetaValue source_value, MetaValue dest_value);
};

SIR_INLINE void MetaValue::add_relocation(
    X64AsmBuilder *builder,
    Mnem mnem,
    OperandKind other_operand_kind,
    SizeClass other_operand_size_class) const
{
    (void)mnem;

    if (this->kind == MetaValueKind_Global ||
        this->kind == MetaValueKind_GlobalPtr) {
        size_t relocation_offset = builder->get_code_offset() - 4;
        if (other_operand_kind == OperandKind_Imm) {
            relocation_offset -= SIZE_CLASS_SIZES[other_operand_size_class];
        }

        int64_t data_offset = this->global.offset;
        /* if (mnem == Mnem_MOV) { */
        /*     data_offset -= 4; */
        /* } */

        builder->obj_builder->add_data_relocation(
            builder->obj_builder,
            this->global.section_type,
            data_offset,
            relocation_offset,
            4);
    }
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
MetaValue create_stack_ptr_value(size_t byte_size, int32_t offset)
{
    ZoneScoped;

    MetaValue value = {};
    value.kind = MetaValueKind_IRegisterMemoryPtr;
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
    return value;
}

SIR_INLINE MetaValue create_global_value(
    X64AsmBuilder *builder,
    uint32_t flags,
    const uint8_t *data,
    size_t data_len)
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

    size_t offset = builder->obj_builder->get_section_size(
        builder->obj_builder, section_type);
    builder->obj_builder->add_to_section(
        builder->obj_builder, section_type, data, data_len);

    MetaValue value = {};
    value.kind = MetaValueKind_Global;
    switch (data_len) {
    case 1:
    case 2:
    case 4:
    case 8: value.size_class = SIZE_CLASSES[data_len]; break;
    default: value.size_class = SizeClass_None; break;
    }
    value.global.offset = offset;
    value.global.section_type = section_type;
    return value;
}

SIR_INLINE MetaValue create_global_ptr_value(
    X64AsmBuilder *builder,
    uint32_t flags,
    const uint8_t *data,
    size_t data_len)
{
    ZoneScoped;
    MetaValue global = create_global_value(builder, flags, data, data_len);
    global.kind = MetaValueKind_GlobalPtr;
    return global;
}

SIR_INLINE
size_t X64AsmBuilder::get_code_offset()
{
    return this->obj_builder->get_section_size(
        this->obj_builder, SIRSectionType_Text);
}

SIR_INLINE
size_t X64AsmBuilder::encode_raw(const uint8_t *bytes, size_t len)
{
    ZoneScoped;

    this->obj_builder->add_to_section(
        this->obj_builder, SIRSectionType_Text, (uint8_t *)bytes, len);
    return len;
}

SIR_INLINE
size_t
X64AsmBuilder::encode(uint64_t mnem, FeOp op0, FeOp op1, FeOp op2, FeOp op3)
{
    ZoneScoped;

    uint8_t temp[16] = {}; // Instructions can only be at most 15 bytes, but we
                           // reserve 16 bytes for easier zero initialization
    uint8_t *ptr = &temp[0];
    int failed = fe_enc64(&ptr, mnem, op0, op1, op2, op3);
    SIR_ASSERT(!failed);

    size_t inst_len = (size_t)(ptr - temp);
    this->obj_builder->add_to_section(
        this->obj_builder, SIRSectionType_Text, &temp[0], inst_len);
    return inst_len;
}

SIR_INLINE
size_t X64AsmBuilder::encode_at(
    size_t offset, uint64_t mnem, FeOp op0, FeOp op1, FeOp op2, FeOp op3)
{
    ZoneScoped;

    uint8_t temp[16] = {}; // Instructions can only be at most 15 bytes, but we
                           // reserve 16 bytes for easier zero initialization
    uint8_t *ptr = &temp[0];
    int failed = fe_enc64(&ptr, mnem, op0, op1, op2, op3);
    SIR_ASSERT(!failed);

    size_t inst_len = (size_t)(ptr - temp);
    this->obj_builder->set_section_data(
        this->obj_builder, SIRSectionType_Text, offset, &temp[0], inst_len);
    return inst_len;
}

SIR_INLINE
size_t X64AsmBuilder::encode_direct_call(SIRInstRef func_ref)
{
    ZoneScoped;

    uint8_t inst_bytes[5] = {0xe8, 0x00, 0x00, 0x00, 0x00};
    size_t inst_len =
        this->encode_raw(inst_bytes, SIR_CARRAY_LENGTH(inst_bytes));

    size_t curr_offset = this->get_code_offset();

    MetaValue *meta_inst = &this->meta_insts[func_ref.id];

    this->obj_builder->add_procedure_relocation(
        this->obj_builder, meta_inst->func->symbol_ref, curr_offset - 4, 4);

    return inst_len;
}

void X64AsmBuilder::encode_function_ending(SIRInstRef func_ref)
{
    ZoneScoped;

    MetaFunction *meta_func = this->meta_insts[func_ref.id].func;

    // Restore callee saved registers
    for (size_t i = 0; i < meta_func->callee_saved_registers_len; ++i) {
        RegisterIndex reg_index = meta_func->callee_saved_registers[i];
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

void X64AsmBuilder::encode_mnem0(Mnem mnem)
{
    ZoneScoped;
    int64_t encoding = ENCODING_ENTRIES0[mnem];
    this->encode(encoding);
}

void X64AsmBuilder::encode_mnem1(Mnem mnem, const MetaValue *op1)
{
    ZoneScoped;

    OperandKind op1_opkind = OPERAND_KINDS[op1->kind];

    int64_t encoding = ENCODING_ENTRIES1[mnem][op1_opkind][op1->size_class];
    this->encode(encoding, op1->into_operand());
    op1->add_relocation(this, mnem, op1_opkind, op1->size_class);
}

void X64AsmBuilder::encode_mnem2(
    Mnem mnem, const MetaValue *dest, const MetaValue *source)
{
    ZoneScoped;

    OperandKind source_opkind = OPERAND_KINDS[source->kind];
    OperandKind dest_opkind = OPERAND_KINDS[dest->kind];

    if ((source_opkind == OperandKind_Memory ||
         source_opkind == OperandKind_MemoryPtr) &&
        dest_opkind == OperandKind_Memory) {
        int64_t encoding1 =
            ENCODING_ENTRIES2[mnem][OperandKind_Reg][dest->size_class]
                             [source_opkind][source->size_class];
        int64_t encoding2 =
            ENCODING_ENTRIES2[Mnem_MOV][dest_opkind][dest->size_class]
                             [OperandKind_Reg][dest->size_class];
        this->encode(encoding1, FE_AX, source->into_operand());
        source->add_relocation(this, mnem);
        this->encode(encoding2, dest->into_operand(), FE_AX);
        dest->add_relocation(this, Mnem_MOV);
    } else {
        int64_t encoding =
            ENCODING_ENTRIES2[mnem][dest_opkind][dest->size_class]
                             [source_opkind][source->size_class];
        this->encode(encoding, dest->into_operand(), source->into_operand());
        source->add_relocation(this, mnem, dest_opkind, dest->size_class);
        dest->add_relocation(this, mnem, source_opkind, source->size_class);
    }
}

void X64AsmBuilder::encode_mnem3(
    Mnem mnem, const MetaValue *op1, const MetaValue *op2, const MetaValue *op3)
{
    ZoneScoped;

    OperandKind op1_opkind = OPERAND_KINDS[op1->kind];
    OperandKind op2_opkind = OPERAND_KINDS[op2->kind];
    OperandKind op3_opkind = OPERAND_KINDS[op3->kind];

    int64_t encoding =
        ENCODING_ENTRIES3[mnem][op1_opkind][op1->size_class][op2_opkind]
                         [op2->size_class][op3_opkind][op3->size_class];
    this->encode(
        encoding,
        op1->into_operand(),
        op2->into_operand(),
        op3->into_operand());
    op1->add_relocation(this, mnem, op1_opkind, op1->size_class);
    op2->add_relocation(this, mnem, op2_opkind, op2->size_class);
    op3->add_relocation(this, mnem, op3_opkind, op3->size_class);
}

void X64AsmBuilder::encode_lea2(const MetaValue *dest, const MetaValue *source)
{
    ZoneScoped;

    OperandKind source_opkind = OPERAND_KINDS[source->kind];
    OperandKind dest_opkind = OPERAND_KINDS[dest->kind];

    if (source_opkind == OperandKind_Memory &&
        dest_opkind == OperandKind_Memory) {
        int64_t encoding1 =
            ENCODING_ENTRIES2[Mnem_LEA][OperandKind_Reg][dest->size_class]
                             [OperandKind_Memory][source->size_class];
        int64_t encoding2 =
            ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Memory][dest->size_class]
                             [OperandKind_Reg][dest->size_class];
        this->encode(encoding1, FE_AX, source->into_operand());
        source->add_relocation(this, Mnem_LEA);
        this->encode(encoding2, dest->into_operand(), FE_AX);
        dest->add_relocation(this, Mnem_MOV);
    } else {
        int64_t encoding =
            ENCODING_ENTRIES2[Mnem_LEA][dest_opkind][dest->size_class]
                             [source_opkind][source->size_class];
        this->encode(encoding, dest->into_operand(), source->into_operand());
        source->add_relocation(this, Mnem_LEA, dest_opkind, dest->size_class);
        dest->add_relocation(this, Mnem_LEA, source_opkind, source->size_class);
    }
}

enum SysVParamClass {
    SysVParamClass_Int = 0,
    SysVParamClass_SSE,
};

static void extract_type_sysv_classes(
    X64AsmBuilder *builder,
    SIRType *type,
    uint32_t *field_offset,
    uint8_t *byte_classes)
{
    ZoneScoped;

    uint32_t type_align = SIRTypeAlignOf(builder->module, type);
    uint32_t type_size = SIRTypeSizeOf(builder->module, type);

    switch (type->kind) {
    case SIRTypeKind_Void:
    case SIRTypeKind_Bool: {
        SIR_ASSERT(0);
    }
    case SIRTypeKind_Pointer:
    case SIRTypeKind_Int: {
        *field_offset = SIR_ROUND_UP(type_align, *field_offset); // Add padding
        for (size_t j = 0; j < type_size; ++j) {
            byte_classes[*field_offset + j] = SysVParamClass_Int;
        }
        *field_offset += type_size;
        break;
    }
    case SIRTypeKind_Float: {
        *field_offset = SIR_ROUND_UP(type_align, *field_offset); // Add padding
        for (size_t i = 0; i < type_size; ++i) {
            byte_classes[*field_offset + i] = SysVParamClass_SSE;
        }
        *field_offset += type_size;
        break;
    }
    case SIRTypeKind_Struct: {
        for (size_t i = 0; i < type->struct_.fields_len; ++i) {
            SIRType *field_type = type->struct_.fields[i];
            extract_type_sysv_classes(
                builder, field_type, field_offset, byte_classes);
        }
        break;
    }
    case SIRTypeKind_Array: {
        for (size_t i = 0; i < type->array.count; ++i) {
            SIRType *field_type = type->array.sub;
            extract_type_sysv_classes(
                builder, field_type, field_offset, byte_classes);
        }
        break;
    }
    }
}

static void sysv_get_type_register_classes(
    X64AsmBuilder *builder,
    SIRType *type,
    SysVParamClass *class1,
    SysVParamClass *class2)
{
    ZoneScoped;

    uint32_t type_size = SIRTypeSizeOf(builder->module, type);
    SIR_ASSERT(type_size <= 16);

    uint8_t byte_classes[16] = {};
    uint32_t field_offset = 0;
    extract_type_sysv_classes(builder, type, &field_offset, byte_classes);

    *class1 = SysVParamClass_SSE;
    for (size_t i = 0; i < 8 && i < type_size; ++i) {
        if (byte_classes[i] == SysVParamClass_Int) {
            *class1 = SysVParamClass_Int;
            break;
        }
    }

    *class2 = SysVParamClass_SSE;
    for (size_t i = 8; i < type_size; ++i) {
        if (byte_classes[i] == SysVParamClass_Int) {
            *class2 = SysVParamClass_Int;
            break;
        }
    }
}

static bool sysv_param_should_use_regs(
    X64AsmBuilder *builder,
    SIRType *param_type,
    SysVParamClass *class1,
    SysVParamClass *class2,
    uint32_t used_int_regs,
    uint32_t used_float_regs)
{
    ZoneScoped;

    bool use_regs = false;
    size_t param_size = SIRTypeSizeOf(builder->module, param_type);

    if (param_size <= 16) {
        uint32_t param_int_regs = 0;
        uint32_t param_float_regs = 0;

        sysv_get_type_register_classes(builder, param_type, class1, class2);

        switch (*class1) {
        case SysVParamClass_Int: param_int_regs += 1; break;
        case SysVParamClass_SSE: param_float_regs += 1; break;
        }

        if (param_size > 8) {
            switch (*class2) {
            case SysVParamClass_Int: param_int_regs += 1; break;
            case SysVParamClass_SSE: param_float_regs += 1; break;
            }
        }

        int32_t param_available_int_regs =
            (SIR_CARRAY_LENGTH(SYSV_INT_PARAM_REGS) - used_int_regs -
             param_int_regs);
        int32_t param_available_float_regs =
            (SIR_CARRAY_LENGTH(SYSV_FLOAT_PARAM_REGS) - used_float_regs -
             param_float_regs);

        use_regs = (param_available_int_regs >= 0) &&
                   (param_available_float_regs >= 0);
    }

    return use_regs;
}

void X64AsmBuilder::move_inst_rvalue(
    SIRInstRef inst_ref, const MetaValue *dest_value)
{
    ZoneScoped;

    SIRInst inst = SIRModuleGetInst(this->module, inst_ref);
    switch (inst.kind) {
    case SIRInstKind_StackSlot: {
        this->encode_lea2(dest_value, &this->meta_insts[inst_ref.id]);
        break;
    }
    case SIRInstKind_Global: {
        this->encode_lea2(dest_value, &this->meta_insts[inst_ref.id]);
        break;
    }
    case SIRInstKind_BitCast: {
        this->move_inst_rvalue(inst.op1, dest_value);
        break;
    }
    default: {
        size_t value_size = SIRTypeSizeOf(this->module, inst.type);
        switch (value_size) {
        case 1:
        case 2:
        case 4:
        case 8: {
            this->encode_mnem2(
                Mnem_MOV, dest_value, &this->meta_insts[inst_ref.id]);
            break;
        }
        default: {
            this->encode_memcpy(
                value_size, this->meta_insts[inst_ref.id], *dest_value);
            break;
        }
        }
        break;
    }
    }
}

void X64AsmBuilder::encode_memcpy(
    size_t value_size, MetaValue source_value, MetaValue dest_value)
{
    ZoneScoped;

    if (value_size <= 8) {
        switch (value_size) {
        case 1:
        case 2:
        case 4:
        case 8: {
            // Register-sized values
            dest_value.size_class = SIZE_CLASSES[value_size];
            source_value.size_class = SIZE_CLASSES[value_size];
            this->encode_mnem2(Mnem_MOV, &dest_value, &source_value);
            return;
        }
        default: {
            // This part moves non-register sized memory
            // in or out of registers. It follows the SystemV ABI convention
            // for storing memory in registers. I'm not sure if this is OK in
            // other ABIs.

            uint8_t part_sizes[3];
            uint8_t mem_part_offsets[3];
            int64_t part_count = 0;

            size_t value_size_left = value_size;
            while (value_size_left > 0) {
                size_t multiple = 1;
                size_t value_size_tmp = value_size_left;
                while ((value_size_tmp & 1) == 0 && multiple < 8) {
                    multiple <<= 1;
                    value_size_tmp >>= 1;
                }

                part_sizes[part_count++] = multiple;
                value_size_left -= multiple;
            }

            size_t offset_tmp = value_size;
            for (int64_t i = 0; i < part_count; ++i) {
                offset_tmp -= part_sizes[i];
                mem_part_offsets[i] = offset_tmp;
            }

            if (dest_value.kind == MetaValueKind_IRegister &&
                source_value.kind == MetaValueKind_IRegisterMemory) {

                MetaValue tmp_reg =
                    create_int_register_value(4, RegisterIndex_RCX);
                MetaValue source_mem = source_value;
                int64_t index_in_dest_reg = 0;
                int64_t index_in_tmp_reg = 0;
                for (int64_t i = 0; i < part_count; ++i) {
                    MetaValue reg_value = dest_value;
                    int64_t index_in_reg = i;

                    if (mem_part_offsets[i] >= 4) {
                        index_in_reg = index_in_tmp_reg++;
                        reg_value = tmp_reg;
                    } else {
                        index_in_reg = index_in_dest_reg++;
                    }

                    if (index_in_reg != 0) {
                        MetaValue imm_val =
                            create_imm_int_value(1, part_sizes[i] * 8);
                        reg_value.size_class = SIZE_CLASSES[part_sizes[i] * 2];
                        this->encode_mnem2(Mnem_SHL, &reg_value, &imm_val);
                    }

                    reg_value.size_class = SIZE_CLASSES[part_sizes[i]];
                    source_mem.size_class = SIZE_CLASSES[part_sizes[i]];
                    source_mem.regmem.offset =
                        source_value.regmem.offset + mem_part_offsets[i];
                    this->encode_mnem2(Mnem_MOV, &reg_value, &source_mem);
                }

                if (index_in_tmp_reg > 0) {
                    dest_value.size_class = SizeClass_8;
                    tmp_reg.size_class = SizeClass_8;

                    MetaValue imm_val = create_imm_int_value(1, 32);
                    this->encode_mnem2(Mnem_SHL, &tmp_reg, &imm_val);
                    this->encode_mnem2(Mnem_OR, &dest_value, &tmp_reg);
                }
                return;
            } else if (
                dest_value.kind == MetaValueKind_IRegisterMemory &&
                source_value.kind == MetaValueKind_IRegister) {

                MetaValue dest_mem = dest_value;
                for (int64_t i = part_count - 1; i >= 0; --i) {
                    source_value.size_class = SIZE_CLASSES[part_sizes[i]];
                    dest_mem.size_class = SIZE_CLASSES[part_sizes[i]];
                    dest_mem.regmem.offset =
                        dest_value.regmem.offset + mem_part_offsets[i];
                    this->encode_mnem2(Mnem_MOV, &dest_mem, &source_value);

                    if (i != 0) {
                        MetaValue imm_val =
                            create_imm_int_value(1, part_sizes[i] * 8);
                        source_value.size_class = SizeClass_8;
                        this->encode_mnem2(Mnem_SHR, &source_value, &imm_val);
                    }
                }
                return;
            }
            break;
        }
        }
    }

    if ((source_value.kind == MetaValueKind_IRegisterMemory ||
         source_value.kind == MetaValueKind_Global) &&
        (dest_value.kind == MetaValueKind_IRegisterMemory ||
         dest_value.kind == MetaValueKind_Global)) {
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

            this->encode_mnem2(Mnem_MOV, &dest_value, &source_value);

            if (source_value.kind == MetaValueKind_IRegisterMemory) {
                source_value.regmem.offset += multiple;
            } else {
                source_value.global.offset += multiple;
            }

            if (dest_value.kind == MetaValueKind_IRegisterMemory) {
                dest_value.regmem.offset += multiple;
            } else {
                dest_value.global.offset += multiple;
            }
        }
    } else {
        SIR_ASSERT(!"memcpy unimplemented");
    }
}

static void generate_const(X64AsmBuilder *builder, SIRInstRef inst_ref)
{
    ZoneScoped;

    SIRInst inst = SIRModuleGetInst(builder->module, inst_ref);

    switch (inst.kind) {
    case SIRInstKind_Unknown:
    case SIRInstKind_Alias:
    case SIRInstKind_Global:
    case SIRInstKind_StackSlot:
    case SIRInstKind_Block:
    case SIRInstKind_Function:
    case SIRInstKind_FunctionParameter:
    case SIRInstKind_SetCond:
    case SIRInstKind_PushFunctionParameter:
    case SIRInstKind_ReturnVoid:
    case SIRInstKind_ReturnValue:
    case SIRInstKind_Load:
    case SIRInstKind_Store:
    case SIRInstKind_Jump:
    case SIRInstKind_Branch:
    case SIRInstKind_PhiIncoming:
    case SIRInstKind_Phi:
    case SIRInstKind_FuncCall:
    case SIRInstKind_BitCast:
    case SIRInstKind_ZExt:
    case SIRInstKind_SExt:
    case SIRInstKind_Trunc:
    case SIRInstKind_FPTrunc:
    case SIRInstKind_FPExt:
    case SIRInstKind_SIToFP:
    case SIRInstKind_UIToFP:
    case SIRInstKind_FPToSI:
    case SIRInstKind_FPToUI:
    case SIRInstKind_FNeg:
    case SIRInstKind_ArrayElemPtr:
    case SIRInstKind_StructElemPtr:
    case SIRInstKind_ExtractArrayElem:
    case SIRInstKind_ExtractStructElem:
    case SIRInstKind_Binop: SIR_ASSERT(0); break;

    case SIRInstKind_ConstBool: {
        builder->meta_insts[inst_ref.id] =
            create_imm_int_value(1, inst.const_bool.value ? 1 : 0);
        break;
    }

    case SIRInstKind_ConstInt: {
        uint8_t data[8];

        size_t byte_size = inst.type->int_.bits >> 3;
        if (byte_size == 8 && inst.const_int.u64 > UINT32_MAX) {
            *((uint64_t *)data) = inst.const_int.u64;
            builder->meta_insts[inst_ref.id] = create_global_value(
                builder,
                SIRGlobalFlags_Initialized | SIRGlobalFlags_ReadOnly,
                &data[0],
                byte_size);
        } else {
            builder->meta_insts[inst_ref.id] =
                create_imm_int_value(byte_size, inst.const_int.u64);
        }

        break;
    }

    case SIRInstKind_ConstFloat: {
        uint8_t data[8];
        size_t byte_size = inst.type->float_.bits >> 3;

        switch (byte_size) {
        case 4: *((float *)data) = (float)inst.const_float.f64; break;
        case 8: *((double *)data) = (double)inst.const_float.f64; break;
        }

        builder->meta_insts[inst_ref.id] = create_global_value(
            builder,
            SIRGlobalFlags_Initialized | SIRGlobalFlags_ReadOnly,
            &data[0],
            byte_size);

        break;
    }
    }
}

static void
generate_inst(X64AsmBuilder *builder, SIRInstRef func_ref, SIRInstRef inst_ref)
{
    ZoneScoped;

    MetaFunction *meta_func = builder->meta_insts[func_ref.id].func;

    SIRInst inst = SIRModuleGetInst(builder->module, inst_ref);

    switch (inst.kind) {
    case SIRInstKind_Unknown:
    case SIRInstKind_Function:
    case SIRInstKind_Block:
    case SIRInstKind_FunctionParameter:
    case SIRInstKind_Global:
    case SIRInstKind_StackSlot:
    case SIRInstKind_ConstBool:
    case SIRInstKind_ConstInt:
    case SIRInstKind_ConstFloat: SIR_ASSERT(0); break;

    case SIRInstKind_PhiIncoming: {
        break;
    }

    case SIRInstKind_SetCond: {
        builder->current_cond = inst.op1;
        break;
    }

    case SIRInstKind_PushFunctionParameter: {
        builder->current_func_params.push_back(inst.op1);
        break;
    }

    case SIRInstKind_Alias: {
        builder->meta_insts[inst_ref.id] = builder->meta_insts[inst.op1.id];
        break;
    }

    case SIRInstKind_BitCast: {
        builder->meta_insts[inst_ref.id] = builder->meta_insts[inst.op1.id];
        break;
    }

    case SIRInstKind_ZExt: {
        MetaValue dest_value = builder->meta_insts[inst_ref.id];
        MetaValue source_value = builder->meta_insts[inst.op1.id];
        Mnem mnem = Mnem_MOVZX;

        if (source_value.kind == MetaValueKind_ImmInt) {
            source_value.size_class = dest_value.size_class;
            mnem = Mnem_MOV;
        }

        builder->encode_mnem2(mnem, &dest_value, &source_value);

        break;
    }

    case SIRInstKind_SExt: {
        MetaValue dest_value = builder->meta_insts[inst_ref.id];
        MetaValue source_value = builder->meta_insts[inst.op1.id];
        Mnem mnem = Mnem_MOVSX;

        if (source_value.kind == MetaValueKind_ImmInt) {
            source_value.size_class = dest_value.size_class;
            mnem = Mnem_MOV;
        }

        builder->encode_mnem2(mnem, &dest_value, &source_value);

        break;
    }

    case SIRInstKind_Trunc: {
        MetaValue dest_value = builder->meta_insts[inst_ref.id];

        SIRType *source_type = SIRModuleGetInst(builder->module, inst.op1).type;
        SIRType *dest_type = inst.type;

        MetaValue source_ax_value = create_int_register_value(
            SIRTypeSizeOf(builder->module, source_type), RegisterIndex_RAX);

        MetaValue trunc_ax_value = create_int_register_value(
            SIRTypeSizeOf(builder->module, dest_type), RegisterIndex_RAX);
        builder->move_inst_rvalue(inst.op1, &source_ax_value);
        builder->encode_mnem2(Mnem_MOV, &dest_value, &trunc_ax_value);

        break;
    }

    case SIRInstKind_FPExt:
    case SIRInstKind_FPTrunc: {
        MetaValue dest_value = builder->meta_insts[inst_ref.id];
        MetaValue source_value = builder->meta_insts[inst.op1.id];

        SIRType *source_type = SIRModuleGetInst(builder->module, inst.op1).type;
        SIRType *dest_type = inst.type;

        uint32_t source_size = SIRTypeSizeOf(builder->module, source_type);
        uint32_t dest_size = SIRTypeSizeOf(builder->module, dest_type);

        if (source_size != dest_size) {
            MetaValue xmm0_value = create_float_register_value(
                SIRTypeSizeOf(builder->module, dest_type), RegisterIndex_XMM0);
            builder->encode_mnem2(Mnem_SSE_CVTS, &xmm0_value, &source_value);
            builder->encode_mnem2(Mnem_MOV, &dest_value, &xmm0_value);
        } else {
            builder->encode_mnem2(Mnem_MOV, &dest_value, &source_value);
        }

        break;
    }

    case SIRInstKind_SIToFP: {
        MetaValue dest_value = builder->meta_insts[inst_ref.id];
        MetaValue source_value = builder->meta_insts[inst.op1.id];

        SIRType *source_type = SIRModuleGetInst(builder->module, inst.op1).type;

        uint32_t source_size = SIRTypeSizeOf(builder->module, source_type);
        uint32_t dest_size = SIRTypeSizeOf(builder->module, inst.type);

        MetaValue tmp_int_value = {};

        if (source_value.kind == MetaValueKind_ImmInt) {
            tmp_int_value = create_int_register_value(
                SIZE_CLASS_SIZES[source_value.size_class], RegisterIndex_RAX);
            builder->encode_mnem2(Mnem_MOV, &tmp_int_value, &source_value);
            source_value = tmp_int_value;
        }

        switch (source_size) {
        case 1:
        case 2: {
            tmp_int_value = create_int_register_value(4, RegisterIndex_RAX);
            builder->encode_mnem2(Mnem_MOVSX, &tmp_int_value, &source_value);
            break;
        }
        case 4:
        case 8: {
            tmp_int_value = source_value;
            break;
        }
        default: SIR_ASSERT(0); break;
        }

        MetaValue dest_reg_value =
            create_float_register_value(dest_size, RegisterIndex_XMM0);

        builder->encode_mnem2(
            Mnem_SSE_CVTSI2S, &dest_reg_value, &tmp_int_value);

        builder->encode_mnem2(Mnem_MOV, &dest_value, &dest_reg_value);
        break;
    }

    case SIRInstKind_UIToFP: {
        MetaValue dest_value = builder->meta_insts[inst_ref.id];
        MetaValue source_value = builder->meta_insts[inst.op1.id];

        SIRType *source_type = SIRModuleGetInst(builder->module, inst.op1).type;

        uint32_t source_size = SIRTypeSizeOf(builder->module, source_type);
        uint32_t dest_size = SIRTypeSizeOf(builder->module, inst.type);

        MetaValue tmp_int_value = {};

        if (source_value.kind == MetaValueKind_ImmInt) {
            tmp_int_value = create_int_register_value(
                SIZE_CLASS_SIZES[source_value.size_class], RegisterIndex_RAX);
            builder->encode_mnem2(Mnem_MOV, &tmp_int_value, &source_value);
            source_value = tmp_int_value;
        }

        switch (source_size) {
        case 1:
        case 2: {
            tmp_int_value = create_int_register_value(4, RegisterIndex_RAX);
            builder->encode_mnem2(Mnem_MOVZX, &tmp_int_value, &source_value);
            break;
        }
        case 4: {
            tmp_int_value = create_int_register_value(4, RegisterIndex_RAX);
            builder->encode_mnem2(Mnem_MOV, &tmp_int_value, &source_value);
            tmp_int_value = create_int_register_value(8, RegisterIndex_RAX);
            break;
        }
        case 8: {
            tmp_int_value = source_value;
            break;
        }
        default: SIR_ASSERT(0); break;
        }

        MetaValue dest_reg_value =
            create_float_register_value(dest_size, RegisterIndex_XMM0);

        builder->encode_mnem2(
            Mnem_SSE_CVTSI2S, &dest_reg_value, &tmp_int_value);

        builder->encode_mnem2(Mnem_MOV, &dest_value, &dest_reg_value);
        break;
        break;
    }

    case SIRInstKind_FPToUI:
    case SIRInstKind_FPToSI: {
        MetaValue dest_value = builder->meta_insts[inst_ref.id];
        MetaValue source_value = builder->meta_insts[inst.op1.id];

        uint32_t dest_size = SIRTypeSizeOf(builder->module, inst.type);

        MetaValue dest_reg_value =
            create_int_register_value(SIR_MAX(dest_size, 4), RegisterIndex_RAX);

        builder->encode_mnem2(Mnem_SSE_CVTS2SI, &dest_reg_value, &source_value);

        dest_reg_value =
            create_int_register_value(dest_size, RegisterIndex_RAX);

        builder->encode_mnem2(Mnem_MOV, &dest_value, &dest_reg_value);

        break;
    }

    case SIRInstKind_FNeg: {
        MetaValue dest_value = builder->meta_insts[inst_ref.id];
        MetaValue source_value = builder->meta_insts[inst.op1.id];

        uint32_t dest_size = SIRTypeSizeOf(builder->module, inst.type);

        MetaValue tmp_reg_value =
            create_int_register_value(dest_size, RegisterIndex_RAX);
        builder->encode_mnem2(Mnem_MOV, &tmp_reg_value, &source_value);

        switch (dest_size) {
        case 4: {
            builder->encode(FE_XOR32ri, FE_AX, -2147483648);
            break;
        }
        case 8: {
            builder->encode(FE_MOV64ri, FE_CX, -9223372036854775808ULL);
            builder->encode(FE_XOR64rr, FE_AX, FE_CX);
            break;
        }
        }

        builder->encode_mnem2(Mnem_MOV, &dest_value, &tmp_reg_value);
        break;
    }

    case SIRInstKind_Binop: {
        MetaValue dest_value = builder->meta_insts[inst_ref.id];

        size_t operand_size = SIRTypeSizeOf(
            builder->module, SIRModuleGetInst(builder->module, inst.op1).type);

        MetaValue left_val = builder->meta_insts[inst.op1.id];
        MetaValue right_val = builder->meta_insts[inst.op2.id];

        switch (inst.binop) {
        case SIRBinaryOperation_Unknown:
        case SIRBinaryOperation_MAX: SIR_ASSERT(0); break;

        case SIRBinaryOperation_IAdd: {
            builder->encode_mnem2(Mnem_MOV, &dest_value, &left_val);
            builder->encode_mnem2(Mnem_ADD, &dest_value, &right_val);
            break;
        }

        case SIRBinaryOperation_ISub: {
            builder->encode_mnem2(Mnem_MOV, &dest_value, &left_val);
            builder->encode_mnem2(Mnem_SUB, &dest_value, &right_val);
            break;
        }

        case SIRBinaryOperation_IMul: {
            MetaValue ax_value =
                create_int_register_value(operand_size, RegisterIndex_RAX);
            MetaValue dx_value =
                create_int_register_value(operand_size, RegisterIndex_RDX);

            switch (operand_size) {
            case 1:
            case 2: {
                MetaValue ext_ax_value =
                    create_int_register_value(4, RegisterIndex_RAX);
                MetaValue ext_dx_value =
                    create_int_register_value(4, RegisterIndex_RDX);

                Mnem ext_mnem =
                    inst.type->int_.is_signed ? Mnem_MOVSX : Mnem_MOVZX;

                if (left_val.kind == MetaValueKind_ImmInt) {
                    left_val.size_class = SizeClass_4;
                    builder->encode_mnem2(Mnem_MOV, &ext_ax_value, &left_val);
                } else {
                    builder->encode_mnem2(ext_mnem, &ext_ax_value, &left_val);
                }

                if (right_val.kind == MetaValueKind_ImmInt) {
                    right_val.size_class = SizeClass_4;
                    builder->encode_mnem2(Mnem_MOV, &ext_dx_value, &right_val);
                } else {
                    builder->encode_mnem2(ext_mnem, &ext_dx_value, &right_val);
                }

                builder->encode_mnem2(Mnem_MUL, &ext_ax_value, &ext_dx_value);

                break;
            }
            default: {
                builder->encode_mnem2(Mnem_MOV, &ax_value, &left_val);
                builder->encode_mnem2(Mnem_MOV, &dx_value, &right_val);

                builder->encode_mnem2(Mnem_MUL, &ax_value, &dx_value);

                break;
            }
            }

            builder->encode_mnem2(Mnem_MOV, &dest_value, &ax_value);

            break;
        }

        case SIRBinaryOperation_UDiv:
        case SIRBinaryOperation_SDiv:
        case SIRBinaryOperation_SRem:
        case SIRBinaryOperation_URem: {
            MetaValue dividend_value =
                create_int_register_value(operand_size, RegisterIndex_RAX);
            MetaValue divisor_value = right_val;

            bool is_signed = inst.binop == SIRBinaryOperation_SDiv ||
                             inst.binop == SIRBinaryOperation_SRem;
            bool is_rem = inst.binop == SIRBinaryOperation_SRem ||
                          inst.binop == SIRBinaryOperation_URem;

            Mnem ext_mnem = Mnem_MOVZX;
            Mnem div_mnem = Mnem_DIV;
            if (is_signed) {
                ext_mnem = Mnem_MOVSX;
                div_mnem = Mnem_IDIV;
            }

            switch (operand_size) {
            case 1:
            case 2: {
                dividend_value =
                    create_int_register_value(4, RegisterIndex_RAX);

                if (left_val.kind == MetaValueKind_ImmInt) {
                    left_val.size_class = SizeClass_4;
                    builder->encode_mnem2(Mnem_MOV, &dividend_value, &left_val);
                } else {
                    builder->encode_mnem2(
                        Mnem_MOVSX, &dividend_value, &left_val);
                }

                // We need to move the divisor into a register to sign-extend it
                divisor_value = create_int_register_value(4, RegisterIndex_RCX);

                if (right_val.kind == MetaValueKind_ImmInt) {
                    right_val.size_class = SizeClass_4;
                    builder->encode_mnem2(Mnem_MOV, &divisor_value, &right_val);
                } else {
                    builder->encode_mnem2(ext_mnem, &divisor_value, &right_val);
                }

                break;
            }
            case 4:
            case 8: {
                builder->encode_mnem2(Mnem_MOV, &dividend_value, &left_val);

                // We need to move the divisor into a register if it's an
                // immediate
                if (right_val.kind == MetaValueKind_ImmInt) {
                    divisor_value = create_int_register_value(
                        operand_size, RegisterIndex_RCX);
                    builder->encode_mnem2(Mnem_MOV, &divisor_value, &right_val);
                }
                break;
            }
            default: SIR_ASSERT(0); break;
            }

            if (is_signed) {
                builder->encode_mnem1(Mnem_C_SEP, &dividend_value);
            } else {
                MetaValue dx_value =
                    create_int_register_value(4, RegisterIndex_RDX);
                MetaValue zero_value = create_imm_int_value(4, 0);

                builder->encode_mnem2(Mnem_MOV, &dx_value, &zero_value);
            }
            builder->encode_mnem1(div_mnem, &divisor_value);

            if (!is_rem) {
                dividend_value.size_class = SIZE_CLASSES[operand_size];
                builder->encode_mnem2(Mnem_MOV, &dest_value, &dividend_value);
            } else {
                MetaValue dx_value =
                    create_int_register_value(operand_size, RegisterIndex_RDX);
                builder->encode_mnem2(Mnem_MOV, &dest_value, &dx_value);
            }

            break;
        }

        case SIRBinaryOperation_FAdd: {
            MetaValue xmm0_value =
                create_float_register_value(operand_size, RegisterIndex_XMM0);
            builder->encode_mnem2(Mnem_MOV, &xmm0_value, &left_val);
            builder->encode_mnem2(Mnem_SSE_ADD, &xmm0_value, &right_val);
            builder->encode_mnem2(Mnem_MOV, &dest_value, &xmm0_value);
            break;
        }

        case SIRBinaryOperation_FSub: {
            MetaValue xmm0_value =
                create_float_register_value(operand_size, RegisterIndex_XMM0);
            builder->encode_mnem2(Mnem_MOV, &xmm0_value, &left_val);
            builder->encode_mnem2(Mnem_SSE_SUB, &xmm0_value, &right_val);
            builder->encode_mnem2(Mnem_MOV, &dest_value, &xmm0_value);
            break;
        }

        case SIRBinaryOperation_FMul: {
            MetaValue xmm0_value =
                create_float_register_value(operand_size, RegisterIndex_XMM0);
            builder->encode_mnem2(Mnem_MOV, &xmm0_value, &left_val);
            builder->encode_mnem2(Mnem_SSE_MUL, &xmm0_value, &right_val);
            builder->encode_mnem2(Mnem_MOV, &dest_value, &xmm0_value);
            break;
        }

        case SIRBinaryOperation_FDiv: {
            MetaValue xmm0_value =
                create_float_register_value(operand_size, RegisterIndex_XMM0);
            builder->encode_mnem2(Mnem_MOV, &xmm0_value, &left_val);
            builder->encode_mnem2(Mnem_SSE_DIV, &xmm0_value, &right_val);
            builder->encode_mnem2(Mnem_MOV, &dest_value, &xmm0_value);
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

            builder->move_inst_rvalue(inst.op1, &ax_value);
            builder->move_inst_rvalue(inst.op2, &dx_value);

            int64_t x86_inst = 0;
            switch (operand_size) {
            case 1: x86_inst = FE_CMP8rr; break;
            case 2: x86_inst = FE_CMP16rr; break;
            case 4: x86_inst = FE_CMP32rr; break;
            case 8: x86_inst = FE_CMP64rr; break;
            default: SIR_ASSERT(0); break;
            }

            builder->encode(x86_inst, FE_AX, FE_DX);

            switch (inst.binop) {
            default: SIR_ASSERT(0); break;
            case SIRBinaryOperation_IEQ:
                builder->encode(FE_SETZ8r, FE_AX); // sete
                break;
            case SIRBinaryOperation_INE:
                builder->encode(FE_SETNZ8r, FE_AX); // setne
                break;
            case SIRBinaryOperation_UGT:
                builder->encode(FE_SETA8r, FE_AX); // seta
                break;
            case SIRBinaryOperation_UGE:
                builder->encode(FE_SETNC8r, FE_AX); // setae
                break;
            case SIRBinaryOperation_ULT:
                builder->encode(FE_SETC8r, FE_AX); // setb
                break;
            case SIRBinaryOperation_ULE:
                builder->encode(FE_SETBE8r, FE_AX); // setbe
                break;
            case SIRBinaryOperation_SGT:
                builder->encode(FE_SETG8r, FE_AX); // setg
                break;
            case SIRBinaryOperation_SGE:
                builder->encode(FE_SETGE8r, FE_AX); // setge
                break;
            case SIRBinaryOperation_SLT:
                builder->encode(FE_SETL8r, FE_AX); // setl
                break;
            case SIRBinaryOperation_SLE:
                builder->encode(FE_SETLE8r, FE_AX); // setle
                break;
            }

            MetaValue al_value =
                create_int_register_value(1, RegisterIndex_RAX);
            builder->encode(FE_AND8ri, FE_AX, 1);
            builder->encode_mnem2(Mnem_MOV, &dest_value, &al_value);
            break;
        }

        case SIRBinaryOperation_FEQ:
        case SIRBinaryOperation_FNE:
        case SIRBinaryOperation_FGT:
        case SIRBinaryOperation_FGE:
        case SIRBinaryOperation_FLT:
        case SIRBinaryOperation_FLE: {
            MetaValue xmm0_value =
                create_float_register_value(operand_size, RegisterIndex_XMM0);
            MetaValue xmm1_value =
                create_float_register_value(operand_size, RegisterIndex_XMM1);

            builder->encode_mnem2(Mnem_MOV, &xmm0_value, &left_val);
            builder->encode_mnem2(Mnem_MOV, &xmm1_value, &right_val);

            switch (inst.binop) {
            default: SIR_ASSERT(0); break;
            case SIRBinaryOperation_FEQ:
            case SIRBinaryOperation_FNE:
                builder->encode_mnem2(
                    Mnem_SSE_UCOMIS, &xmm0_value, &xmm1_value);
                break;
            case SIRBinaryOperation_FGE:
            case SIRBinaryOperation_FGT:
                builder->encode_mnem2(
                    Mnem_SSE_UCOMIS, &xmm0_value, &xmm1_value);
                break;
            case SIRBinaryOperation_FLT:
            case SIRBinaryOperation_FLE:
                builder->encode_mnem2(
                    Mnem_SSE_UCOMIS, &xmm1_value, &xmm0_value);
                break;
            }

            switch (inst.binop) {
            default: SIR_ASSERT(0); break;
            case SIRBinaryOperation_FEQ:
                builder->encode(FE_SETZ8r, FE_AX); // sete
                break;
            case SIRBinaryOperation_FNE:
                builder->encode(FE_SETNZ8r, FE_AX); // setne
                break;
            case SIRBinaryOperation_FLT:
            case SIRBinaryOperation_FGT:
                builder->encode(FE_SETA8r, FE_AX); // seta
                break;
            case SIRBinaryOperation_FLE:
            case SIRBinaryOperation_FGE:
                builder->encode(FE_SETNC8r, FE_AX); // setae
                break;
            }

            MetaValue al_value =
                create_int_register_value(1, RegisterIndex_RAX);
            builder->encode(FE_AND8ri, FE_AX, 1);
            builder->encode_mnem2(Mnem_MOV, &dest_value, &al_value);
            break;
        }

        case SIRBinaryOperation_And:
        case SIRBinaryOperation_Or:
        case SIRBinaryOperation_Xor: {
            Mnem mnem;
            switch (inst.binop) {
            case SIRBinaryOperation_And: mnem = Mnem_AND; break;
            case SIRBinaryOperation_Or: mnem = Mnem_OR; break;
            case SIRBinaryOperation_Xor: mnem = Mnem_XOR; break;
            default: SIR_ASSERT(0); break;
            }

            builder->encode_mnem2(Mnem_MOV, &dest_value, &left_val);
            builder->encode_mnem2(mnem, &dest_value, &right_val);
            break;
        }

        case SIRBinaryOperation_Shl:
        case SIRBinaryOperation_AShr:
        case SIRBinaryOperation_LShr: {
            Mnem mnem;
            switch (inst.binop) {
            case SIRBinaryOperation_Shl: mnem = Mnem_SHL; break;
            case SIRBinaryOperation_AShr: mnem = Mnem_SAR; break;
            case SIRBinaryOperation_LShr: mnem = Mnem_SHR; break;
            default: SIR_ASSERT(0); break;
            }

            builder->encode_mnem2(Mnem_MOV, &dest_value, &left_val);

            MetaValue shift_bits_val = right_val;
            if (shift_bits_val.kind == MetaValueKind_ImmInt) {
                shift_bits_val.size_class = SizeClass_1;
            } else {
                // The shift instructions require the RHS register to be CL
                shift_bits_val = create_int_register_value(
                    SIZE_CLASS_SIZES[right_val.size_class], RegisterIndex_RCX);
                builder->encode_mnem2(Mnem_MOV, &shift_bits_val, &right_val);

                shift_bits_val =
                    create_int_register_value(1, RegisterIndex_RCX);
            }

            builder->encode_mnem2(mnem, &dest_value, &shift_bits_val);
            break;
        }
        }

        break;
    }

    case SIRInstKind_ArrayElemPtr: {
        size_t index_size = SIRTypeSizeOf(
            builder->module,
            SIRModuleGetInst(builder->module, inst.array_elem_ptr.index_ref)
                .type);

        MetaValue index_value =
            create_int_register_value(index_size, RegisterIndex_RCX);
        builder->encode_mnem2(
            Mnem_MOV,
            &index_value,
            &builder->meta_insts[inst.array_elem_ptr.index_ref.id]);

        MetaValue base_ptr_value =
            create_int_register_value(8, RegisterIndex_RAX);
        builder->encode_mnem2(
            Mnem_MOV,
            &base_ptr_value,
            &builder->meta_insts[inst.array_elem_ptr.accessed_ref.id]);

        int32_t scale = SIRTypeSizeOf(builder->module, inst.type->pointer.sub);

        MetaValue value_addr = create_int_register_memory_value(
            8, RegisterIndex_RAX, scale, RegisterIndex_RCX, 0);

        builder->encode_mnem2(
            Mnem_LEA, &builder->meta_insts[inst_ref.id], &value_addr);

        break;
    }

    case SIRInstKind_StructElemPtr: {
        SIRInstRef field_index_ref = inst.struct_elem_ptr.field_index_ref;
        uint32_t field_index =
            SIRModuleGetInst(builder->module, field_index_ref).const_int.u64;

        SIRType *struct_type =
            SIRModuleGetInst(builder->module, inst.struct_elem_ptr.accessed_ref)
                .type->pointer.sub;

        uint32_t field_offset =
            SIRTypeStructOffsetOf(builder->module, struct_type, field_index);

        MetaValue base_ptr_value =
            builder->meta_insts[inst.struct_elem_ptr.accessed_ref.id];
        switch (base_ptr_value.kind) {
        case MetaValueKind_IRegisterMemoryPtr: {
            base_ptr_value.regmem.offset += field_offset;
            builder->meta_insts[inst_ref.id] = base_ptr_value;
            break;
        }
        default: {
            MetaValue ptr_value =
                create_int_register_value(8, RegisterIndex_RAX);
            builder->encode_mnem2(Mnem_MOV, &ptr_value, &base_ptr_value);
            ptr_value = create_int_register_memory_value(
                8, RegisterIndex_RAX, 0, RegisterIndex_None, field_offset);
            builder->encode_mnem2(
                Mnem_LEA, &builder->meta_insts[inst_ref.id], &ptr_value);
            break;
        }
        }
        break;
    }

    case SIRInstKind_ExtractArrayElem: {
        SIRInstRef elem_index_ref = inst.extract_array_elem.index_ref;
        uint32_t elem_index =
            SIRModuleGetInst(builder->module, elem_index_ref).const_int.u64;

        size_t value_size = SIRTypeSizeOf(builder->module, inst.type);

        MetaValue accessed_value =
            builder->meta_insts[inst.extract_array_elem.accessed_ref.id];
        SIR_ASSERT(accessed_value.kind == MetaValueKind_IRegisterMemory);
        MetaValue ptr_value = create_int_register_value(8, RegisterIndex_RAX);
        builder->encode_mnem2(Mnem_LEA, &ptr_value, &accessed_value);

        MetaValue value_addr = create_int_register_memory_value(
            value_size,
            RegisterIndex_RAX,
            value_size * elem_index,
            RegisterIndex_None,
            0);

        switch (value_size) {
        case 1:
        case 2:
        case 4:
        case 8: {
            builder->encode_mnem2(
                Mnem_MOV, &builder->meta_insts[inst_ref.id], &value_addr);
            break;
        }
        default: {
            builder->encode_memcpy(
                value_size, value_addr, builder->meta_insts[inst_ref.id]);
            break;
        }
        }
        break;
    }

    case SIRInstKind_ExtractStructElem: {
        SIRInstRef field_index_ref = inst.extract_struct_elem.field_index_ref;
        uint32_t field_index =
            SIRModuleGetInst(builder->module, field_index_ref).const_int.u64;

        size_t value_size = SIRTypeSizeOf(builder->module, inst.type);

        SIRType *struct_type =
            SIRModuleGetInst(
                builder->module, inst.extract_struct_elem.accessed_ref)
                .type;

        uint32_t field_offset =
            SIRTypeStructOffsetOf(builder->module, struct_type, field_index);

        MetaValue ptr_mem_value =
            builder->meta_insts[inst.extract_struct_elem.accessed_ref.id];
        SIR_ASSERT(ptr_mem_value.kind == MetaValueKind_IRegisterMemory);
        ptr_mem_value.regmem.offset += field_offset;

        switch (value_size) {
        case 1:
        case 2:
        case 4:
        case 8: {
            ptr_mem_value.size_class = SIZE_CLASSES[value_size];
            builder->encode_mnem2(
                Mnem_MOV, &builder->meta_insts[inst_ref.id], &ptr_mem_value);
            break;
        }
        default: {
            ptr_mem_value.size_class = SizeClass_None;
            builder->encode_memcpy(
                value_size, ptr_mem_value, builder->meta_insts[inst_ref.id]);
            break;
        }
        }

        break;
    }

    case SIRInstKind_Store: {
        size_t value_size = SIRTypeSizeOf(
            builder->module,
            SIRModuleGetInst(builder->module, inst.store.value_ref).type);

        MetaValue stored_value = builder->meta_insts[inst.store.value_ref.id];

        MetaValue dest_value = builder->meta_insts[inst.store.ptr_ref.id];
        switch (dest_value.kind) {
        case MetaValueKind_IRegisterMemoryPtr: {
            dest_value.kind = MetaValueKind_IRegisterMemory;
            break;
        }
        case MetaValueKind_GlobalPtr: {
            /* dest_value.kind = MetaValueKind_Global; */
            MetaValue new_ptr_value =
                create_int_register_value(8, RegisterIndex_RCX);
            builder->encode_mnem2(Mnem_MOV, &new_ptr_value, &dest_value);
            dest_value = create_int_register_memory_value(
                value_size, new_ptr_value.reg.index, 0, RegisterIndex_None, 0);
            break;
        }
        case MetaValueKind_IRegister: {
            MetaValue new_ptr_value = create_int_register_memory_value(
                value_size, dest_value.reg.index, 0, RegisterIndex_None, 0);
            dest_value = new_ptr_value;
            break;
        }
        case MetaValueKind_IRegisterMemory: {
            MetaValue new_ptr_value =
                create_int_register_value(8, RegisterIndex_RCX);
            builder->encode_mnem2(Mnem_MOV, &new_ptr_value, &dest_value);
            dest_value = create_int_register_memory_value(
                value_size, new_ptr_value.reg.index, 0, RegisterIndex_None, 0);
            break;
        }
        default: {
            SIR_ASSERT(0);
            break;
        }
        }

        builder->encode_memcpy(value_size, stored_value, dest_value);

        break;
    }

    case SIRInstKind_Load: {
        size_t value_size = SIRTypeSizeOf(builder->module, inst.type);

        MetaValue source_value = builder->meta_insts[inst.load.ptr_ref.id];
        switch (source_value.kind) {
        case MetaValueKind_IRegisterMemoryPtr: {
            source_value.kind = MetaValueKind_IRegisterMemory;
            break;
        }
        case MetaValueKind_GlobalPtr: {
            source_value.kind = MetaValueKind_Global;
            break;
        }
        case MetaValueKind_IRegister: {
            MetaValue new_ptr_value = create_int_register_memory_value(
                value_size, source_value.reg.index, 0, RegisterIndex_None, 0);
            source_value = new_ptr_value;
            break;
        }
        case MetaValueKind_IRegisterMemory: {
            MetaValue new_ptr_value =
                create_int_register_value(8, RegisterIndex_RCX);
            builder->encode_mnem2(Mnem_MOV, &new_ptr_value, &source_value);
            source_value = create_int_register_memory_value(
                value_size, new_ptr_value.reg.index, 0, RegisterIndex_None, 0);
            break;
        }
        default: {
            SIR_ASSERT(0);
            break;
        }
        }

        builder->encode_memcpy(
            value_size, source_value, builder->meta_insts[inst_ref.id]);

        break;
    }

    case SIRInstKind_FuncCall: {
        SIRInstRef called_func_ref = inst.op1;
        SIRFunction *called_func =
            SIRModuleGetInst(builder->module, called_func_ref).func;

        // TODO: save and restore caller-saved registers

        SIR_ASSERT(
            called_func->param_types_len <= builder->current_func_params.len);

        switch (called_func->calling_convention) {
        case SIRCallingConvention_SystemV: {
            uint32_t used_int_regs = 0;
            uint32_t used_float_regs = 0;

            size_t param_stack_offset = 0;

            // Write parameters to ABI locations
            for (size_t i = 0; i < builder->current_func_params.len; ++i) {
                SIRInstRef param_inst_ref = builder->current_func_params[i];
                SIR_ASSERT(param_inst_ref.id > 0);

                SIRInst param_inst =
                    SIRModuleGetInst(builder->module, param_inst_ref);

                if (i < called_func->param_types_len) {
                    SIRType *param_type = called_func->param_types[i];
                    SIR_ASSERT(param_type == param_inst.type);
                }

                SIRType *param_type = param_inst.type;
                uint32_t param_align =
                    SIRTypeAlignOf(builder->module, param_type);
                uint32_t param_size =
                    SIRTypeSizeOf(builder->module, param_type);

                SysVParamClass class1 = SysVParamClass_SSE;
                SysVParamClass class2 = SysVParamClass_SSE;
                bool use_regs = sysv_param_should_use_regs(
                    builder,
                    param_type,
                    &class1,
                    &class2,
                    used_int_regs,
                    used_float_regs);

                if (use_regs) {
                    MetaValue param_meta_inst =
                        builder->meta_insts[param_inst_ref.id];

                    // First register
                    {
                        uint32_t param_size1 = SIR_MIN(param_size, 8);

                        MetaValue param_meta_inst1 = param_meta_inst;
                        param_meta_inst1.size_class = SIZE_CLASSES[param_size1];

                        MetaValue param_value1;
                        switch (class1) {
                        case SysVParamClass_Int:
                            param_value1 = create_int_register_value(
                                param_size1,
                                SYSV_INT_PARAM_REGS[used_int_regs++]);
                            break;
                        case SysVParamClass_SSE:
                            param_value1 = create_float_register_value(
                                param_size1,
                                SYSV_FLOAT_PARAM_REGS[used_float_regs++]);
                            break;
                        }

                        builder->encode_memcpy(
                            param_size1, param_meta_inst1, param_value1);
                    }

                    // Second register
                    if (param_size > 8) {
                        uint32_t param_size2 = param_size - 8;

                        MetaValue param_meta_inst2 = param_meta_inst;
                        param_meta_inst2.size_class = SIZE_CLASSES[param_size2];
                        param_meta_inst2.regmem.offset += 8;

                        MetaValue param_value2;
                        switch (class2) {
                        case SysVParamClass_Int:
                            param_value2 = create_int_register_value(
                                param_size2,
                                SYSV_INT_PARAM_REGS[used_int_regs++]);
                            break;
                        case SysVParamClass_SSE:
                            param_value2 = create_float_register_value(
                                param_size2,
                                SYSV_FLOAT_PARAM_REGS[used_float_regs++]);
                            break;
                        }

                        builder->encode_memcpy(
                            param_size2, param_meta_inst2, param_value2);
                    }
                } else {
                    param_stack_offset =
                        SIR_ROUND_UP(param_align, param_stack_offset);
                    MetaValue dest_param_value =
                        create_int_register_memory_value(
                            param_size,
                            RegisterIndex_RSP,
                            0,
                            RegisterIndex_None,
                            param_stack_offset);
                    param_stack_offset +=
                        param_size; // TODO: not sure if builder should have
                                    // alignment added to it
                    builder->move_inst_rvalue(
                        param_inst_ref, &dest_param_value);
                }
            }

            if (called_func->variadic) {
                // Write the amount of vector registers to %AL
                MetaValue float_count_imm =
                    create_imm_int_value(4, used_float_regs);
                MetaValue float_count_reg =
                    create_int_register_value(4, RegisterIndex_RAX);
                builder->encode_mnem2(
                    Mnem_MOV, &float_count_reg, &float_count_imm);
            }

            break;
        }
        }

        // Reset parameter list
        builder->current_func_params.len = 0;

        builder->encode_direct_call(called_func_ref);

        // Move returned values to result location

        switch (called_func->calling_convention) {
        case SIRCallingConvention_SystemV: {
            SIRType *return_type = called_func->return_type;
            size_t return_type_size =
                SIRTypeSizeOf(builder->module, return_type);
            if (return_type_size == 0) break;

            if (return_type_size <= 16) {
                uint32_t used_int_regs = 0;
                uint32_t used_float_regs = 0;

                SysVParamClass class1 = SysVParamClass_SSE;
                SysVParamClass class2 = SysVParamClass_SSE;
                sysv_get_type_register_classes(
                    builder, return_type, &class1, &class2);

                MetaValue returned_value = builder->meta_insts[inst_ref.id];

                // First register
                {
                    MetaValue loc1 = {};
                    switch (class1) {
                    case SysVParamClass_Int:
                        loc1 = create_int_register_value(
                            8, SYSV_INT_RETURN_REGS[used_int_regs++]);
                        break;
                    case SysVParamClass_SSE:
                        loc1 = create_float_register_value(
                            8, SYSV_FLOAT_RETURN_REGS[used_float_regs++]);
                        break;
                    }

                    MetaValue returned_value1 = returned_value;

                    builder->encode_memcpy(
                        SIR_MIN(8, return_type_size), loc1, returned_value1);
                }

                // Second register
                if (return_type_size > 8) {
                    SIR_ASSERT(
                        returned_value.kind == MetaValueKind_IRegisterMemory);

                    MetaValue loc2 = {};
                    switch (class2) {
                    case SysVParamClass_Int:
                        loc2 = create_int_register_value(
                            8, SYSV_INT_RETURN_REGS[used_int_regs++]);
                        break;
                    case SysVParamClass_SSE:
                        loc2 = create_float_register_value(
                            8, SYSV_FLOAT_RETURN_REGS[used_float_regs++]);
                        break;
                    }

                    MetaValue returned_value2 = returned_value;
                    returned_value2.regmem.offset += 8;

                    builder->encode_memcpy(
                        return_type_size - 8, loc2, returned_value2);
                }
            } else {
                SIR_ASSERT(!"unimplemented returning larger values");
            }
            break;
        }
        }

        break;
    }

    case SIRInstKind_ReturnVoid: {
        builder->encode_function_ending(func_ref);
        break;
    }

    case SIRInstKind_ReturnValue: {
        SIRInstRef returned_inst_ref = inst.op1;
        SIRInst returned_inst =
            SIRModuleGetInst(builder->module, returned_inst_ref);
        SIRFunction *current_func =
            SIRModuleGetInst(builder->module, builder->current_func).func;

        SIRType *return_type = returned_inst.type;
        switch (current_func->calling_convention) {
        case SIRCallingConvention_SystemV: {
            size_t return_type_size =
                SIRTypeSizeOf(builder->module, return_type);
            if (return_type_size == 0) break;

            if (return_type_size <= 16) {
                uint32_t used_int_regs = 0;
                uint32_t used_float_regs = 0;

                SysVParamClass class1 = SysVParamClass_SSE;
                SysVParamClass class2 = SysVParamClass_SSE;
                sysv_get_type_register_classes(
                    builder, return_type, &class1, &class2);

                MetaValue returned_value =
                    builder->meta_insts[returned_inst_ref.id];

                // First register
                {
                    MetaValue loc1 = {};
                    switch (class1) {
                    case SysVParamClass_Int:
                        loc1 = create_int_register_value(
                            8, SYSV_INT_RETURN_REGS[used_int_regs++]);
                        break;
                    case SysVParamClass_SSE:
                        loc1 = create_float_register_value(
                            8, SYSV_FLOAT_RETURN_REGS[used_float_regs++]);
                        break;
                    }

                    MetaValue returned_value1 = returned_value;

                    builder->encode_memcpy(
                        SIR_MIN(8, return_type_size), returned_value1, loc1);
                }

                // Second register
                if (return_type_size > 8) {
                    SIR_ASSERT(
                        returned_value.kind == MetaValueKind_IRegisterMemory);
                    MetaValue loc2 = {};
                    switch (class2) {
                    case SysVParamClass_Int:
                        loc2 = create_int_register_value(
                            8, SYSV_INT_RETURN_REGS[used_int_regs++]);
                        break;
                    case SysVParamClass_SSE:
                        loc2 = create_float_register_value(
                            8, SYSV_FLOAT_RETURN_REGS[used_float_regs++]);
                        break;
                    }

                    MetaValue returned_value2 = returned_value;
                    returned_value2.regmem.offset += 8;

                    builder->encode_memcpy(
                        return_type_size - 8, returned_value2, loc2);
                }
            } else {
                SIR_ASSERT(!"TODO: unimplemented returning larger values");
            }
            break;
        }
        }

        builder->encode_function_ending(func_ref);
        break;
    }

    case SIRInstKind_Jump: {
        SIRInst dest_block = SIRModuleGetInst(builder->module, inst.op1);

        SIRInstRef phi_ref = {0};
        for (size_t i = 0; i < dest_block.block->inst_refs.len; ++i) {
            SIRInstRef next_inst_ref = dest_block.block->inst_refs[i];
            SIRInst next_inst =
                SIRModuleGetInst(builder->module, next_inst_ref);
            if (i == 0) {
                if (next_inst.kind != SIRInstKind_Phi) break;

                phi_ref = next_inst_ref;
            } else {
                if (next_inst.kind != SIRInstKind_PhiIncoming) break;

                if (next_inst.phi_incoming.block_ref.id ==
                    builder->current_block.id) {
                    builder->move_inst_rvalue(
                        next_inst.phi_incoming.value_ref,
                        &builder->meta_insts[phi_ref.id]);
                    break;
                }
            }
        }

        FuncJumpPatch patch = {
            .instruction = FE_JMP | FE_JMPL,
            .instruction_offset = builder->get_code_offset(),
            .destination_block = inst.op1,
        };
        meta_func->jump_patches.push_back(patch);

        builder->encode(FE_JMP | FE_JMPL, -patch.instruction_offset);

        break;
    }
    case SIRInstKind_Branch: {
        MetaValue al_value = create_int_register_value(1, RegisterIndex_RAX);

        SIRInstRef true_block = inst.op1;
        SIRInstRef false_block = inst.op2;

        SIRInstRef true_phi = {0};
        SIRInstRef true_phi_value = {0};
        SIRInstRef false_phi = {0};
        SIRInstRef false_phi_value = {0};

        SIRInst dest_block;

        // Get true phi
        dest_block = SIRModuleGetInst(builder->module, true_block);
        for (size_t i = 0; i < dest_block.block->inst_refs.len; ++i) {
            SIRInstRef next_inst_ref = dest_block.block->inst_refs[i];
            SIRInst next_inst =
                SIRModuleGetInst(builder->module, next_inst_ref);
            if (i == 0) {
                if (next_inst.kind != SIRInstKind_Phi) break;

                true_phi = next_inst_ref;
            } else {
                if (next_inst.kind != SIRInstKind_PhiIncoming) break;

                if (next_inst.phi_incoming.block_ref.id ==
                    builder->current_block.id) {
                    true_phi_value = next_inst.phi_incoming.value_ref;
                    break;
                }
            }
        }

        // Get false phi
        dest_block = SIRModuleGetInst(builder->module, false_block);
        for (size_t i = 0; i < dest_block.block->inst_refs.len; ++i) {
            SIRInstRef next_inst_ref = dest_block.block->inst_refs[i];
            SIRInst next_inst =
                SIRModuleGetInst(builder->module, next_inst_ref);
            if (i == 0) {
                if (next_inst.kind != SIRInstKind_Phi) break;

                false_phi = next_inst_ref;
            } else {
                if (next_inst.kind != SIRInstKind_PhiIncoming) break;

                if (next_inst.phi_incoming.block_ref.id ==
                    builder->current_block.id) {
                    false_phi_value = next_inst.phi_incoming.value_ref;
                    break;
                }
            }
        }

        builder->encode_mnem2(
            Mnem_MOV,
            &al_value,
            &builder->meta_insts[builder->current_cond.id]);

        builder->encode(
            FE_TEST8rr,
            REGISTERS[al_value.reg.index],
            REGISTERS[al_value.reg.index]);

        if (true_phi.id) {
            builder->move_inst_rvalue(
                true_phi_value, &builder->meta_insts[true_phi.id]);
        }

        FuncJumpPatch true_patch = {
            .instruction = FE_JNZ | FE_JMPL,
            .instruction_offset = builder->get_code_offset(),
            .destination_block = true_block,
        };
        meta_func->jump_patches.push_back(true_patch);
        builder->encode(FE_JNZ | FE_JMPL, -true_patch.instruction_offset);

        if (false_phi.id) {
            builder->move_inst_rvalue(
                false_phi_value, &builder->meta_insts[false_phi.id]);
        }

        FuncJumpPatch false_patch = {
            .instruction = FE_JMP | FE_JMPL,
            .instruction_offset = builder->get_code_offset(),
            .destination_block = false_block,
        };
        meta_func->jump_patches.push_back(false_patch);
        builder->encode(FE_JMP | FE_JMPL, -false_patch.instruction_offset);

        break;
    }
    case SIRInstKind_Phi: {
        break;
    }
    }
}

SIR_INLINE void generate_global(X64AsmBuilder *builder, SIRInstRef global_ref)
{
    ZoneScoped;

    SIRInst global = SIRModuleGetInst(builder->module, global_ref);

    builder->meta_insts[global_ref.id] = create_global_ptr_value(
        builder,
        global.global->flags,
        global.global->data,
        global.global->data_len);
}

SIR_INLINE void
move_func_params_to_stack(X64AsmBuilder *builder, SIRFunction *func)
{
    ZoneScoped;

    switch (func->calling_convention) {
    case SIRCallingConvention_SystemV: {
        uint32_t used_int_regs = 0;
        uint32_t used_float_regs = 0;

        uint32_t stack_param_offset = 16;

        for (uint32_t i = 0; i < func->param_types_len; ++i) {
            SIRType *param_type = func->param_types[i];
            SIRInstRef param_inst_ref = func->param_insts[i];
            MetaValue param_meta_inst = builder->meta_insts[param_inst_ref.id];

            uint32_t param_size = SIRTypeSizeOf(builder->module, param_type);

            SysVParamClass class1 = SysVParamClass_SSE;
            SysVParamClass class2 = SysVParamClass_SSE;
            bool use_regs = sysv_param_should_use_regs(
                builder,
                param_type,
                &class1,
                &class2,
                used_int_regs,
                used_float_regs);

            if (use_regs) {
                // First register
                {
                    uint32_t param_size1 = SIR_MIN(param_size, 8);

                    MetaValue param_meta_inst1 = param_meta_inst;
                    param_meta_inst1.size_class = SIZE_CLASSES[param_size1];

                    MetaValue param_value1;
                    switch (class1) {
                    case SysVParamClass_Int:
                        param_value1 = create_int_register_value(
                            param_size1, SYSV_INT_PARAM_REGS[used_int_regs++]);
                        break;
                    case SysVParamClass_SSE:
                        param_value1 = create_float_register_value(
                            param_size1,
                            SYSV_FLOAT_PARAM_REGS[used_float_regs++]);
                        break;
                    }

                    builder->encode_memcpy(
                        param_size1, param_value1, param_meta_inst1);
                }

                // Second register
                if (param_size > 8) {
                    uint32_t param_size2 = param_size - 8;

                    MetaValue param_meta_inst2 = param_meta_inst;
                    param_meta_inst2.size_class = SIZE_CLASSES[param_size2];
                    param_meta_inst2.regmem.offset += 8;

                    MetaValue param_value2;
                    switch (class2) {
                    case SysVParamClass_Int:
                        param_value2 = create_int_register_value(
                            param_size2, SYSV_INT_PARAM_REGS[used_int_regs++]);
                        break;
                    case SysVParamClass_SSE:
                        param_value2 = create_float_register_value(
                            param_size2,
                            SYSV_FLOAT_PARAM_REGS[used_float_regs++]);
                        break;
                    }

                    builder->encode_memcpy(
                        param_size2, param_value2, param_meta_inst2);
                }
            } else {
                MetaValue param_value = create_int_register_memory_value(
                    param_size,
                    RegisterIndex_RBP,
                    0,
                    RegisterIndex_None,
                    stack_param_offset);
                stack_param_offset += param_size;

                builder->encode_memcpy(
                    param_size, param_value, param_meta_inst);
            }
        }

        break;
    }
    }
}

SIR_INLINE size_t get_func_call_stack_parameters_size(
    X64AsmBuilder *builder, SIRInstRef func_call_ref)
{
    ZoneScoped;

    size_t stack_parameters_size = 0;

    SIRInst func_call = SIRModuleGetInst(builder->module, func_call_ref);
    SIRFunction *called_func =
        SIRModuleGetInst(builder->module, func_call.op1).func;

    switch (called_func->calling_convention) {
    case SIRCallingConvention_SystemV: {
        uint32_t used_int_regs = 0;
        uint32_t used_float_regs = 0;

        for (uint32_t i = 0; i < called_func->param_types_len; ++i) {
            SIRType *param_type = called_func->param_types[i];

            uint32_t param_align = SIRTypeAlignOf(builder->module, param_type);
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
                size_t param_size = SIRTypeSizeOf(builder->module, param_type);

                SysVParamClass class1 = SysVParamClass_SSE;
                SysVParamClass class2 = SysVParamClass_SSE;
                bool use_regs = sysv_param_should_use_regs(
                    builder,
                    param_type,
                    &class1,
                    &class2,
                    used_int_regs,
                    used_float_regs);

                if (!use_regs) {
                    stack_parameters_size += param_size;
                } else {
                    switch (class1) {
                    case SysVParamClass_Int: used_int_regs += 1; break;
                    case SysVParamClass_SSE: used_float_regs += 1; break;
                    }

                    if (param_size > 8) {
                        switch (class2) {
                        case SysVParamClass_Int: used_int_regs += 1; break;
                        case SysVParamClass_SSE: used_float_regs += 1; break;
                        }
                    }
                }
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

static void reg_stack_alloc(
    X64AsmBuilder *builder, SIRFunction *func, MetaFunction *meta_func)
{
    ZoneScoped;

    size_t stack_params_size = 0;

    // Spill all elegible insts to stack
    for (SIRInstRef block_ref : func->blocks) {
        SIRInst block = SIRModuleGetInst(builder->module, block_ref);
        for (SIRInstRef inst_ref : block.block->inst_refs) {
            SIRInst inst = SIRModuleGetInst(builder->module, inst_ref);

            switch (inst.kind) {
            // Invalid for this stage of generation:
            case SIRInstKind_Unknown:
            case SIRInstKind_Global:
            case SIRInstKind_StackSlot:
            case SIRInstKind_Block:
            case SIRInstKind_Function:
            case SIRInstKind_FunctionParameter: SIR_ASSERT(0); break;

            // Contain no data for spilling:
            case SIRInstKind_PhiIncoming:
            case SIRInstKind_Store:
            case SIRInstKind_Jump:
            case SIRInstKind_Branch:
            case SIRInstKind_ReturnVoid:
            case SIRInstKind_ReturnValue:
            case SIRInstKind_SetCond:
            case SIRInstKind_PushFunctionParameter: break;

            // Data already stored somewhere else:
            case SIRInstKind_Alias:
            case SIRInstKind_ConstInt:
            case SIRInstKind_ConstFloat:
            case SIRInstKind_ConstBool:
            case SIRInstKind_BitCast: break;

            // Create stack space for these kinds:
            case SIRInstKind_Phi:
            case SIRInstKind_ZExt:
            case SIRInstKind_SExt:
            case SIRInstKind_Trunc:
            case SIRInstKind_FPTrunc:
            case SIRInstKind_FPExt:
            case SIRInstKind_SIToFP:
            case SIRInstKind_UIToFP:
            case SIRInstKind_FPToSI:
            case SIRInstKind_FPToUI:
            case SIRInstKind_FNeg:
            case SIRInstKind_Binop:
            case SIRInstKind_ArrayElemPtr:
            case SIRInstKind_StructElemPtr:
            case SIRInstKind_ExtractArrayElem:
            case SIRInstKind_ExtractStructElem:
            case SIRInstKind_Load:
            case SIRInstKind_FuncCall: {
                uint32_t inst_size = SIRTypeSizeOf(builder->module, inst.type);
                uint32_t inst_align =
                    SIRTypeAlignOf(builder->module, inst.type);

                meta_func->stack_size =
                    SIR_ROUND_UP(inst_align, meta_func->stack_size);
                meta_func->stack_size += inst_size;

                builder->meta_insts[inst_ref.id] = create_stack_value(
                    inst_size, -((int32_t)meta_func->stack_size));

                if (inst.kind == SIRInstKind_FuncCall) {
                    size_t func_stack_params_size =
                        get_func_call_stack_parameters_size(builder, inst_ref);
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
}

static void inst_aliasing_pass(X64AsmBuilder *builder, SIRFunction *func)
{
    ZoneScoped;

    for (SIRInstRef block_ref : func->blocks) {
        SIRInst block = SIRModuleGetInst(builder->module, block_ref);
        for (SIRInstRef inst_ref : block.block->inst_refs) {
            SIRInst inst = SIRModuleGetInst(builder->module, inst_ref);
            switch (inst.kind) {
            case SIRInstKind_Trunc: {
                SIRType *dest_type =
                    SIRModuleGetInstType(builder->module, inst_ref);
                SIRType *source_type =
                    SIRModuleGetInstType(builder->module, inst.op1);

                uint32_t dest_type_size =
                    SIRTypeSizeOf(builder->module, dest_type);
                uint32_t source_type_size =
                    SIRTypeSizeOf(builder->module, source_type);
                uint32_t dest_type_align =
                    SIRTypeAlignOf(builder->module, dest_type);
                uint32_t source_type_align =
                    SIRTypeAlignOf(builder->module, source_type);

                if (dest_type_align == source_type_align &&
                    dest_type_size == source_type_size) {
                    inst.kind = SIRInstKind_Alias;
                    inst.op1 = inst.op1;
                    inst.op2.id = 0;
                    builder->module->insts[inst_ref.id] = inst;
                }

                break;
            }
            case SIRInstKind_ZExt: {
                SIRType *dest_type =
                    SIRModuleGetInstType(builder->module, inst_ref);
                SIRType *source_type =
                    SIRModuleGetInstType(builder->module, inst.op1);

                uint32_t dest_type_size =
                    SIRTypeSizeOf(builder->module, dest_type);
                uint32_t source_type_size =
                    SIRTypeSizeOf(builder->module, source_type);
                uint32_t dest_type_align =
                    SIRTypeAlignOf(builder->module, dest_type);
                uint32_t source_type_align =
                    SIRTypeAlignOf(builder->module, source_type);

                if (dest_type_align == source_type_align &&
                    dest_type_size == source_type_size) {
                    inst.kind = SIRInstKind_Alias;
                    inst.op1 = inst.op1;
                    inst.op2.id = 0;
                    builder->module->insts[inst_ref.id] = inst;
                }

                break;
            }
            default: break;
            }
        }
    }
}

static void generate_function(X64AsmBuilder *builder, SIRInstRef func_ref)
{
    ZoneScoped;

    SIRInst func_inst = SIRModuleGetInst(builder->module, func_ref);
    SIR_ASSERT(func_inst.kind == SIRInstKind_Function);
    SIRFunction *func = func_inst.func;
    SIR_ASSERT(func);

    MetaFunction *meta_func = SIRAlloc(builder->module->arena, MetaFunction);
    *meta_func = {};

    meta_func->jump_patches =
        SIRArray<FuncJumpPatch>::create((SIRAllocator *)builder->module->arena);

    switch (func->calling_convention) {
    case SIRCallingConvention_SystemV: {
        meta_func->callee_saved_registers_len =
            SIR_CARRAY_LENGTH(SYSV_CALLEE_SAVED);
        meta_func->callee_saved_registers = &SYSV_CALLEE_SAVED[0];

        meta_func->caller_saved_registers_len =
            SIR_CARRAY_LENGTH(SYSV_CALLER_SAVED);
        meta_func->caller_saved_registers = &SYSV_CALLER_SAVED[0];

        break;
    }
    }

    if (func->name.len > 0) {
        if (func->blocks.len == 0) {
            // Function without body
            meta_func->symbol_ref = builder->obj_builder->add_symbol(
                builder->obj_builder,
                func->name,
                SIRSectionType_None,
                SIRSymbolType_None,
                SIRLinkage_External);
        } else {
            // Function with body
            meta_func->symbol_ref = builder->obj_builder->add_symbol(
                builder->obj_builder,
                func->name,
                SIRSectionType_Text,
                SIRSymbolType_Function,
                SIRLinkage_External);
        }
    }

    MetaValue *func_meta_inst = &builder->meta_insts[func_ref.id];
    *func_meta_inst = {};
    func_meta_inst->kind = MetaValueKind_Function;
    func_meta_inst->func = meta_func;

    if (func->blocks.len == 0) {
        return;
    }

    // Aliasing pass
    inst_aliasing_pass(builder, func);

    for (SIRInstRef stack_slot_ref : func->stack_slots) {
        SIRInst stack_slot = SIRModuleGetInst(builder->module, stack_slot_ref);
        SIR_ASSERT(stack_slot.type->kind == SIRTypeKind_Pointer);
        uint32_t slot_size =
            SIRTypeSizeOf(builder->module, stack_slot.type->pointer.sub);
        uint32_t slot_align =
            SIRTypeAlignOf(builder->module, stack_slot.type->pointer.sub);
        meta_func->stack_size = SIR_ROUND_UP(slot_align, meta_func->stack_size);
        meta_func->stack_size += slot_size;

        builder->meta_insts[stack_slot_ref.id] = create_stack_ptr_value(
            slot_size, -((int32_t)meta_func->stack_size));
    }

    // Create stack space for function parameters
    for (SIRInstRef param_inst_ref : func->param_insts) {
        SIRInst param_inst = SIRModuleGetInst(builder->module, param_inst_ref);

        uint32_t inst_size = SIRTypeSizeOf(builder->module, param_inst.type);
        uint32_t inst_align = SIRTypeAlignOf(builder->module, param_inst.type);

        meta_func->stack_size = SIR_ROUND_UP(inst_align, meta_func->stack_size);
        meta_func->stack_size += inst_size;

        builder->meta_insts[param_inst_ref.id] =
            create_stack_value(inst_size, -((int32_t)meta_func->stack_size));
    }

    // Register allocation / variable spilling
    reg_stack_alloc(builder, func, meta_func);

    for (uint32_t i = 0; i < RegisterIndex_COUNT; ++i) {
        if (meta_func->registers_used[i]) {
            meta_func->stack_size = SIR_ROUND_UP(8, meta_func->stack_size);
            meta_func->stack_size += 8;

            meta_func->saved_register_stack_offset[i] =
                -((int32_t)meta_func->stack_size);
        }
    }

    size_t function_start_offset = builder->obj_builder->get_section_size(
        builder->obj_builder, SIRSectionType_Text);

    // Begin stack frame
    meta_func->stack_size = SIR_ROUND_UP(0x10, meta_func->stack_size);
    builder->encode(FE_PUSHr, FE_BP);
    builder->encode(FE_MOV64rr, FE_BP, FE_SP);
    builder->encode(FE_SUB64ri, FE_SP, meta_func->stack_size);

    // Move parameters to stack
    move_func_params_to_stack(builder, func);

    // Save callee saved registers
    for (size_t i = 0; i < meta_func->callee_saved_registers_len; ++i) {
        RegisterIndex reg_index = meta_func->callee_saved_registers[i];
        if (meta_func->registers_used[reg_index]) {
            builder->encode(
                FE_MOV64mr,
                FE_MEM(
                    FE_BP,
                    0,
                    0,
                    meta_func->saved_register_stack_offset[reg_index]),
                REGISTERS[reg_index]);
        }
    }

    builder->current_func = func_ref;

    // Generate blocks
    for (SIRInstRef block_ref : func->blocks) {
        SIRInst block = SIRModuleGetInst(builder->module, block_ref);
        MetaValue *meta_block = &builder->meta_insts[block_ref.id];
        *meta_block = {};

        meta_block->kind = MetaValueKind_Block;
        meta_block->block.offset = builder->get_code_offset();

        builder->current_block = block_ref;

        for (SIRInstRef inst_ref : block.block->inst_refs) {
            generate_inst(builder, func_ref, inst_ref);
            /* builder->encode(FE_NOP); */
        }
    }

    builder->current_func = {0};

    // Patch jumps
    for (const FuncJumpPatch &jump_patch : meta_func->jump_patches) {
        MetaValue *meta_block =
            &builder->meta_insts[jump_patch.destination_block.id];
        builder->encode_at(
            jump_patch.instruction_offset,
            jump_patch.instruction,
            meta_block->block.offset - jump_patch.instruction_offset);
    }

    size_t function_end_offset = builder->obj_builder->get_section_size(
        builder->obj_builder, SIRSectionType_Text);

    size_t function_size = function_end_offset - function_start_offset;

    builder->obj_builder->set_symbol_region(
        builder->obj_builder,
        meta_func->symbol_ref,
        function_start_offset,
        function_size);
}

static void generate(SIRAsmBuilder *asm_builder)
{
    ZoneScoped;
    X64AsmBuilder *builder = (X64AsmBuilder *)asm_builder;

    // Generate constants
    for (SIRInstRef const_ref : builder->module->consts) {
        generate_const(builder, const_ref);
    }

    // Generate globals
    for (SIRInstRef global_ref : builder->module->globals) {
        generate_global(builder, global_ref);
    }

    // Generate functions
    for (SIRInstRef func_ref : builder->module->functions) {
        generate_function(builder, func_ref);
    }
}

static void destroy(SIRAsmBuilder *asm_builder)
{
    X64AsmBuilder *builder = (X64AsmBuilder *)asm_builder;
    builder->current_func_params.destroy();
    builder->meta_insts.destroy();
}

SIRAsmBuilder *
SIRCreateX64Builder(SIRModule *module, SIRObjectBuilder *obj_builder)
{
    ZoneScoped;

    X64AsmBuilder *asm_builder = SIRAllocInit(module->arena, X64AsmBuilder);

    asm_builder->vt.generate = generate;
    asm_builder->vt.destroy = destroy;

    asm_builder->module = module;
    asm_builder->obj_builder = obj_builder;

    asm_builder->current_func_params =
        SIRArray<SIRInstRef>::create(&SIR_MALLOC_ALLOCATOR);

    asm_builder->meta_insts =
        SIRArray<MetaValue>::create(&SIR_MALLOC_ALLOCATOR);
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
    OPERAND_KINDS[MetaValueKind_IRegisterMemoryPtr] = OperandKind_MemoryPtr;
    OPERAND_KINDS[MetaValueKind_GlobalPtr] = OperandKind_MemoryPtr;

    ENCODING_ENTRIES1[Mnem_C_SEP][OperandKind_Reg][SizeClass_4] = FE_C_SEP32;
    ENCODING_ENTRIES1[Mnem_C_SEP][OperandKind_Reg][SizeClass_8] = FE_C_SEP64;

    ENCODING_ENTRIES1[Mnem_IDIV][OperandKind_Reg][SizeClass_4] = FE_IDIV32r;
    ENCODING_ENTRIES1[Mnem_IDIV][OperandKind_Reg][SizeClass_8] = FE_IDIV64r;
    ENCODING_ENTRIES1[Mnem_IDIV][OperandKind_Memory][SizeClass_4] = FE_IDIV32m;
    ENCODING_ENTRIES1[Mnem_IDIV][OperandKind_Memory][SizeClass_8] = FE_IDIV64m;

    ENCODING_ENTRIES1[Mnem_DIV][OperandKind_Reg][SizeClass_4] = FE_DIV32r;
    ENCODING_ENTRIES1[Mnem_DIV][OperandKind_Reg][SizeClass_8] = FE_DIV64r;
    ENCODING_ENTRIES1[Mnem_DIV][OperandKind_Memory][SizeClass_4] = FE_DIV32m;
    ENCODING_ENTRIES1[Mnem_DIV][OperandKind_Memory][SizeClass_8] = FE_DIV64m;

    // MOV

    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Reg][SizeClass_8][OperandKind_Reg]
                     [SizeClass_8] = FE_MOV64rr;
    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Reg][SizeClass_4][OperandKind_Reg]
                     [SizeClass_4] = FE_MOV32rr;
    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Reg][SizeClass_2][OperandKind_Reg]
                     [SizeClass_2] = FE_MOV16rr;
    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Reg][SizeClass_1][OperandKind_Reg]
                     [SizeClass_1] = FE_MOV8rr;

    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Reg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_8] = FE_MOV64rm;
    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Reg][SizeClass_4]
                     [OperandKind_Memory][SizeClass_4] = FE_MOV32rm;
    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Reg][SizeClass_2]
                     [OperandKind_Memory][SizeClass_2] = FE_MOV16rm;
    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Reg][SizeClass_1]
                     [OperandKind_Memory][SizeClass_1] = FE_MOV8rm;

    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Reg][SizeClass_8][OperandKind_Imm]
                     [SizeClass_8] = FE_MOV64ri;
    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Reg][SizeClass_4][OperandKind_Imm]
                     [SizeClass_4] = FE_MOV32ri;
    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Reg][SizeClass_2][OperandKind_Imm]
                     [SizeClass_2] = FE_MOV16ri;
    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Reg][SizeClass_1][OperandKind_Imm]
                     [SizeClass_1] = FE_MOV8ri;

    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Memory][SizeClass_8]
                     [OperandKind_Imm][SizeClass_8] = FE_MOV64mi;
    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Memory][SizeClass_4]
                     [OperandKind_Imm][SizeClass_4] = FE_MOV32mi;
    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Memory][SizeClass_2]
                     [OperandKind_Imm][SizeClass_2] = FE_MOV16mi;
    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Memory][SizeClass_1]
                     [OperandKind_Imm][SizeClass_1] = FE_MOV8mi;

    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Memory][SizeClass_8]
                     [OperandKind_Reg][SizeClass_8] = FE_MOV64mr;
    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Memory][SizeClass_4]
                     [OperandKind_Reg][SizeClass_4] = FE_MOV32mr;
    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Memory][SizeClass_2]
                     [OperandKind_Reg][SizeClass_2] = FE_MOV16mr;
    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Memory][SizeClass_1]
                     [OperandKind_Reg][SizeClass_1] = FE_MOV8mr;

    // MOV pointer to register (LEA)
    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Reg][SizeClass_8]
                     [OperandKind_MemoryPtr][SizeClass_1] = FE_LEA64rm;
    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Reg][SizeClass_8]
                     [OperandKind_MemoryPtr][SizeClass_2] = FE_LEA64rm;
    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Reg][SizeClass_8]
                     [OperandKind_MemoryPtr][SizeClass_4] = FE_LEA64rm;
    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Reg][SizeClass_8]
                     [OperandKind_MemoryPtr][SizeClass_8] = FE_LEA64rm;
    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Reg][SizeClass_8]
                     [OperandKind_MemoryPtr][SizeClass_None] = FE_LEA64rm;

    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Memory][SizeClass_8]
                     [OperandKind_MemoryPtr][SizeClass_1] = FE_LEA64rm;
    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Memory][SizeClass_8]
                     [OperandKind_MemoryPtr][SizeClass_2] = FE_LEA64rm;
    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Memory][SizeClass_8]
                     [OperandKind_MemoryPtr][SizeClass_4] = FE_LEA64rm;
    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Memory][SizeClass_8]
                     [OperandKind_MemoryPtr][SizeClass_8] = FE_LEA64rm;
    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Memory][SizeClass_8]
                     [OperandKind_MemoryPtr][SizeClass_None] = FE_LEA64rm;

    // MOVSX

    ENCODING_ENTRIES2[Mnem_MOVSX][OperandKind_Reg][SizeClass_1][OperandKind_Reg]
                     [SizeClass_1] = FE_MOV8rr;

    ENCODING_ENTRIES2[Mnem_MOVSX][OperandKind_Reg][SizeClass_1]
                     [OperandKind_Memory][SizeClass_1] = FE_MOV8rm;

    ENCODING_ENTRIES2[Mnem_MOVSX][OperandKind_Reg][SizeClass_2][OperandKind_Reg]
                     [SizeClass_1] = FE_MOVSXr16r8;
    ENCODING_ENTRIES2[Mnem_MOVSX][OperandKind_Reg][SizeClass_2][OperandKind_Reg]
                     [SizeClass_2] = FE_MOV16rr;

    ENCODING_ENTRIES2[Mnem_MOVSX][OperandKind_Reg][SizeClass_2]
                     [OperandKind_Memory][SizeClass_1] = FE_MOVSXr16m8;
    ENCODING_ENTRIES2[Mnem_MOVSX][OperandKind_Reg][SizeClass_2]
                     [OperandKind_Memory][SizeClass_2] = FE_MOV16rm;

    ENCODING_ENTRIES2[Mnem_MOVSX][OperandKind_Reg][SizeClass_4][OperandKind_Reg]
                     [SizeClass_1] = FE_MOVSXr32r8;
    ENCODING_ENTRIES2[Mnem_MOVSX][OperandKind_Reg][SizeClass_4][OperandKind_Reg]
                     [SizeClass_2] = FE_MOVSXr32r16;
    ENCODING_ENTRIES2[Mnem_MOVSX][OperandKind_Reg][SizeClass_4][OperandKind_Reg]
                     [SizeClass_4] = FE_MOV32rr;

    ENCODING_ENTRIES2[Mnem_MOVSX][OperandKind_Reg][SizeClass_4]
                     [OperandKind_Memory][SizeClass_1] = FE_MOVSXr32m8;
    ENCODING_ENTRIES2[Mnem_MOVSX][OperandKind_Reg][SizeClass_4]
                     [OperandKind_Memory][SizeClass_2] = FE_MOVSXr32m16;
    ENCODING_ENTRIES2[Mnem_MOVSX][OperandKind_Reg][SizeClass_4]
                     [OperandKind_Memory][SizeClass_4] = FE_MOV32rm;

    ENCODING_ENTRIES2[Mnem_MOVSX][OperandKind_Reg][SizeClass_8][OperandKind_Reg]
                     [SizeClass_8] = FE_MOV64rr;
    ENCODING_ENTRIES2[Mnem_MOVSX][OperandKind_Reg][SizeClass_8][OperandKind_Reg]
                     [SizeClass_4] = FE_MOVSXr64r32;
    ENCODING_ENTRIES2[Mnem_MOVSX][OperandKind_Reg][SizeClass_8][OperandKind_Reg]
                     [SizeClass_2] = FE_MOVSXr64r16;
    ENCODING_ENTRIES2[Mnem_MOVSX][OperandKind_Reg][SizeClass_8][OperandKind_Reg]
                     [SizeClass_1] = FE_MOVSXr64r8;

    ENCODING_ENTRIES2[Mnem_MOVSX][OperandKind_Reg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_8] = FE_MOV64rm;
    ENCODING_ENTRIES2[Mnem_MOVSX][OperandKind_Reg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_4] = FE_MOVSXr64m32;
    ENCODING_ENTRIES2[Mnem_MOVSX][OperandKind_Reg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_2] = FE_MOVSXr64m16;
    ENCODING_ENTRIES2[Mnem_MOVSX][OperandKind_Reg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_1] = FE_MOVSXr64m8;

    // MOVZX

    ENCODING_ENTRIES2[Mnem_MOVZX][OperandKind_Reg][SizeClass_1][OperandKind_Reg]
                     [SizeClass_1] = FE_MOV8rr;

    ENCODING_ENTRIES2[Mnem_MOVZX][OperandKind_Reg][SizeClass_1]
                     [OperandKind_Memory][SizeClass_1] = FE_MOV8rm;

    ENCODING_ENTRIES2[Mnem_MOVZX][OperandKind_Reg][SizeClass_2][OperandKind_Reg]
                     [SizeClass_1] = FE_MOVZXr16r8;
    ENCODING_ENTRIES2[Mnem_MOVZX][OperandKind_Reg][SizeClass_2][OperandKind_Reg]
                     [SizeClass_2] = FE_MOV16rr;

    ENCODING_ENTRIES2[Mnem_MOVZX][OperandKind_Reg][SizeClass_2]
                     [OperandKind_Memory][SizeClass_1] = FE_MOVZXr16m8;
    ENCODING_ENTRIES2[Mnem_MOVZX][OperandKind_Reg][SizeClass_2]
                     [OperandKind_Memory][SizeClass_2] = FE_MOV16rm;

    ENCODING_ENTRIES2[Mnem_MOVZX][OperandKind_Reg][SizeClass_4][OperandKind_Reg]
                     [SizeClass_1] = FE_MOVZXr32r8;
    ENCODING_ENTRIES2[Mnem_MOVZX][OperandKind_Reg][SizeClass_4][OperandKind_Reg]
                     [SizeClass_2] = FE_MOVZXr32r16;
    ENCODING_ENTRIES2[Mnem_MOVZX][OperandKind_Reg][SizeClass_4][OperandKind_Reg]
                     [SizeClass_4] = FE_MOV32rr;

    ENCODING_ENTRIES2[Mnem_MOVZX][OperandKind_Reg][SizeClass_4]
                     [OperandKind_Memory][SizeClass_1] = FE_MOVZXr32m8;
    ENCODING_ENTRIES2[Mnem_MOVZX][OperandKind_Reg][SizeClass_4]
                     [OperandKind_Memory][SizeClass_2] = FE_MOVZXr32m16;
    ENCODING_ENTRIES2[Mnem_MOVZX][OperandKind_Reg][SizeClass_4]
                     [OperandKind_Memory][SizeClass_4] = FE_MOV32rm;

    ENCODING_ENTRIES2[Mnem_MOVZX][OperandKind_Reg][SizeClass_8][OperandKind_Reg]
                     [SizeClass_1] = FE_MOVZXr64r8;
    ENCODING_ENTRIES2[Mnem_MOVZX][OperandKind_Reg][SizeClass_8][OperandKind_Reg]
                     [SizeClass_2] = FE_MOVZXr64r16;
    ENCODING_ENTRIES2[Mnem_MOVZX][OperandKind_Reg][SizeClass_8][OperandKind_Reg]
                     [SizeClass_4] = FE_MOV32rr;
    ENCODING_ENTRIES2[Mnem_MOVZX][OperandKind_Reg][SizeClass_8][OperandKind_Reg]
                     [SizeClass_8] = FE_MOV64rr;

    ENCODING_ENTRIES2[Mnem_MOVZX][OperandKind_Reg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_1] = FE_MOVZXr64m8;
    ENCODING_ENTRIES2[Mnem_MOVZX][OperandKind_Reg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_2] = FE_MOVZXr64m16;
    ENCODING_ENTRIES2[Mnem_MOVZX][OperandKind_Reg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_4] = FE_MOV32rm;
    ENCODING_ENTRIES2[Mnem_MOVZX][OperandKind_Reg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_8] = FE_MOV64rm;

    // MOVS

    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_FReg][SizeClass_4][OperandKind_FReg]
                     [SizeClass_4] = FE_SSE_MOVSSrr;
    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_FReg][SizeClass_8][OperandKind_FReg]
                     [SizeClass_8] = FE_SSE_MOVSDrr;

    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_FReg][SizeClass_4]
                     [OperandKind_Memory][SizeClass_4] = FE_SSE_MOVSSrm;
    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_FReg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_8] = FE_SSE_MOVSDrm;

    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Memory][SizeClass_4]
                     [OperandKind_FReg][SizeClass_4] = FE_SSE_MOVSSmr;
    ENCODING_ENTRIES2[Mnem_MOV][OperandKind_Memory][SizeClass_8]
                     [OperandKind_FReg][SizeClass_8] = FE_SSE_MOVSDmr;

    // LEA

    ENCODING_ENTRIES2[Mnem_LEA][OperandKind_Reg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_1] = FE_LEA64rm;
    ENCODING_ENTRIES2[Mnem_LEA][OperandKind_Reg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_2] = FE_LEA64rm;
    ENCODING_ENTRIES2[Mnem_LEA][OperandKind_Reg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_4] = FE_LEA64rm;
    ENCODING_ENTRIES2[Mnem_LEA][OperandKind_Reg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_8] = FE_LEA64rm;
    ENCODING_ENTRIES2[Mnem_LEA][OperandKind_Reg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_None] = FE_LEA64rm;

    ENCODING_ENTRIES2[Mnem_LEA][OperandKind_Memory][SizeClass_8]
                     [OperandKind_Memory][SizeClass_1] = FE_LEA64rm;
    ENCODING_ENTRIES2[Mnem_LEA][OperandKind_Memory][SizeClass_8]
                     [OperandKind_Memory][SizeClass_2] = FE_LEA64rm;
    ENCODING_ENTRIES2[Mnem_LEA][OperandKind_Memory][SizeClass_8]
                     [OperandKind_Memory][SizeClass_4] = FE_LEA64rm;
    ENCODING_ENTRIES2[Mnem_LEA][OperandKind_Memory][SizeClass_8]
                     [OperandKind_Memory][SizeClass_8] = FE_LEA64rm;
    ENCODING_ENTRIES2[Mnem_LEA][OperandKind_Memory][SizeClass_8]
                     [OperandKind_Memory][SizeClass_None] = FE_LEA64rm;

    // ADD

    ENCODING_ENTRIES2[Mnem_ADD][OperandKind_Memory][SizeClass_8]
                     [OperandKind_Reg][SizeClass_8] = FE_ADD64mr;
    ENCODING_ENTRIES2[Mnem_ADD][OperandKind_Memory][SizeClass_4]
                     [OperandKind_Reg][SizeClass_4] = FE_ADD32mr;
    ENCODING_ENTRIES2[Mnem_ADD][OperandKind_Memory][SizeClass_2]
                     [OperandKind_Reg][SizeClass_2] = FE_ADD16mr;
    ENCODING_ENTRIES2[Mnem_ADD][OperandKind_Memory][SizeClass_1]
                     [OperandKind_Reg][SizeClass_1] = FE_ADD8mr;

    ENCODING_ENTRIES2[Mnem_ADD][OperandKind_Reg][SizeClass_8][OperandKind_Reg]
                     [SizeClass_8] = FE_ADD64rr;
    ENCODING_ENTRIES2[Mnem_ADD][OperandKind_Reg][SizeClass_4][OperandKind_Reg]
                     [SizeClass_4] = FE_ADD32rr;
    ENCODING_ENTRIES2[Mnem_ADD][OperandKind_Reg][SizeClass_2][OperandKind_Reg]
                     [SizeClass_2] = FE_ADD16rr;
    ENCODING_ENTRIES2[Mnem_ADD][OperandKind_Reg][SizeClass_1][OperandKind_Reg]
                     [SizeClass_1] = FE_ADD8rr;

    ENCODING_ENTRIES2[Mnem_ADD][OperandKind_Reg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_8] = FE_ADD64rm;
    ENCODING_ENTRIES2[Mnem_ADD][OperandKind_Reg][SizeClass_4]
                     [OperandKind_Memory][SizeClass_4] = FE_ADD32rm;
    ENCODING_ENTRIES2[Mnem_ADD][OperandKind_Reg][SizeClass_2]
                     [OperandKind_Memory][SizeClass_2] = FE_ADD16rm;
    ENCODING_ENTRIES2[Mnem_ADD][OperandKind_Reg][SizeClass_1]
                     [OperandKind_Memory][SizeClass_1] = FE_ADD8rm;

    ENCODING_ENTRIES2[Mnem_ADD][OperandKind_Reg][SizeClass_8][OperandKind_Imm]
                     [SizeClass_8] = FE_ADD64ri;
    ENCODING_ENTRIES2[Mnem_ADD][OperandKind_Reg][SizeClass_4][OperandKind_Imm]
                     [SizeClass_4] = FE_ADD32ri;
    ENCODING_ENTRIES2[Mnem_ADD][OperandKind_Reg][SizeClass_2][OperandKind_Imm]
                     [SizeClass_2] = FE_ADD16ri;
    ENCODING_ENTRIES2[Mnem_ADD][OperandKind_Reg][SizeClass_1][OperandKind_Imm]
                     [SizeClass_1] = FE_ADD8ri;

    ENCODING_ENTRIES2[Mnem_ADD][OperandKind_Memory][SizeClass_8]
                     [OperandKind_Imm][SizeClass_8] = FE_ADD64mi;
    ENCODING_ENTRIES2[Mnem_ADD][OperandKind_Memory][SizeClass_4]
                     [OperandKind_Imm][SizeClass_4] = FE_ADD32mi;
    ENCODING_ENTRIES2[Mnem_ADD][OperandKind_Memory][SizeClass_2]
                     [OperandKind_Imm][SizeClass_2] = FE_ADD16mi;
    ENCODING_ENTRIES2[Mnem_ADD][OperandKind_Memory][SizeClass_1]
                     [OperandKind_Imm][SizeClass_1] = FE_ADD8mi;

    // SUB

    ENCODING_ENTRIES2[Mnem_SUB][OperandKind_Memory][SizeClass_8]
                     [OperandKind_Reg][SizeClass_8] = FE_SUB64mr;
    ENCODING_ENTRIES2[Mnem_SUB][OperandKind_Memory][SizeClass_4]
                     [OperandKind_Reg][SizeClass_4] = FE_SUB32mr;
    ENCODING_ENTRIES2[Mnem_SUB][OperandKind_Memory][SizeClass_2]
                     [OperandKind_Reg][SizeClass_2] = FE_SUB16mr;
    ENCODING_ENTRIES2[Mnem_SUB][OperandKind_Memory][SizeClass_1]
                     [OperandKind_Reg][SizeClass_1] = FE_SUB8mr;

    ENCODING_ENTRIES2[Mnem_SUB][OperandKind_Reg][SizeClass_8][OperandKind_Reg]
                     [SizeClass_8] = FE_SUB64rr;
    ENCODING_ENTRIES2[Mnem_SUB][OperandKind_Reg][SizeClass_4][OperandKind_Reg]
                     [SizeClass_4] = FE_SUB32rr;
    ENCODING_ENTRIES2[Mnem_SUB][OperandKind_Reg][SizeClass_2][OperandKind_Reg]
                     [SizeClass_2] = FE_SUB16rr;
    ENCODING_ENTRIES2[Mnem_SUB][OperandKind_Reg][SizeClass_1][OperandKind_Reg]
                     [SizeClass_1] = FE_SUB8rr;

    ENCODING_ENTRIES2[Mnem_SUB][OperandKind_Reg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_8] = FE_SUB64rm;
    ENCODING_ENTRIES2[Mnem_SUB][OperandKind_Reg][SizeClass_4]
                     [OperandKind_Memory][SizeClass_4] = FE_SUB32rm;
    ENCODING_ENTRIES2[Mnem_SUB][OperandKind_Reg][SizeClass_2]
                     [OperandKind_Memory][SizeClass_2] = FE_SUB16rm;
    ENCODING_ENTRIES2[Mnem_SUB][OperandKind_Reg][SizeClass_1]
                     [OperandKind_Memory][SizeClass_1] = FE_SUB8rm;

    ENCODING_ENTRIES2[Mnem_SUB][OperandKind_Reg][SizeClass_8][OperandKind_Imm]
                     [SizeClass_8] = FE_SUB64ri;
    ENCODING_ENTRIES2[Mnem_SUB][OperandKind_Reg][SizeClass_4][OperandKind_Imm]
                     [SizeClass_4] = FE_SUB32ri;
    ENCODING_ENTRIES2[Mnem_SUB][OperandKind_Reg][SizeClass_2][OperandKind_Imm]
                     [SizeClass_2] = FE_SUB16ri;
    ENCODING_ENTRIES2[Mnem_SUB][OperandKind_Reg][SizeClass_1][OperandKind_Imm]
                     [SizeClass_1] = FE_SUB8ri;

    ENCODING_ENTRIES2[Mnem_SUB][OperandKind_Memory][SizeClass_8]
                     [OperandKind_Imm][SizeClass_8] = FE_SUB64mi;
    ENCODING_ENTRIES2[Mnem_SUB][OperandKind_Memory][SizeClass_4]
                     [OperandKind_Imm][SizeClass_4] = FE_SUB32mi;
    ENCODING_ENTRIES2[Mnem_SUB][OperandKind_Memory][SizeClass_2]
                     [OperandKind_Imm][SizeClass_2] = FE_SUB16mi;
    ENCODING_ENTRIES2[Mnem_SUB][OperandKind_Memory][SizeClass_1]
                     [OperandKind_Imm][SizeClass_1] = FE_SUB8mi;

    // MUL

    ENCODING_ENTRIES2[Mnem_MUL][OperandKind_Reg][SizeClass_8][OperandKind_Reg]
                     [SizeClass_8] = FE_IMUL64rr;
    ENCODING_ENTRIES2[Mnem_MUL][OperandKind_Reg][SizeClass_4][OperandKind_Reg]
                     [SizeClass_4] = FE_IMUL32rr;

    // AND

    ENCODING_ENTRIES2[Mnem_AND][OperandKind_Reg][SizeClass_1][OperandKind_Reg]
                     [SizeClass_1] = FE_AND8rr;
    ENCODING_ENTRIES2[Mnem_AND][OperandKind_Reg][SizeClass_2][OperandKind_Reg]
                     [SizeClass_2] = FE_AND16rr;
    ENCODING_ENTRIES2[Mnem_AND][OperandKind_Reg][SizeClass_4][OperandKind_Reg]
                     [SizeClass_4] = FE_AND32rr;
    ENCODING_ENTRIES2[Mnem_AND][OperandKind_Reg][SizeClass_8][OperandKind_Reg]
                     [SizeClass_8] = FE_AND64rr;

    ENCODING_ENTRIES2[Mnem_AND][OperandKind_Reg][SizeClass_1][OperandKind_Imm]
                     [SizeClass_1] = FE_AND8ri;
    ENCODING_ENTRIES2[Mnem_AND][OperandKind_Reg][SizeClass_2][OperandKind_Imm]
                     [SizeClass_2] = FE_AND16ri;
    ENCODING_ENTRIES2[Mnem_AND][OperandKind_Reg][SizeClass_4][OperandKind_Imm]
                     [SizeClass_4] = FE_AND32ri;
    ENCODING_ENTRIES2[Mnem_AND][OperandKind_Reg][SizeClass_8][OperandKind_Imm]
                     [SizeClass_8] = FE_AND64ri;

    ENCODING_ENTRIES2[Mnem_AND][OperandKind_Reg][SizeClass_1]
                     [OperandKind_Memory][SizeClass_1] = FE_AND8rm;
    ENCODING_ENTRIES2[Mnem_AND][OperandKind_Reg][SizeClass_2]
                     [OperandKind_Memory][SizeClass_2] = FE_AND16rm;
    ENCODING_ENTRIES2[Mnem_AND][OperandKind_Reg][SizeClass_4]
                     [OperandKind_Memory][SizeClass_4] = FE_AND32rm;
    ENCODING_ENTRIES2[Mnem_AND][OperandKind_Reg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_8] = FE_AND64rm;

    ENCODING_ENTRIES2[Mnem_AND][OperandKind_Memory][SizeClass_1]
                     [OperandKind_Reg][SizeClass_1] = FE_AND8mr;
    ENCODING_ENTRIES2[Mnem_AND][OperandKind_Memory][SizeClass_2]
                     [OperandKind_Reg][SizeClass_2] = FE_AND16mr;
    ENCODING_ENTRIES2[Mnem_AND][OperandKind_Memory][SizeClass_4]
                     [OperandKind_Reg][SizeClass_4] = FE_AND32mr;
    ENCODING_ENTRIES2[Mnem_AND][OperandKind_Memory][SizeClass_8]
                     [OperandKind_Reg][SizeClass_8] = FE_AND64mr;

    ENCODING_ENTRIES2[Mnem_AND][OperandKind_Memory][SizeClass_1]
                     [OperandKind_Imm][SizeClass_1] = FE_AND8mi;
    ENCODING_ENTRIES2[Mnem_AND][OperandKind_Memory][SizeClass_2]
                     [OperandKind_Imm][SizeClass_2] = FE_AND16mi;
    ENCODING_ENTRIES2[Mnem_AND][OperandKind_Memory][SizeClass_4]
                     [OperandKind_Imm][SizeClass_4] = FE_AND32mi;
    ENCODING_ENTRIES2[Mnem_AND][OperandKind_Memory][SizeClass_8]
                     [OperandKind_Imm][SizeClass_8] = FE_AND64mi;

    // OR

    ENCODING_ENTRIES2[Mnem_OR][OperandKind_Reg][SizeClass_1][OperandKind_Reg]
                     [SizeClass_1] = FE_OR8rr;
    ENCODING_ENTRIES2[Mnem_OR][OperandKind_Reg][SizeClass_2][OperandKind_Reg]
                     [SizeClass_2] = FE_OR16rr;
    ENCODING_ENTRIES2[Mnem_OR][OperandKind_Reg][SizeClass_4][OperandKind_Reg]
                     [SizeClass_4] = FE_OR32rr;
    ENCODING_ENTRIES2[Mnem_OR][OperandKind_Reg][SizeClass_8][OperandKind_Reg]
                     [SizeClass_8] = FE_OR64rr;

    ENCODING_ENTRIES2[Mnem_OR][OperandKind_Reg][SizeClass_1][OperandKind_Imm]
                     [SizeClass_1] = FE_OR8ri;
    ENCODING_ENTRIES2[Mnem_OR][OperandKind_Reg][SizeClass_2][OperandKind_Imm]
                     [SizeClass_2] = FE_OR16ri;
    ENCODING_ENTRIES2[Mnem_OR][OperandKind_Reg][SizeClass_4][OperandKind_Imm]
                     [SizeClass_4] = FE_OR32ri;
    ENCODING_ENTRIES2[Mnem_OR][OperandKind_Reg][SizeClass_8][OperandKind_Imm]
                     [SizeClass_8] = FE_OR64ri;

    ENCODING_ENTRIES2[Mnem_OR][OperandKind_Reg][SizeClass_1][OperandKind_Memory]
                     [SizeClass_1] = FE_OR8rm;
    ENCODING_ENTRIES2[Mnem_OR][OperandKind_Reg][SizeClass_2][OperandKind_Memory]
                     [SizeClass_2] = FE_OR16rm;
    ENCODING_ENTRIES2[Mnem_OR][OperandKind_Reg][SizeClass_4][OperandKind_Memory]
                     [SizeClass_4] = FE_OR32rm;
    ENCODING_ENTRIES2[Mnem_OR][OperandKind_Reg][SizeClass_8][OperandKind_Memory]
                     [SizeClass_8] = FE_OR64rm;

    ENCODING_ENTRIES2[Mnem_OR][OperandKind_Memory][SizeClass_1][OperandKind_Reg]
                     [SizeClass_1] = FE_OR8mr;
    ENCODING_ENTRIES2[Mnem_OR][OperandKind_Memory][SizeClass_2][OperandKind_Reg]
                     [SizeClass_2] = FE_OR16mr;
    ENCODING_ENTRIES2[Mnem_OR][OperandKind_Memory][SizeClass_4][OperandKind_Reg]
                     [SizeClass_4] = FE_OR32mr;
    ENCODING_ENTRIES2[Mnem_OR][OperandKind_Memory][SizeClass_8][OperandKind_Reg]
                     [SizeClass_8] = FE_OR64mr;

    ENCODING_ENTRIES2[Mnem_OR][OperandKind_Memory][SizeClass_1][OperandKind_Imm]
                     [SizeClass_1] = FE_OR8mi;
    ENCODING_ENTRIES2[Mnem_OR][OperandKind_Memory][SizeClass_2][OperandKind_Imm]
                     [SizeClass_2] = FE_OR16mi;
    ENCODING_ENTRIES2[Mnem_OR][OperandKind_Memory][SizeClass_4][OperandKind_Imm]
                     [SizeClass_4] = FE_OR32mi;
    ENCODING_ENTRIES2[Mnem_OR][OperandKind_Memory][SizeClass_8][OperandKind_Imm]
                     [SizeClass_8] = FE_OR64mi;

    // XOR

    ENCODING_ENTRIES2[Mnem_XOR][OperandKind_Reg][SizeClass_1][OperandKind_Reg]
                     [SizeClass_1] = FE_XOR8rr;
    ENCODING_ENTRIES2[Mnem_XOR][OperandKind_Reg][SizeClass_2][OperandKind_Reg]
                     [SizeClass_2] = FE_XOR16rr;
    ENCODING_ENTRIES2[Mnem_XOR][OperandKind_Reg][SizeClass_4][OperandKind_Reg]
                     [SizeClass_4] = FE_XOR32rr;
    ENCODING_ENTRIES2[Mnem_XOR][OperandKind_Reg][SizeClass_8][OperandKind_Reg]
                     [SizeClass_8] = FE_XOR64rr;

    ENCODING_ENTRIES2[Mnem_XOR][OperandKind_Reg][SizeClass_1][OperandKind_Imm]
                     [SizeClass_1] = FE_XOR8ri;
    ENCODING_ENTRIES2[Mnem_XOR][OperandKind_Reg][SizeClass_2][OperandKind_Imm]
                     [SizeClass_2] = FE_XOR16ri;
    ENCODING_ENTRIES2[Mnem_XOR][OperandKind_Reg][SizeClass_4][OperandKind_Imm]
                     [SizeClass_4] = FE_XOR32ri;
    ENCODING_ENTRIES2[Mnem_XOR][OperandKind_Reg][SizeClass_8][OperandKind_Imm]
                     [SizeClass_8] = FE_XOR64ri;

    ENCODING_ENTRIES2[Mnem_XOR][OperandKind_Reg][SizeClass_1]
                     [OperandKind_Memory][SizeClass_1] = FE_XOR8rm;
    ENCODING_ENTRIES2[Mnem_XOR][OperandKind_Reg][SizeClass_2]
                     [OperandKind_Memory][SizeClass_2] = FE_XOR16rm;
    ENCODING_ENTRIES2[Mnem_XOR][OperandKind_Reg][SizeClass_4]
                     [OperandKind_Memory][SizeClass_4] = FE_XOR32rm;
    ENCODING_ENTRIES2[Mnem_XOR][OperandKind_Reg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_8] = FE_XOR64rm;

    ENCODING_ENTRIES2[Mnem_XOR][OperandKind_Memory][SizeClass_1]
                     [OperandKind_Reg][SizeClass_1] = FE_XOR8mr;
    ENCODING_ENTRIES2[Mnem_XOR][OperandKind_Memory][SizeClass_2]
                     [OperandKind_Reg][SizeClass_2] = FE_XOR16mr;
    ENCODING_ENTRIES2[Mnem_XOR][OperandKind_Memory][SizeClass_4]
                     [OperandKind_Reg][SizeClass_4] = FE_XOR32mr;
    ENCODING_ENTRIES2[Mnem_XOR][OperandKind_Memory][SizeClass_8]
                     [OperandKind_Reg][SizeClass_8] = FE_XOR64mr;

    ENCODING_ENTRIES2[Mnem_XOR][OperandKind_Memory][SizeClass_1]
                     [OperandKind_Imm][SizeClass_1] = FE_XOR8mi;
    ENCODING_ENTRIES2[Mnem_XOR][OperandKind_Memory][SizeClass_2]
                     [OperandKind_Imm][SizeClass_2] = FE_XOR16mi;
    ENCODING_ENTRIES2[Mnem_XOR][OperandKind_Memory][SizeClass_4]
                     [OperandKind_Imm][SizeClass_4] = FE_XOR32mi;
    ENCODING_ENTRIES2[Mnem_XOR][OperandKind_Memory][SizeClass_8]
                     [OperandKind_Imm][SizeClass_8] = FE_XOR64mi;

    // SHL

    ENCODING_ENTRIES2[Mnem_SHL][OperandKind_Reg][SizeClass_1][OperandKind_Reg]
                     [SizeClass_1] = FE_SHL8rr;
    ENCODING_ENTRIES2[Mnem_SHL][OperandKind_Reg][SizeClass_2][OperandKind_Reg]
                     [SizeClass_1] = FE_SHL16rr;
    ENCODING_ENTRIES2[Mnem_SHL][OperandKind_Reg][SizeClass_4][OperandKind_Reg]
                     [SizeClass_1] = FE_SHL32rr;
    ENCODING_ENTRIES2[Mnem_SHL][OperandKind_Reg][SizeClass_8][OperandKind_Reg]
                     [SizeClass_1] = FE_SHL64rr;

    ENCODING_ENTRIES2[Mnem_SHL][OperandKind_Reg][SizeClass_1][OperandKind_Imm]
                     [SizeClass_1] = FE_SHL8ri;
    ENCODING_ENTRIES2[Mnem_SHL][OperandKind_Reg][SizeClass_2][OperandKind_Imm]
                     [SizeClass_1] = FE_SHL16ri;
    ENCODING_ENTRIES2[Mnem_SHL][OperandKind_Reg][SizeClass_4][OperandKind_Imm]
                     [SizeClass_1] = FE_SHL32ri;
    ENCODING_ENTRIES2[Mnem_SHL][OperandKind_Reg][SizeClass_8][OperandKind_Imm]
                     [SizeClass_1] = FE_SHL64ri;

    ENCODING_ENTRIES2[Mnem_SHL][OperandKind_Memory][SizeClass_1]
                     [OperandKind_Reg][SizeClass_1] = FE_SHL8mr;
    ENCODING_ENTRIES2[Mnem_SHL][OperandKind_Memory][SizeClass_2]
                     [OperandKind_Reg][SizeClass_1] = FE_SHL16mr;
    ENCODING_ENTRIES2[Mnem_SHL][OperandKind_Memory][SizeClass_4]
                     [OperandKind_Reg][SizeClass_1] = FE_SHL32mr;
    ENCODING_ENTRIES2[Mnem_SHL][OperandKind_Memory][SizeClass_8]
                     [OperandKind_Reg][SizeClass_1] = FE_SHL64mr;

    ENCODING_ENTRIES2[Mnem_SHL][OperandKind_Memory][SizeClass_1]
                     [OperandKind_Imm][SizeClass_1] = FE_SHL8mi;
    ENCODING_ENTRIES2[Mnem_SHL][OperandKind_Memory][SizeClass_2]
                     [OperandKind_Imm][SizeClass_1] = FE_SHL16mi;
    ENCODING_ENTRIES2[Mnem_SHL][OperandKind_Memory][SizeClass_4]
                     [OperandKind_Imm][SizeClass_1] = FE_SHL32mi;
    ENCODING_ENTRIES2[Mnem_SHL][OperandKind_Memory][SizeClass_8]
                     [OperandKind_Imm][SizeClass_1] = FE_SHL64mi;

    // SAR

    ENCODING_ENTRIES2[Mnem_SAR][OperandKind_Reg][SizeClass_1][OperandKind_Reg]
                     [SizeClass_1] = FE_SAR8rr;
    ENCODING_ENTRIES2[Mnem_SAR][OperandKind_Reg][SizeClass_2][OperandKind_Reg]
                     [SizeClass_1] = FE_SAR16rr;
    ENCODING_ENTRIES2[Mnem_SAR][OperandKind_Reg][SizeClass_4][OperandKind_Reg]
                     [SizeClass_1] = FE_SAR32rr;
    ENCODING_ENTRIES2[Mnem_SAR][OperandKind_Reg][SizeClass_8][OperandKind_Reg]
                     [SizeClass_1] = FE_SAR64rr;

    ENCODING_ENTRIES2[Mnem_SAR][OperandKind_Reg][SizeClass_1][OperandKind_Imm]
                     [SizeClass_1] = FE_SAR8ri;
    ENCODING_ENTRIES2[Mnem_SAR][OperandKind_Reg][SizeClass_2][OperandKind_Imm]
                     [SizeClass_1] = FE_SAR16ri;
    ENCODING_ENTRIES2[Mnem_SAR][OperandKind_Reg][SizeClass_4][OperandKind_Imm]
                     [SizeClass_1] = FE_SAR32ri;
    ENCODING_ENTRIES2[Mnem_SAR][OperandKind_Reg][SizeClass_8][OperandKind_Imm]
                     [SizeClass_1] = FE_SAR64ri;

    ENCODING_ENTRIES2[Mnem_SAR][OperandKind_Memory][SizeClass_1]
                     [OperandKind_Reg][SizeClass_1] = FE_SAR8mr;
    ENCODING_ENTRIES2[Mnem_SAR][OperandKind_Memory][SizeClass_2]
                     [OperandKind_Reg][SizeClass_1] = FE_SAR16mr;
    ENCODING_ENTRIES2[Mnem_SAR][OperandKind_Memory][SizeClass_4]
                     [OperandKind_Reg][SizeClass_1] = FE_SAR32mr;
    ENCODING_ENTRIES2[Mnem_SAR][OperandKind_Memory][SizeClass_8]
                     [OperandKind_Reg][SizeClass_1] = FE_SAR64mr;

    ENCODING_ENTRIES2[Mnem_SAR][OperandKind_Memory][SizeClass_1]
                     [OperandKind_Imm][SizeClass_1] = FE_SAR8mi;
    ENCODING_ENTRIES2[Mnem_SAR][OperandKind_Memory][SizeClass_2]
                     [OperandKind_Imm][SizeClass_1] = FE_SAR16mi;
    ENCODING_ENTRIES2[Mnem_SAR][OperandKind_Memory][SizeClass_4]
                     [OperandKind_Imm][SizeClass_1] = FE_SAR32mi;
    ENCODING_ENTRIES2[Mnem_SAR][OperandKind_Memory][SizeClass_8]
                     [OperandKind_Imm][SizeClass_1] = FE_SAR64mi;

    // SHR

    ENCODING_ENTRIES2[Mnem_SHR][OperandKind_Reg][SizeClass_1][OperandKind_Reg]
                     [SizeClass_1] = FE_SHR8rr;
    ENCODING_ENTRIES2[Mnem_SHR][OperandKind_Reg][SizeClass_2][OperandKind_Reg]
                     [SizeClass_1] = FE_SHR16rr;
    ENCODING_ENTRIES2[Mnem_SHR][OperandKind_Reg][SizeClass_4][OperandKind_Reg]
                     [SizeClass_1] = FE_SHR32rr;
    ENCODING_ENTRIES2[Mnem_SHR][OperandKind_Reg][SizeClass_8][OperandKind_Reg]
                     [SizeClass_1] = FE_SHR64rr;

    ENCODING_ENTRIES2[Mnem_SHR][OperandKind_Reg][SizeClass_1][OperandKind_Imm]
                     [SizeClass_1] = FE_SHR8ri;
    ENCODING_ENTRIES2[Mnem_SHR][OperandKind_Reg][SizeClass_2][OperandKind_Imm]
                     [SizeClass_1] = FE_SHR16ri;
    ENCODING_ENTRIES2[Mnem_SHR][OperandKind_Reg][SizeClass_4][OperandKind_Imm]
                     [SizeClass_1] = FE_SHR32ri;
    ENCODING_ENTRIES2[Mnem_SHR][OperandKind_Reg][SizeClass_8][OperandKind_Imm]
                     [SizeClass_1] = FE_SHR64ri;

    ENCODING_ENTRIES2[Mnem_SHR][OperandKind_Memory][SizeClass_1]
                     [OperandKind_Reg][SizeClass_1] = FE_SHR8mr;
    ENCODING_ENTRIES2[Mnem_SHR][OperandKind_Memory][SizeClass_2]
                     [OperandKind_Reg][SizeClass_1] = FE_SHR16mr;
    ENCODING_ENTRIES2[Mnem_SHR][OperandKind_Memory][SizeClass_4]
                     [OperandKind_Reg][SizeClass_1] = FE_SHR32mr;
    ENCODING_ENTRIES2[Mnem_SHR][OperandKind_Memory][SizeClass_8]
                     [OperandKind_Reg][SizeClass_1] = FE_SHR64mr;

    ENCODING_ENTRIES2[Mnem_SHR][OperandKind_Memory][SizeClass_1]
                     [OperandKind_Imm][SizeClass_1] = FE_SHR8mi;
    ENCODING_ENTRIES2[Mnem_SHR][OperandKind_Memory][SizeClass_2]
                     [OperandKind_Imm][SizeClass_1] = FE_SHR16mi;
    ENCODING_ENTRIES2[Mnem_SHR][OperandKind_Memory][SizeClass_4]
                     [OperandKind_Imm][SizeClass_1] = FE_SHR32mi;
    ENCODING_ENTRIES2[Mnem_SHR][OperandKind_Memory][SizeClass_8]
                     [OperandKind_Imm][SizeClass_1] = FE_SHR64mi;

    // SSE ADD

    ENCODING_ENTRIES2[Mnem_SSE_ADD][OperandKind_FReg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_8] = FE_SSE_ADDSDrm;
    ENCODING_ENTRIES2[Mnem_SSE_ADD][OperandKind_FReg][SizeClass_4]
                     [OperandKind_Memory][SizeClass_4] = FE_SSE_ADDSSrm;

    ENCODING_ENTRIES2[Mnem_SSE_ADD][OperandKind_FReg][SizeClass_8]
                     [OperandKind_FReg][SizeClass_8] = FE_SSE_ADDSDrr;
    ENCODING_ENTRIES2[Mnem_SSE_ADD][OperandKind_FReg][SizeClass_4]
                     [OperandKind_FReg][SizeClass_4] = FE_SSE_ADDSSrr;

    // SSE SUB

    ENCODING_ENTRIES2[Mnem_SSE_SUB][OperandKind_FReg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_8] = FE_SSE_SUBSDrm;
    ENCODING_ENTRIES2[Mnem_SSE_SUB][OperandKind_FReg][SizeClass_4]
                     [OperandKind_Memory][SizeClass_4] = FE_SSE_SUBSSrm;

    ENCODING_ENTRIES2[Mnem_SSE_SUB][OperandKind_FReg][SizeClass_8]
                     [OperandKind_FReg][SizeClass_8] = FE_SSE_SUBSDrr;
    ENCODING_ENTRIES2[Mnem_SSE_SUB][OperandKind_FReg][SizeClass_4]
                     [OperandKind_FReg][SizeClass_4] = FE_SSE_SUBSSrr;

    // SSE MUL

    ENCODING_ENTRIES2[Mnem_SSE_MUL][OperandKind_FReg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_8] = FE_SSE_MULSDrm;
    ENCODING_ENTRIES2[Mnem_SSE_MUL][OperandKind_FReg][SizeClass_4]
                     [OperandKind_Memory][SizeClass_4] = FE_SSE_MULSSrm;

    ENCODING_ENTRIES2[Mnem_SSE_MUL][OperandKind_FReg][SizeClass_8]
                     [OperandKind_FReg][SizeClass_8] = FE_SSE_MULSDrr;
    ENCODING_ENTRIES2[Mnem_SSE_MUL][OperandKind_FReg][SizeClass_4]
                     [OperandKind_FReg][SizeClass_4] = FE_SSE_MULSSrr;

    // SSE DIV

    ENCODING_ENTRIES2[Mnem_SSE_DIV][OperandKind_FReg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_8] = FE_SSE_DIVSDrm;
    ENCODING_ENTRIES2[Mnem_SSE_DIV][OperandKind_FReg][SizeClass_4]
                     [OperandKind_Memory][SizeClass_4] = FE_SSE_DIVSSrm;

    ENCODING_ENTRIES2[Mnem_SSE_DIV][OperandKind_FReg][SizeClass_8]
                     [OperandKind_FReg][SizeClass_8] = FE_SSE_DIVSDrr;
    ENCODING_ENTRIES2[Mnem_SSE_DIV][OperandKind_FReg][SizeClass_4]
                     [OperandKind_FReg][SizeClass_4] = FE_SSE_DIVSSrr;

    // SSE UCOMIS

    ENCODING_ENTRIES2[Mnem_SSE_UCOMIS][OperandKind_FReg][SizeClass_8]
                     [OperandKind_FReg][SizeClass_8] = FE_SSE_UCOMISDrr;
    ENCODING_ENTRIES2[Mnem_SSE_UCOMIS][OperandKind_FReg][SizeClass_4]
                     [OperandKind_FReg][SizeClass_4] = FE_SSE_UCOMISSrr;

    ENCODING_ENTRIES2[Mnem_SSE_UCOMIS][OperandKind_FReg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_8] = FE_SSE_UCOMISDrm;
    ENCODING_ENTRIES2[Mnem_SSE_UCOMIS][OperandKind_FReg][SizeClass_4]
                     [OperandKind_Memory][SizeClass_4] = FE_SSE_UCOMISSrm;

    // SSE CVTS

    ENCODING_ENTRIES2[Mnem_SSE_CVTS][OperandKind_FReg][SizeClass_8]
                     [OperandKind_FReg][SizeClass_4] = FE_SSE_CVTSS2SDrr;
    ENCODING_ENTRIES2[Mnem_SSE_CVTS][OperandKind_FReg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_4] = FE_SSE_CVTSS2SDrm;

    ENCODING_ENTRIES2[Mnem_SSE_CVTS][OperandKind_FReg][SizeClass_4]
                     [OperandKind_FReg][SizeClass_8] = FE_SSE_CVTSD2SSrr;
    ENCODING_ENTRIES2[Mnem_SSE_CVTS][OperandKind_FReg][SizeClass_4]
                     [OperandKind_Memory][SizeClass_8] = FE_SSE_CVTSD2SSrm;

    // SSE CVTSI2S

    ENCODING_ENTRIES2[Mnem_SSE_CVTSI2S][OperandKind_FReg][SizeClass_4]
                     [OperandKind_Reg][SizeClass_4] = FE_SSE_CVTSI2SS32rr;
    ENCODING_ENTRIES2[Mnem_SSE_CVTSI2S][OperandKind_FReg][SizeClass_4]
                     [OperandKind_Memory][SizeClass_4] = FE_SSE_CVTSI2SS32rm;

    ENCODING_ENTRIES2[Mnem_SSE_CVTSI2S][OperandKind_FReg][SizeClass_4]
                     [OperandKind_Reg][SizeClass_8] = FE_SSE_CVTSI2SS64rr;
    ENCODING_ENTRIES2[Mnem_SSE_CVTSI2S][OperandKind_FReg][SizeClass_4]
                     [OperandKind_Memory][SizeClass_8] = FE_SSE_CVTSI2SS64rm;

    ENCODING_ENTRIES2[Mnem_SSE_CVTSI2S][OperandKind_FReg][SizeClass_8]
                     [OperandKind_Reg][SizeClass_4] = FE_SSE_CVTSI2SD32rr;
    ENCODING_ENTRIES2[Mnem_SSE_CVTSI2S][OperandKind_FReg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_4] = FE_SSE_CVTSI2SD32rm;

    ENCODING_ENTRIES2[Mnem_SSE_CVTSI2S][OperandKind_FReg][SizeClass_8]
                     [OperandKind_Reg][SizeClass_8] = FE_SSE_CVTSI2SD64rr;
    ENCODING_ENTRIES2[Mnem_SSE_CVTSI2S][OperandKind_FReg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_8] = FE_SSE_CVTSI2SD64rm;

    // SSE CVTS2SI

    ENCODING_ENTRIES2[Mnem_SSE_CVTS2SI][OperandKind_Reg][SizeClass_4]
                     [OperandKind_FReg][SizeClass_4] = FE_SSE_CVTSS2SI32rr;
    ENCODING_ENTRIES2[Mnem_SSE_CVTS2SI][OperandKind_Reg][SizeClass_4]
                     [OperandKind_Memory][SizeClass_4] = FE_SSE_CVTSS2SI32rm;

    ENCODING_ENTRIES2[Mnem_SSE_CVTS2SI][OperandKind_Reg][SizeClass_8]
                     [OperandKind_FReg][SizeClass_4] = FE_SSE_CVTSS2SI64rr;
    ENCODING_ENTRIES2[Mnem_SSE_CVTS2SI][OperandKind_Reg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_4] = FE_SSE_CVTSS2SI64rm;

    ENCODING_ENTRIES2[Mnem_SSE_CVTS2SI][OperandKind_Reg][SizeClass_4]
                     [OperandKind_FReg][SizeClass_8] = FE_SSE_CVTSD2SI32rr;
    ENCODING_ENTRIES2[Mnem_SSE_CVTS2SI][OperandKind_Reg][SizeClass_4]
                     [OperandKind_Memory][SizeClass_8] = FE_SSE_CVTSD2SI32rm;

    ENCODING_ENTRIES2[Mnem_SSE_CVTS2SI][OperandKind_Reg][SizeClass_8]
                     [OperandKind_FReg][SizeClass_8] = FE_SSE_CVTSD2SI64rr;
    ENCODING_ENTRIES2[Mnem_SSE_CVTS2SI][OperandKind_Reg][SizeClass_8]
                     [OperandKind_Memory][SizeClass_8] = FE_SSE_CVTSD2SI64rm;

    return &asm_builder->vt;
}
