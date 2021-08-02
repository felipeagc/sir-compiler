#pragma once

#include "sir_base.hpp"
#include "sir_type.hpp"

struct SIRModule;
struct SIRInst;

enum SIRCallingConvention : uint8_t {
    SIRCallingConvention_SystemV,
};

SIRString SIRCallingConventionToString(SIRCallingConvention calling_convention);

enum SIRLinkage : uint8_t {
    SIRLinkage_Internal,
    SIRLinkage_External,
};

SIRString SIRLinkageToString(SIRLinkage linkage);

enum SIRTargetArch {
    SIRTargetArch_X86_64,
};

enum SIREndianness {
    SIREndianness_LittleEndian,
    SIREndianness_BigEndian,
};

struct SIRInstRef {
    uint32_t id;
};

struct SIRFunction {
    SIRString name;
    SIRType ** param_types;
    size_t param_types_len;
    SIRType *return_type;
    bool variadic;

    SIRArray<SIRInstRef> stack_slots;
    SIRArray<SIRInstRef> blocks;
    SIRArray<SIRInstRef> param_insts;

    SIRLinkage linkage;
    SIRCallingConvention calling_convention;
};

enum SIRGlobalFlags {
    SIRGlobalFlags_ReadOnly = 1 << 0,
    SIRGlobalFlags_Initialized = 1 << 1,
};

enum SIRBinaryOperation {
    SIRBinaryOperation_Unknown = 0,

    SIRBinaryOperation_IAdd,
    SIRBinaryOperation_ISub,
    SIRBinaryOperation_IMul,
    SIRBinaryOperation_SDiv,
    SIRBinaryOperation_UDiv,
    SIRBinaryOperation_SRem,
    SIRBinaryOperation_URem,

    SIRBinaryOperation_FAdd,
    SIRBinaryOperation_FSub,
    SIRBinaryOperation_FMul,
    SIRBinaryOperation_FDiv,
    SIRBinaryOperation_FRem,

    SIRBinaryOperation_IEQ,
    SIRBinaryOperation_INE,
    SIRBinaryOperation_UGT,
    SIRBinaryOperation_UGE,
    SIRBinaryOperation_ULT,
    SIRBinaryOperation_ULE,
    SIRBinaryOperation_SGT,
    SIRBinaryOperation_SGE,
    SIRBinaryOperation_SLT,
    SIRBinaryOperation_SLE,

    SIRBinaryOperation_FEQ,
    SIRBinaryOperation_FNE,
    SIRBinaryOperation_FGT,
    SIRBinaryOperation_FGE,
    SIRBinaryOperation_FLT,
    SIRBinaryOperation_FLE,

    SIRBinaryOperation_Shl,
    SIRBinaryOperation_AShr,
    SIRBinaryOperation_LShr,

    SIRBinaryOperation_And,
    SIRBinaryOperation_Or,
    SIRBinaryOperation_Xor,

    SIRBinaryOperation_MAX,
};

enum SIRInstKind : uint8_t {
    SIRInstKind_Unknown = 0,
    SIRInstKind_Global,
    SIRInstKind_StackSlot,
    SIRInstKind_Block,
    SIRInstKind_ImmediateInt,
    SIRInstKind_ImmediateFloat,
    SIRInstKind_ImmediateBool,
    SIRInstKind_Function,
    SIRInstKind_FunctionParameter,
    SIRInstKind_ReturnVoid,
    SIRInstKind_ReturnValue,
    SIRInstKind_Load,
    SIRInstKind_Store,
    SIRInstKind_Jump,
    SIRInstKind_Branch,
    SIRInstKind_FuncCall,
    SIRInstKind_PtrCast,
    SIRInstKind_ZExt,
    SIRInstKind_SExt,
    SIRInstKind_Trunc,
    SIRInstKind_ArrayElemPtr,
    SIRInstKind_StructElemPtr,
    SIRInstKind_ExtractArrayElem,
    SIRInstKind_ExtractStructElem,
    SIRInstKind_Binop,
};

struct SIRInst {
    union {
        SIRFunction *func;
        struct {
            SIRArray<SIRInstRef> inst_refs;
        } block;
        struct {
            uint32_t index;
        } func_param;
        struct {
            uint64_t u64;
        } imm_int;
        struct {
            double f64;
        } imm_float;
        struct {
            bool value;
        } imm_bool;
        struct {
            const uint8_t *data;
            size_t data_len;
            uint32_t flags;
        } global;
        struct {
            SIRInstRef inst_ref;
        } return_value;
        struct {
            SIRInstRef block_ref;
        } jump;
        struct {
            SIRInstRef cond_inst_ref;
            SIRInstRef true_block_ref;
            SIRInstRef false_block_ref;
        } branch;
        struct {
            SIRInstRef func_ref;
            SIRInstRef *params;
            size_t params_len;
        } func_call;
        struct {
            SIRInstRef inst_ref;
        } ptr_cast;
        struct {
            SIRInstRef accessed_ref;
            SIRInstRef index_ref;
        } array_elem_ptr;
        struct {
            SIRInstRef accessed_ref;
            uint32_t field_index;
        } struct_elem_ptr;
        struct {
            SIRInstRef accessed_ref;
            SIRInstRef index_ref;
        } extract_array_elem;
        struct {
            SIRInstRef accessed_ref;
            uint32_t field_index;
        } extract_struct_elem;
        struct {
            SIRInstRef ptr_ref;
            SIRInstRef value_ref;
        } store;
        struct {
            SIRInstRef ptr_ref;
        } load;
        struct {
            SIRBinaryOperation op;
            SIRInstRef left_ref;
            SIRInstRef right_ref;
        } binop;
        struct {
            SIRInstRef stack_slot_ref;
        } stack_ptr;
        struct {
            SIRInstRef global_ref;
        } global_ptr;
        struct {
            SIRInstRef inst_ref;
        } zext;
        struct {
            SIRInstRef inst_ref;
        } sext;
        struct {
            SIRInstRef inst_ref;
        } trunc;
    };
    SIRInstKind kind;
    SIRType *type;
};

struct SIRModule {
    SIRArenaAllocator *arena;
    SIRArray<SIRInst> insts;
    SIRArray<SIRInstRef> globals;
    SIRArray<SIRInstRef> functions;
    SIRStringMap function_map;
    SIRStringMap global_string_map;
    SIRStringMap type_map;
    SIRTargetArch target_arch;
    SIREndianness endianness;

    SIRType *void_type;
    SIRType *bool_type;
    SIRType *i8_type;
    SIRType *i16_type;
    SIRType *i32_type;
    SIRType *i64_type;
    SIRType *f32_type;
    SIRType *f64_type;
};

SIRModule *SIRModuleCreate(SIRTargetArch target_arch, SIREndianness endianness);
void SIRModuleDestroy(SIRModule *module);

SIR_INLINE
SIRInst SIRModuleGetInst(SIRModule *module, SIRInstRef inst_ref)
{
    return module->insts[inst_ref.id];
}

SIRType *SIRModuleCreatePointerType(SIRModule *module, SIRType *sub);
SIRType *
SIRModuleCreateArrayType(SIRModule *module, SIRType *sub, uint64_t count);
SIRType *SIRModuleCreateStructType(
    SIRModule *module, SIRType ** fields, size_t field_count, bool packed);

SIRType *SIRModuleGetCachedType(SIRModule *module, SIRType *type);

SIRInstRef SIRModuleAddFunction(
    SIRModule *module,
    SIRString name,
    SIRCallingConvention calling_convention,
    SIRLinkage linkage,
    bool variadic,
    SIRType ** param_types,
    size_t param_types_len,
    SIRType *return_type);
SIRInstRef SIRModuleAddGlobal(
    SIRModule *module, SIRType *type, uint32_t flags, const uint8_t *data, size_t data_len);
SIRInstRef SIRModuleAddGlobalString(SIRModule *module, const SIRString &str);
SIRInstRef
SIRModuleAddStackSlot(SIRModule *module, SIRInstRef func_ref, SIRType *type);
SIRInstRef SIRModuleGetFuncParam(
    SIRModule *module, SIRInstRef func_ref, uint32_t param_index);
SIRInstRef SIRModuleInsertBlockAtEnd(SIRModule *module, SIRInstRef func_ref);
SIRString SIRModulePrintAlloc(SIRModule *module, SIRAllocator *allocator);

struct SIRBuilder {
    SIRModule *module;
    SIRInstRef current_func_ref;
    SIRInstRef current_block_ref;
};

SIRBuilder *SIRBuilderCreate(SIRModule *module);
void SIRBuilderSetFunction(SIRBuilder *builder, SIRInstRef func_ref);
void SIRBuilderPositionAtEnd(SIRBuilder *builder, SIRInstRef block_ref);

SIRInstRef
SIRBuilderInsertImmInt(SIRBuilder *builder, SIRType *type, uint64_t value);
SIRInstRef
SIRBuilderInsertImmFloat(SIRBuilder *builder, SIRType *type, double value);
SIRInstRef SIRBuilderInsertImmBool(SIRBuilder *builder, bool value);

SIRInstRef SIRBuilderInsertArrayElemPtr(
    SIRBuilder *builder, SIRInstRef accessed_ref, SIRInstRef index_ref);
SIRInstRef SIRBuilderInsertStructElemPtr(
    SIRBuilder *builder, SIRInstRef accessed_ref, uint32_t field_index);

SIRInstRef SIRBuilderInsertExtractArrayElem(
    SIRBuilder *builder, SIRInstRef accessed_ref, SIRInstRef index_ref);
SIRInstRef SIRBuilderInsertExtractStructElem(
    SIRBuilder *builder, SIRInstRef accessed_ref, uint32_t field_index);

void SIRBuilderInsertStore(
    SIRBuilder *builder, SIRInstRef ptr_ref, SIRInstRef value_ref);
SIRInstRef SIRBuilderInsertLoad(SIRBuilder *builder, SIRInstRef ptr_ref);

SIRInstRef SIRBuilderInsertPtrCast(
    SIRBuilder *builder, SIRType *dest_type, SIRInstRef inst_ref);
SIRInstRef SIRBuilderInsertZext(
    SIRBuilder *builder, SIRType *dest_type, SIRInstRef inst_ref);
SIRInstRef SIRBuilderInsertSext(
    SIRBuilder *builder, SIRType *dest_type, SIRInstRef inst_ref);
SIRInstRef SIRBuilderInsertTrunc(
    SIRBuilder *builder, SIRType *dest_type, SIRInstRef inst_ref);

SIRInstRef SIRBuilderInsertBinop(
    SIRBuilder *builder,
    SIRBinaryOperation op,
    SIRInstRef left_ref,
    SIRInstRef right_ref);

SIRInstRef SIRBuilderInsertFuncCall(
    SIRBuilder *builder,
    SIRInstRef func_ref,
    const SIRInstRef *parameters,
    size_t param_count);
void SIRBuilderInsertJump(SIRBuilder *builder, SIRInstRef block_ref);

void SIRBuilderInsertBranch(
    SIRBuilder *builder,
    SIRInstRef cond_ref,
    SIRInstRef true_block_ref,
    SIRInstRef false_block_ref);

void SIRBuilderInsertReturnValue(SIRBuilder *builder, SIRInstRef inst_ref);
void SIRBuilderInsertReturnVoid(SIRBuilder *builder);
