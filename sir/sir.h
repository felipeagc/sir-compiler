#pragma once

#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct SIRModule SIRModule;
typedef struct SIRBuilder SIRBuilder;
typedef struct SIRType SIRType;
typedef struct SIRAsmBuilder SIRAsmBuilder;
typedef struct SIRObjectBuilder SIRObjectBuilder;

typedef enum SIRCallingConvention {
    SIRCallingConvention_SystemV,
} SIRCallingConvention;

typedef enum SIRLinkage {
    SIRLinkage_Internal,
    SIRLinkage_External,
    SIRLinkage_Interpeter,
} SIRLinkage;

typedef enum SIRTargetArch {
    SIRTargetArch_X86_64,
} SIRTargetArch;

typedef enum SIREndianness {
    SIREndianness_LittleEndian,
    SIREndianness_BigEndian,
} SIREndianness;

typedef enum SIRGlobalFlags {
    SIRGlobalFlags_ReadOnly = 1 << 0,
    SIRGlobalFlags_Initialized = 1 << 1,
} SIRGlobalFlags;

typedef enum SIRBinaryOperation {
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

    SIRBinaryOperation_BEQ,
    SIRBinaryOperation_BNE,
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
} SIRBinaryOperation;

typedef enum SIRInstKind {
    SIRInstKind_Unknown = 0,
    SIRInstKind_ConstInt,
    SIRInstKind_ConstFloat,
    SIRInstKind_ConstBool,
    SIRInstKind_Alias,
    SIRInstKind_Global,
    SIRInstKind_StackSlot,
    SIRInstKind_Block,
    SIRInstKind_Function,
    SIRInstKind_FunctionParameter,
    SIRInstKind_ReturnVoid,
    SIRInstKind_ReturnValue,
    SIRInstKind_Load,
    SIRInstKind_Store,
    SIRInstKind_Jump,
    SIRInstKind_Branch,
    SIRInstKind_Phi,
    SIRInstKind_FuncCall,
    SIRInstKind_BitCast,
    SIRInstKind_ZExt,
    SIRInstKind_SExt,
    SIRInstKind_Trunc,
    SIRInstKind_ArrayElemPtr,
    SIRInstKind_StructElemPtr,
    SIRInstKind_ExtractArrayElem,
    SIRInstKind_ExtractStructElem,
    SIRInstKind_Binop,
} SIRInstKind;

typedef enum SIRTypeKind {
    SIRTypeKind_Void,
    SIRTypeKind_Int,
    SIRTypeKind_Float,
    SIRTypeKind_Bool,
    SIRTypeKind_Pointer,
    SIRTypeKind_Array,
    SIRTypeKind_Struct,
} SIRTypeKind;

typedef struct SIRInstRef {
    uint32_t id;
} SIRInstRef;

/*
 *  SIRModule functions
 */

SIRModule *SIRModuleCreate(SIRTargetArch target_arch, SIREndianness endianness);
void SIRModuleDestroy(SIRModule *module);

SIRType *SIRModuleGetVoidType(SIRModule *module);
SIRType *SIRModuleGetBoolType(SIRModule *module);
SIRType *SIRModuleGetI8Type(SIRModule *module);
SIRType *SIRModuleGetI16Type(SIRModule *module);
SIRType *SIRModuleGetI32Type(SIRModule *module);
SIRType *SIRModuleGetI64Type(SIRModule *module);
SIRType *SIRModuleGetU8Type(SIRModule *module);
SIRType *SIRModuleGetU16Type(SIRModule *module);
SIRType *SIRModuleGetU32Type(SIRModule *module);
SIRType *SIRModuleGetU64Type(SIRModule *module);
SIRType *SIRModuleGetF32Type(SIRModule *module);
SIRType *SIRModuleGetF64Type(SIRModule *module);

SIRType *SIRModuleCreatePointerType(SIRModule *module, SIRType *sub);
SIRType *
SIRModuleCreateArrayType(SIRModule *module, SIRType *sub, uint64_t count);
SIRType *SIRModuleCreateStructType(
    SIRModule *module, SIRType **fields, size_t field_count, bool packed);

SIRInstRef
SIRModuleAddConstInt(SIRModule *module, SIRType *type, uint64_t value);
SIRInstRef
SIRModuleAddConstFloat(SIRModule *module, SIRType *type, double value);
SIRInstRef SIRModuleAddConstBool(SIRModule *module, bool value);

SIRInstRef SIRModuleAddFunction(
    SIRModule *module,
    const char *name,
    size_t name_len,
    SIRCallingConvention calling_convention,
    SIRLinkage linkage,
    bool variadic,
    SIRType **param_types,
    size_t param_types_len,
    SIRType *return_type);
SIRInstRef SIRModuleAddGlobal(
    SIRModule *module,
    SIRType *type,
    uint32_t flags,
    const uint8_t *data,
    size_t data_len);
SIRInstRef
SIRModuleAddGlobalString(SIRModule *module, const char *str, size_t len);
SIRInstRef
SIRModuleAddStackSlot(SIRModule *module, SIRInstRef func_ref, SIRType *type);
SIRInstRef SIRModuleGetFuncParam(
    SIRModule *module, SIRInstRef func_ref, uint32_t param_index);
SIRInstRef SIRModuleInsertBlockAtEnd(SIRModule *module, SIRInstRef func_ref);

SIRInstKind SIRModuleGetInstKind(SIRModule *module, SIRInstRef inst_ref);
SIRType *SIRModuleGetInstType(SIRModule *module, SIRInstRef inst_ref);
SIRTypeKind SIRModuleGetTypeKind(SIRModule *module, SIRType *type);
uint32_t SIRTypeSizeOf(SIRModule *module, SIRType *type);
uint32_t SIRTypeAlignOf(SIRModule *module, SIRType *type);
uint32_t
SIRTypeStructOffsetOf(SIRModule *module, SIRType *type, uint32_t field_index);
uint32_t
SIRModuleGetBlockInstructionCount(SIRModule *module, SIRInstRef block_ref);
SIRInstRef SIRModuleGetBlockInstruction(
    SIRModule *module, SIRInstRef block_ref, uint32_t inst_index);

char *SIRModulePrintToString(SIRModule *module, size_t *str_len);

/*
 *  SIRBuilder functions
 */
SIRBuilder *SIRBuilderCreate(SIRModule *module);
void SIRBuilderSetFunction(SIRBuilder *builder, SIRInstRef func_ref);
void SIRBuilderPositionAtEnd(SIRBuilder *builder, SIRInstRef block_ref);
SIRInstRef SIRBuilderGetCurrentFunction(SIRBuilder *builder);
SIRInstRef SIRBuilderGetCurrentBlock(SIRBuilder *builder);

SIRInstRef SIRBuilderInsertArrayElemPtr(
    SIRBuilder *builder, SIRInstRef accessed_ref, SIRInstRef index_ref);
SIRInstRef SIRBuilderInsertStructElemPtr(
    SIRBuilder *builder, SIRInstRef accessed_ref, uint32_t field_index);

SIRInstRef SIRBuilderInsertExtractArrayElem(
    SIRBuilder *builder, SIRInstRef accessed_ref, uint32_t elem_index);
SIRInstRef SIRBuilderInsertExtractStructElem(
    SIRBuilder *builder, SIRInstRef accessed_ref, uint32_t field_index);

void SIRBuilderInsertStore(
    SIRBuilder *builder, SIRInstRef ptr_ref, SIRInstRef value_ref);
SIRInstRef SIRBuilderInsertLoad(SIRBuilder *builder, SIRInstRef ptr_ref);

SIRInstRef SIRBuilderInsertBitCast(
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

SIRInstRef SIRBuilderInsertPhi(SIRBuilder *builder, SIRType *type);

void SIRPhiAddIncoming(
    SIRBuilder *builder,
    SIRInstRef phi_ref,
    SIRInstRef block_ref,
    SIRInstRef value_ref);

void SIRBuilderInsertReturnValue(SIRBuilder *builder, SIRInstRef inst_ref);
void SIRBuilderInsertReturnVoid(SIRBuilder *builder);

/*
 * Code generation functions
 */

SIRObjectBuilder *SIRCreateELF64Builder(SIRModule *module);
void SIRObjectBuilderDestroy(SIRObjectBuilder *obj_builder);
void SIRObjectBuilderOutputToFile(
    SIRObjectBuilder *obj_builder, const char *path, size_t path_len);

SIRAsmBuilder *
SIRCreateX64Builder(SIRModule *module, SIRObjectBuilder *obj_builder);
void SIRAsmBuilderGenerate(SIRAsmBuilder *asm_builder);
void SIRAsmBuilderDestroy(SIRAsmBuilder *asm_builder);

#ifdef __cplusplus
}
#endif
