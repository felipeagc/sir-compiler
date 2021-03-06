#pragma once

#include "sir_base.hpp"
#include "sir.h"

struct SIRModule;
struct SIRInst;

SIRString SIRCallingConventionToString(SIRCallingConvention calling_convention);
SIRString SIRLinkageToString(SIRLinkage linkage);

struct SIRType {
    SIRTypeKind kind;
    uint32_t size = 0;
    uint32_t alignment = 0;
    SIRString str = {};
    union {
        struct {
            uint32_t bits;
            bool is_signed;
        } int_;
        struct {
            uint32_t bits;
        } float_;
        struct {
            SIRType *sub;
        } pointer;
        struct {
            SIRType *sub;
            uint64_t count;
        } array;
        struct {
            SIRType **fields;
            uint32_t fields_len;
            bool packed;
        } struct_;
    };
};

SIRString SIRTypeToString(SIRModule *module, SIRType *type);

struct SIRFunction {
    SIRString name;
    SIRType **param_types;
    size_t param_types_len;
    SIRType *return_type;
    bool variadic;

    SIRArray<SIRInstRef> stack_slots;
    SIRArray<SIRInstRef> blocks;
    SIRArray<SIRInstRef> param_insts;

    SIRLinkage linkage;
    SIRCallingConvention calling_convention;
};

struct SIRBlock {
    SIRArray<SIRInstRef> inst_refs;
};

struct SIRGlobal {
    const uint8_t *data;
    size_t data_len;
    uint32_t flags;
};

struct SIRInst {
    SIRInstKind kind;
    SIRBinaryOperation binop;
    SIRType *type;
    union {
        SIRFunction *func;
        SIRBlock *block;
        SIRGlobal *global;
        struct {
            uint64_t u64;
        } const_int;
        struct {
            double f64;
        } const_float;
        struct {
            bool value;
        } const_bool;
        struct {
            uint32_t index;
        } func_param;
        struct {
            SIRInstRef op1;
            SIRInstRef op2;
        };
        struct {
            SIRInstRef ptr_ref;
        } load;
        struct {
            SIRInstRef ptr_ref;
            SIRInstRef value_ref;
        } store;
        struct {
            SIRInstRef block_ref;
            SIRInstRef value_ref;
        } phi_incoming;
        struct {
            SIRInstRef accessed_ref;
            SIRInstRef index_ref;
        } array_elem_ptr;
        struct {
            SIRInstRef accessed_ref;
            SIRInstRef field_index_ref;
        } struct_elem_ptr;
        struct {
            SIRInstRef accessed_ref;
            SIRInstRef index_ref;
        } extract_array_elem;
        struct {
            SIRInstRef accessed_ref;
            SIRInstRef field_index_ref;
        } extract_struct_elem;
    };
};

struct SIRModule {
    SIRArenaAllocator *arena;
    SIRArray<SIRInst> insts;
    SIRArray<SIRInstRef> globals;
    SIRArray<SIRInstRef> consts;
    SIRArray<SIRInstRef> functions;
    SIRStringMap function_map;
    SIRStringMap global_string_map;
    SIRStringMap type_map;
    SIRStringMap named_struct_map;
    SIRTargetArch target_arch;
    SIREndianness endianness;

    SIRType *void_type;
    SIRType *bool_type;
    SIRType *i8_type;
    SIRType *i16_type;
    SIRType *i32_type;
    SIRType *i64_type;
    SIRType *u8_type;
    SIRType *u16_type;
    SIRType *u32_type;
    SIRType *u64_type;
    SIRType *f32_type;
    SIRType *f64_type;
};

struct SIRBuilder {
    SIRModule *module;
    SIRInstRef current_func_ref;
    SIRInstRef current_block_ref;
};

SIR_INLINE
SIRInst SIRModuleGetInst(SIRModule *module, SIRInstRef inst_ref)
{
    return module->insts[inst_ref.id];
}

SIRType *SIRModuleGetCachedType(SIRModule *module, SIRType *type);

typedef void(SIRAuxInstPrinter)(
    void *user_data, SIRInstRef inst_ref, size_t inst_pos, SIRStringBuilder *sb);

char *SIRModulePrintToStringWithAux(
    SIRModule *module,
    size_t *str_len,
    void *user_data,
    SIRAuxInstPrinter *aux_info_printer);
