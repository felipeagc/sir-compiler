#pragma once

#include "sir_base.hpp"
#include "sir.h"

struct SIRModule;
struct SIRInst;

SIRString SIRCallingConventionToString(SIRCallingConvention calling_convention);
SIRString SIRLinkageToString(SIRLinkage linkage);

struct SIRType {
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
            SIRType ** fields;
            uint32_t fields_len;
            bool packed;
        } struct_;
    };
    SIRTypeKind kind;

    uint32_t size = 0;
    uint32_t alignment = 0;
    SIRString str = {};
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

struct SIRPhiPair {
    SIRInstRef block_ref;
    SIRInstRef value_ref;
};

struct SIRInst {
    union {
        SIRFunction *func;
        struct {
            SIRInstRef inst_ref;
        } alias;
        struct {
            SIRArray<SIRInstRef> inst_refs;
        } block;
        struct {
            uint32_t index;
        } func_param;
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
            SIRArray<SIRPhiPair> pairs;
        } phi;
        struct {
            SIRInstRef func_ref;
            SIRInstRef *params;
            size_t params_len;
        } func_call;
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
            uint32_t elem_index;
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
        } cast;
        struct {
            SIRInstRef inst_ref;
        } fneg;
    };
    SIRInstKind kind;
    SIRType *type;
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
