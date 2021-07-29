#pragma once

#include "sir_base.hpp"

struct SIRModule;

enum SIRTypeKind {
    SIRTypeKind_Void,
    SIRTypeKind_Int,
    SIRTypeKind_Float,
    SIRTypeKind_Bool,
    SIRTypeKind_Pointer,
    SIRTypeKind_Array,
    SIRTypeKind_Struct,
};

struct SIRType {
    union {
        struct {
            uint32_t bits;
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
            SIRSlice<SIRType *> fields;
            bool packed;
        } struct_;
    };
    SIRTypeKind kind;

    uint32_t size = 0;
    uint32_t alignment = 0;
    SIRString str = {};

    SIRString to_string(SIRModule *module);
    uint32_t align_of(SIRModule *module);
    uint32_t size_of(SIRModule *module);
};
