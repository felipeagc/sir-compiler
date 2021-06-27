#pragma once

#include "ace_base.hpp"

namespace ace {

struct Module;

enum TypeKind {
    TypeKind_Void,
    TypeKind_Int,
    TypeKind_Float,
    TypeKind_Bool,
    TypeKind_Pointer,
    TypeKind_Array,
    TypeKind_Struct,
};

struct Type {
    union {
        struct {
            uint32_t bits;
        } int_;
        struct {
            uint32_t bits;
        } float_;
        struct {
            Type *sub;
            uint64_t count;
        } array;
        struct {
            Slice<Type *> fields;
            bool packed;
        } struct_;
    };
    TypeKind kind;

  private:
    uint32_t size = 0;
    uint32_t alignment = 0;
    String str = {};

  public:
    String to_string(Module *module);
    uint32_t align_of(Module *module);
    uint32_t size_of(Module *module);
};

} // namespace ace
