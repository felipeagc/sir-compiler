#pragma once

#include "ace_base.hpp"
#include "ace_type.hpp"

namespace ace {

struct Module;
struct Inst;

enum CallingConvention : uint8_t {
    CallingConvention_SystemV,
};

String calling_convention_to_string(CallingConvention calling_convention);

enum Linkage : uint8_t {
    Linkage_Internal,
    Linkage_External,
};

String linkage_to_string(Linkage linkage);

enum TargetArch {
    TargetArch_X86_64,
};

enum Endianness {
    Endianness_LittleEndian,
    Endianness_BigEndian,
};

struct InstRef {
    uint32_t id;

    ACE_INLINE
    Inst get(Module *module) const;
};

struct Function {
    String name;
    Slice<Type *> param_types;
    Type *return_type;
    bool variadic;

    Array<InstRef> stack_slots;
    Array<InstRef> blocks;
    Array<InstRef> param_insts;

    Linkage linkage;
    CallingConvention calling_convention;
};

enum GlobalFlags {
    GlobalFlags_ReadOnly = 1 << 0,
    GlobalFlags_Initialized = 1 << 1,
};

enum InstKind {
    InstKind_Unknown = 0,
    InstKind_Global,
    InstKind_StackSlot,
    InstKind_Block,
    InstKind_ImmediateInt,
    InstKind_ImmediateFloat,
    InstKind_Function,
    InstKind_FunctionParameter,
    InstKind_ReturnVoid,
    InstKind_ReturnValue,
    InstKind_Load,
    InstKind_Store,
    InstKind_Jump,
    InstKind_FuncCall,
    InstKind_PtrCast,
    InstKind_ArrayElemPtr,
};

struct Inst {
    union {
        Function *func;
        struct {
            Array<InstRef> inst_refs;
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
            Slice<uint8_t> data;
            uint32_t flags;
        } global;
        struct {
            InstRef inst_ref;
        } return_value;
        struct {
            InstRef block_ref;
        } jump;
        struct {
            InstRef func_ref;
            Slice<InstRef> parameters;
        } func_call;
        struct {
            InstRef inst_ref;
        } ptr_cast;
        struct {
            InstRef accessed_ref;
            InstRef index_ref;
        } array_elem_ptr;
        struct {
            InstRef ptr_ref;
            InstRef value_ref;
        } store;
        struct {
            InstRef ptr_ref;
        } load;
    };
    InstKind kind;
    Type *type;
};

struct Module {
    ArenaAllocator *arena;
    Array<Inst> insts;
    Array<InstRef> globals;
    Array<InstRef> functions;
    StringMap<InstRef> function_map;
    StringMap<InstRef> global_string_map;
    StringMap<Type *> type_map;
    TargetArch target_arch;
    Endianness endianness;

    Type *void_type;
    Type *bool_type;
    Type *i8_type;
    Type *i16_type;
    Type *i32_type;
    Type *i64_type;
    Type *f32_type;
    Type *f64_type;

  public:
    static Module *create(TargetArch target_arch, Endianness endianness);
    void destroy();

    Type *create_pointer_type(Type *sub);
    Type *create_array_type(Type *sub, uint64_t count);
    Type *create_struct_type(Slice<Type *> fields, bool packed);

    Type *get_cached_type(Type *type);

    InstRef add_inst(const Inst &inst);

    InstRef add_function(
        String name,
        CallingConvention calling_convention,
        Linkage linkage,
        bool variadic,
        Slice<Type *> param_types,
        Type *return_type);

    InstRef add_global(Type *type, uint32_t flags, Slice<uint8_t> data);
    InstRef add_global_string(const String &str);

    InstRef add_stack_slot(InstRef func_ref, Type *type);
    InstRef get_func_param(InstRef func_ref, uint32_t param_index);

    InstRef insert_block_at_end(InstRef func_ref);
    /* InstRef insert_block_after(InstRef func_ref, InstRef block_ref); */
    /* InstRef insert_block_before(InstRef func_ref, InstRef block_ref); */

    String print_alloc(Allocator *allocator);
};

struct Builder {
    Module *module;
    InstRef current_func_ref;
    InstRef current_block_ref;

    static Builder create(Module *module);

    void set_function(InstRef func_ref);
    void position_at_end(InstRef block_ref);

    InstRef insert_imm_int(Type *type, uint64_t value);
    InstRef insert_imm_float(Type *type, double value);

    InstRef insert_array_elem_ptr(InstRef accessed_ref, InstRef index_ref);

    void insert_store(InstRef ptr_ref, InstRef value_ref);
    InstRef insert_load(InstRef ptr_ref);

    InstRef insert_ptr_cast(Type *dest_type, InstRef inst_ref);

    InstRef
    insert_func_call(InstRef func_ref, const Slice<InstRef> &parameters);
    void insert_jump(InstRef block_ref);
    void insert_return_value(InstRef inst_ref);
    void insert_return_void();
};

inline Inst InstRef::get(Module *module) const
{
    return module->insts[this->id];
}

}; // namespace ace
