#pragma once

#include "ace_base.hpp"
#include "ace_type.hpp"

namespace ace {

struct Module;
struct StackSlot;
struct Inst;
struct Block;
struct Function;

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

struct FunctionRef {
    uint32_t id;
};

struct BlockRef {
    uint32_t id;
};

struct InstRef {
    uint32_t id;
};

struct StackSlotRef {
    uint32_t id;
};

struct GlobalRef {
    uint32_t id;
};

struct ConstRef {
    uint32_t id;
};

enum InstKind {
    InstKind_FunctionParameter,
    InstKind_GetConst,
    InstKind_ReturnVoid,
    InstKind_ReturnValue,
    InstKind_StackLoad,
    InstKind_StackStore,
    InstKind_StackAddr,
    InstKind_Jump,
    InstKind_FuncCall,
    InstKind_GlobalAddr,
    InstKind_GlobalLoad,
    InstKind_GlobalStore,
};

struct Inst {
    union {
        struct {
            uint32_t index;
        } func_param;
        struct {
            ConstRef const_ref;
        } get_const;
        struct {
            InstRef inst_ref;
        } return_value;
        struct {
            StackSlotRef ss_ref;
        } stack_load;
        struct {
            StackSlotRef ss_ref;
            InstRef inst_ref;
        } stack_store;
        struct {
            BlockRef block_ref;
        } jump;
        struct {
            FunctionRef func_ref;
            Slice<InstRef> parameters;
        } func_call;
        struct {
            GlobalRef global_ref;
        } global_addr;
        struct {
            StackSlotRef ss_ref;
        } stack_addr;
        struct {
            GlobalRef global_ref;
        } global_load;
        struct {
            GlobalRef global_ref;
            InstRef inst_ref;
        } global_store;
    };
    InstKind kind;
    Type *type;
};

struct StackSlot {
    Type *type;
};

struct Block {
    Array<InstRef> inst_refs;
};

struct Function {
    String name;
    Slice<Type *> param_types;
    Type *return_type;
    bool variadic;

    Array<StackSlot> stack_slots;
    Array<Block> blocks;
    Array<Inst> insts;
    Array<InstRef> param_insts;

    Array<List<BlockRef>::Node *> block_nodes;
    List<BlockRef> block_refs;

    Linkage linkage;
    CallingConvention calling_convention;
};

enum GlobalFlags {
    GlobalFlags_ReadOnly = 1 << 0,
    GlobalFlags_Initialized = 1 << 1,
};

struct Global {
    Type *type;
    Slice<uint8_t> data;
    uint32_t flags;
};

struct Const {
    Type *type;
    union {
        double f64;
        uint64_t u64;
        Slice<uint8_t> data;
    };

    String to_string(Module *module);
};

struct Module {
    ArenaAllocator *arena;
    Array<Function> functions;
    Array<Global> globals;
    Array<Const> consts;
    StringMap<FunctionRef> function_map;
    StringMap<Type *> type_map;
    StringMap<ConstRef> const_map;
    StringMap<GlobalRef> global_string_map;
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

    Type *create_pointer_type();
    Type *create_array_type(Type *sub, uint64_t count);
    Type *create_struct_type(Slice<Type *> fields, bool packed);

    Type *get_cached_type(Type *type);

    ConstRef create_int_const(Type *type, uint64_t value);
    ConstRef create_float_const(Type *type, double value);

    ConstRef register_const(Const constant);

    FunctionRef add_function(
        String name,
        CallingConvention calling_convention,
        Linkage linkage,
        bool variadic,
        Slice<Type *> param_types,
        Type *return_type);

    GlobalRef add_global(Type *type, uint32_t flags, Slice<uint8_t> data);
    GlobalRef add_global_string(const String &str);

    StackSlotRef add_stack_slot(FunctionRef func_ref, Type *type);

    InstRef get_func_param(FunctionRef func_ref, uint32_t param_index);

    BlockRef insert_block_at_end(FunctionRef func_ref);
    BlockRef insert_block_after(FunctionRef func_ref, BlockRef block_ref);
    BlockRef insert_block_before(FunctionRef func_ref, BlockRef block_ref);

    String print_alloc(Allocator *allocator);
};

struct Builder {
    Module *module;
    FunctionRef current_func_ref;
    BlockRef current_block_ref;

    static Builder create(Module *module);

    void set_function(FunctionRef func_ref);
    void position_at_end(BlockRef block_ref);

    InstRef insert_get_const(ConstRef const_ref);

    InstRef insert_global_addr(GlobalRef global_ref);
    InstRef insert_global_load(GlobalRef global_ref, Type *type);
    void insert_global_store(GlobalRef global_ref, InstRef inst_ref);

    InstRef insert_stack_addr(StackSlotRef ss_ref);
    InstRef insert_stack_load(StackSlotRef ss_ref);
    void insert_stack_store(StackSlotRef ss_ref, InstRef inst_ref);

    InstRef
    insert_func_call(FunctionRef func_ref, const Slice<InstRef> &parameters);
    void insert_jump(BlockRef block_ref);
    void insert_return_value(InstRef inst_ref);
    void insert_return_void();
};

}; // namespace ace
