#include "ace_ir.hpp"
#include <Tracy.hpp>

using namespace ace;

String ace::calling_convention_to_string(CallingConvention calling_convention)
{
    switch (calling_convention) {
    case CallingConvention_SystemV: return "system_v";
    }

    ACE_ASSERT(0);
}

String ace::linkage_to_string(Linkage linkage)
{
    switch (linkage) {
    case Linkage_Internal: return "internal";
    case Linkage_External: return "external";
    }

    ACE_ASSERT(0);
}

String Const::to_string(Module *module)
{
    switch (this->type->kind) {
    case TypeKind_Int: {
        switch (this->type->float_.bits) {
        case 8: return module->arena->sprintf("@i8(%lu)", this->u64);
        case 16: return module->arena->sprintf("@i16(%lu)", this->u64);
        case 32: return module->arena->sprintf("@i32(%lu)", this->u64);
        case 64: return module->arena->sprintf("@i64(%lu)", this->u64);
        default: ACE_ASSERT(0); break;
        }
        break;
    }
    case TypeKind_Float: {
        switch (this->type->float_.bits) {
        case 32: return module->arena->sprintf("@f32(%lf)", this->f64);
        case 64: return module->arena->sprintf("@f64(%lf)", this->f64);
        default: ACE_ASSERT(0); break;
        }
        break;
    }
    default: break;
    }

    return "";
}

static void print_instruction(
    Module *module, Function *func, InstRef inst_ref, StringBuilder *sb)
{
    ZoneScoped;

    (void)module;
    Inst *inst = &func->insts[inst_ref.id];
    sb->append("    ");

    switch (inst->kind) {
    case InstKind_FunctionParameter: ACE_ASSERT(0); break;
    case InstKind_PtrCast: {
        String type_string = inst->type->to_string(module);
        sb->sprintf(
            "%%r%u = ptr_cast %.*s %%r%u",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst->ptr_cast.inst_ref.id);
        break;
    }
    case InstKind_GetConst: {
        String type_string = inst->type->to_string(module);
        sb->sprintf(
            "%%r%u = get_const %.*s %%const%u",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst->get_const.const_ref.id);
        break;
    }
    case InstKind_GlobalAddr: {
        sb->sprintf(
            "%%r%u = global_addr %%global%u",
            inst_ref.id,
            inst->global_addr.global_ref.id);
        break;
    }
    case InstKind_StackAddr: {
        sb->sprintf(
            "%%r%u = stack_addr %%ss%u",
            inst_ref.id,
            inst->stack_addr.ss_ref.id);
        break;
    }
    case InstKind_FuncCall: {
        Function *called_func = &module->functions[inst->func_call.func_ref.id];
        sb->sprintf(
            "%%r%u = func_call %.*s (",
            inst_ref.id,
            (int)called_func->name.len,
            called_func->name.ptr);
        for (size_t i = 0; i < inst->func_call.parameters.len; ++i) {
            if (i > 0) sb->append(", ");
            InstRef param_inst_ref = inst->func_call.parameters[i];
            sb->sprintf("%%r%u", param_inst_ref.id);
        }
        sb->append(")");
        break;
    }
    case InstKind_ReturnVoid: {
        sb->sprintf("return_void");
        break;
    }
    case InstKind_ReturnValue: {
        sb->sprintf("return_value %%r%u", inst->return_value.inst_ref.id);
        break;
    }
    case InstKind_StackLoad: {
        sb->sprintf(
            "%%r%u = stack_load %%ss%u",
            inst_ref.id,
            inst->stack_load.ss_ref.id);
        break;
    }
    case InstKind_StackStore: {
        sb->sprintf(
            "stack_store %%ss%u, %%r%u",
            inst->stack_store.ss_ref.id,
            inst->stack_store.inst_ref.id);
        break;
    }
    case InstKind_GlobalLoad: {
        sb->sprintf(
            "%%r%u = global_load %%global%u",
            inst_ref.id,
            inst->global_load.global_ref.id);
        break;
    }
    case InstKind_GlobalStore: {
        sb->sprintf(
            "global_store %%global%u, %%r%u",
            inst->global_store.global_ref.id,
            inst->global_store.inst_ref.id);
        break;
    }
    case InstKind_Jump: {
        sb->sprintf("jump %%b%u", inst->jump.block_ref.id);
        break;
    }
    }

    sb->append("\n");
}

static void print_block(
    Module *module, Function *func, BlockRef block_ref, StringBuilder *sb)
{
    ZoneScoped;

    Block *block = &func->blocks[block_ref.id];

    sb->sprintf("  block %%b%u:\n", block_ref.id);

    for (InstRef inst_ref : block->inst_refs) {
        print_instruction(module, func, inst_ref, sb);
    }

    sb->append("\n");
}

static void
print_global(Module *module, GlobalRef global_ref, StringBuilder *sb)
{
    Global *global = &module->globals[global_ref.id];
    sb->sprintf("global %%global%u: ", global_ref.id);

    sb->append(global->type->to_string(module));
    sb->append("\n");
}

static void print_function(Module *module, Function *func, StringBuilder *sb)
{
    ZoneScoped;

    String calling_conv_str =
        calling_convention_to_string(func->calling_convention);
    String linkage_str = linkage_to_string(func->linkage);
    sb->sprintf(
        "function [%.*s, %.*s] \"%.*s\" (",
        (int)calling_conv_str.len,
        calling_conv_str.ptr,
        (int)linkage_str.len,
        linkage_str.ptr,
        (int)func->name.len,
        func->name.ptr);
    for (size_t i = 0; i < func->param_insts.len; ++i) {
        Type *param_type = func->param_types[i];
        InstRef param_inst_ref = func->param_insts[i];

        if (i != 0) sb->append(", ");

        sb->sprintf("%%r%u: ", param_inst_ref.id);
        sb->append(param_type->to_string(module));
    }
    sb->append(") -> ");
    sb->append(func->return_type->to_string(module));
    sb->append(" {\n");

    for (uint32_t i = 0; i < func->stack_slots.len; ++i) {
        StackSlot *stack_slot = &func->stack_slots[i];
        String type_string = stack_slot->type->to_string(module);
        sb->sprintf(
            "  %%ss%u: %.*s\n", i, (int)type_string.len, type_string.ptr);
    }

    if (func->stack_slots.len > 0) {
        sb->append("\n");
    }

    for (auto &block_node : func->block_refs) {
        BlockRef block_ref = block_node.value;
        print_block(module, func, block_ref, sb);
    }

    sb->append("}\n\n");
}

Module *Module::create(TargetArch target_arch, Endianness endianness)
{
    auto parent_allocator = MallocAllocator::get_instance();

    const size_t expected_function_count = 65536;
    const size_t expected_type_count = 1024;
    const size_t expected_const_count = 2048;
    const size_t expected_global_count = 2048;
    const size_t expected_global_string_count = 2048;

    auto arena = ArenaAllocator::create(parent_allocator, 1 << 24);
    auto functions = Array<Function>::create(MallocAllocator::get_instance());
    auto globals = Array<Global>::create(MallocAllocator::get_instance());
    auto consts = Array<Const>::create(MallocAllocator::get_instance());
    auto function_map = StringMap<FunctionRef>::create(
        MallocAllocator::get_instance(), expected_function_count);
    auto type_map = StringMap<Type *>::create(
        MallocAllocator::get_instance(), expected_type_count);
    auto const_map = StringMap<ConstRef>::create(
        MallocAllocator::get_instance(), expected_const_count);
    auto global_string_map = StringMap<GlobalRef>::create(
        MallocAllocator::get_instance(), expected_global_string_count);

    functions.reserve(expected_function_count);
    globals.reserve(expected_global_count);
    consts.reserve(expected_const_count);

    Module *module = parent_allocator->alloc<Module>();
    *module = {
        .arena = arena,
        .functions = functions,
        .globals = globals,
        .consts = consts,
        .function_map = function_map,
        .type_map = type_map,
        .const_map = const_map,
        .global_string_map = global_string_map,
        .target_arch = target_arch,
        .endianness = endianness,

        .void_type = nullptr,
        .bool_type = nullptr,
        .i8_type = nullptr,
        .i16_type = nullptr,
        .i32_type = nullptr,
        .i64_type = nullptr,
        .f32_type = nullptr,
        .f64_type = nullptr,
    };

    {
        Type *void_type = module->arena->alloc<Type>();
        *void_type = {};
        void_type->kind = TypeKind_Void;
        module->void_type = module->get_cached_type(void_type);
    }

    {
        Type *bool_type = module->arena->alloc<Type>();
        *bool_type = {};
        bool_type->kind = TypeKind_Bool;
        module->bool_type = module->get_cached_type(bool_type);
    }

    {
        Type *i8_type = module->arena->alloc<Type>();
        *i8_type = {};
        i8_type->kind = TypeKind_Int;
        i8_type->int_.bits = 8;
        module->i8_type = module->get_cached_type(i8_type);
    }

    {
        Type *i16_type = module->arena->alloc<Type>();
        *i16_type = {};
        i16_type->kind = TypeKind_Int;
        i16_type->int_.bits = 16;
        module->i16_type = module->get_cached_type(i16_type);
    }

    {
        Type *i32_type = module->arena->alloc<Type>();
        *i32_type = {};
        i32_type->kind = TypeKind_Int;
        i32_type->int_.bits = 32;
        module->i32_type = module->get_cached_type(i32_type);
    }

    {
        Type *i64_type = module->arena->alloc<Type>();
        *i64_type = {};
        i64_type->kind = TypeKind_Int;
        i64_type->int_.bits = 64;
        module->i64_type = module->get_cached_type(i64_type);
    }

    {
        Type *f32_type = module->arena->alloc<Type>();
        *f32_type = {};
        f32_type->kind = TypeKind_Float;
        f32_type->float_.bits = 32;
        module->f32_type = module->get_cached_type(f32_type);
    }

    {
        Type *f64_type = module->arena->alloc<Type>();
        *f64_type = {};
        f64_type->kind = TypeKind_Float;
        f64_type->float_.bits = 64;
        module->f64_type = module->get_cached_type(f64_type);
    }

    return module;
}

void Module::destroy()
{
    this->functions.destroy();
    this->globals.destroy();
    this->consts.destroy();
    this->function_map.destroy();
    this->type_map.destroy();
    this->const_map.destroy();
    this->global_string_map.destroy();
    this->arena->destroy();

    auto parent_allocator = MallocAllocator::get_instance();
    parent_allocator->free(this);
}

String Module::print_alloc(Allocator *allocator)
{
    ZoneScoped;

    StringBuilder sb = StringBuilder::create(allocator);

    for (uint32_t i = 0; i < this->globals.len; ++i) {
        print_global(this, {i}, &sb);
    }

    sb.append("\n");

    for (auto &func : this->functions) {
        print_function(this, &func, &sb);
    }

    String result = sb.build(allocator);
    sb.destroy();
    return result;
}

Type *Module::create_pointer_type(Type *sub)
{
    Type *type = this->arena->alloc<Type>();
    *type = {};
    type->kind = TypeKind_Pointer;
    type->pointer.sub = sub;
    return this->get_cached_type(type);
}

Type *Module::create_array_type(Type *sub, uint64_t count)
{
    Type *type = this->arena->alloc<Type>();
    *type = {};
    type->kind = TypeKind_Array;
    type->array.sub = sub;
    type->array.count = count;
    return this->get_cached_type(type);
}

Type *Module::create_struct_type(Slice<Type *> fields, bool packed)
{
    Type *type = this->arena->alloc<Type>();
    *type = {};
    type->kind = TypeKind_Array;
    type->struct_.fields = this->arena->clone(fields);
    type->struct_.packed = packed;
    return this->get_cached_type(type);
}

Type *Module::get_cached_type(Type *type)
{
    ZoneScoped;

    String type_string = type->to_string(this);
    Type *existing_type = nullptr;
    if (this->type_map.get(type_string, &existing_type)) {
        return existing_type;
    }

    this->type_map.set(type_string, type);
    return type;
}

ConstRef Module::create_int_const(Type *type, uint64_t value)
{
    ACE_ASSERT(type->kind == TypeKind_Int);
    Const constant = {};
    constant.type = type;
    constant.u64 = value;

    return this->register_const(constant);
}

ConstRef Module::create_float_const(Type *type, double value)
{
    ACE_ASSERT(type->kind == TypeKind_Float);
    Const constant = {};
    constant.type = type;
    constant.f64 = value;

    return this->register_const(constant);
}

ConstRef Module::register_const(Const constant)
{
    ZoneScoped;

    String const_string = constant.to_string(this);
    if (const_string.len == 0) {
        ConstRef new_const_ref = {(uint32_t)this->consts.len};
        this->consts.push_back(constant);
        return new_const_ref;
    }

    ConstRef existing_const_ref = {};
    if (this->const_map.get(const_string, &existing_const_ref)) {
        return existing_const_ref;
    }

    ConstRef new_const_ref = {(uint32_t)this->consts.len};
    this->consts.push_back(constant);
    return new_const_ref;
}

FunctionRef Module::add_function(
    String name,
    CallingConvention calling_convention,
    Linkage linkage,
    bool variadic,
    Slice<Type *> param_types,
    Type *return_type)
{
    ZoneScoped;

    String func_name = this->arena->clone(name);
    uint32_t func_index = 0;
    while (this->function_map.get(func_name)) {
        func_index++;
        func_name = this->arena->sprintf(
            "%.*s.%u", (int)name.len, name.ptr, func_index);
    }

    Function function = {
        .name = func_name,
        .param_types = this->arena->clone(param_types),
        .return_type = return_type,
        .variadic = variadic,

        .stack_slots = Array<StackSlot>::create(this->arena),
        .blocks = Array<Block>::create(this->arena),
        .insts = Array<Inst>::create(this->arena),
        .param_insts = Array<InstRef>::create(this->arena),

        .block_nodes = Array<List<BlockRef>::Node *>::create(this->arena),
        .block_refs = List<BlockRef>::create(this->arena),

        .linkage = linkage,
        .calling_convention = calling_convention,
    };

    for (uint32_t i = 0; i < param_types.len; ++i) {
        Inst param_inst = {
            .func_param = {.index = i},
            .kind = InstKind_FunctionParameter,
            .type = param_types[i],
        };
        InstRef inst_ref = {(uint32_t)function.insts.len};
        function.insts.push_back(param_inst);
        function.param_insts.push_back(inst_ref);
    }

    FunctionRef ref = {(uint32_t)this->functions.len};

    this->function_map.set(function.name, ref);
    this->functions.push_back(function);

    return ref;
}

GlobalRef Module::add_global(Type *type, uint32_t flags, Slice<uint8_t> data)
{
    ZoneScoped;

    ACE_ASSERT(type->size_of(this) == data.len);

    Global global = {
        .type = type,
        .data = this->arena->clone(data),
        .flags = flags,
    };

    GlobalRef ref = {(uint32_t)this->globals.len};

    this->globals.push_back(global);

    return ref;
}

GlobalRef Module::add_global_string(const String &str)
{
    ZoneScoped;

    GlobalRef existing_global_ref = {0};
    if (this->global_string_map.get(str, &existing_global_ref)) {
        return existing_global_ref;
    }

    Global global = {
        .type = this->create_array_type(this->i8_type, str.len + 1),
        .data = {(uint8_t *)this->arena->null_terminate(str), str.len + 1},
        .flags = GlobalFlags_Initialized | GlobalFlags_ReadOnly,
    };

    GlobalRef ref = {(uint32_t)this->globals.len};

    this->globals.push_back(global);

    this->global_string_map.set(str, ref);

    return ref;
}

StackSlotRef Module::add_stack_slot(FunctionRef func_ref, Type *type)
{
    ZoneScoped;

    Function *func = &this->functions[func_ref.id];

    StackSlot stack_slot = {
        .type = type,
    };

    StackSlotRef ref = {(uint32_t)func->stack_slots.len};
    func->stack_slots.push_back(stack_slot);

    return ref;
}

InstRef Module::get_func_param(FunctionRef func_ref, uint32_t param_index)
{
    ZoneScoped;
    Function *func = &this->functions[func_ref.id];
    return func->param_insts[param_index];
}

BlockRef Module::insert_block_at_end(FunctionRef func_ref)
{
    ZoneScoped;

    auto func = &this->functions[func_ref.id];

    Block basic_block = {
        .inst_refs = Array<InstRef>::create(this->arena),
    };

    BlockRef ref = {
        .id = (uint32_t)func->blocks.len,
    };

    auto node = func->block_refs.push_back(ref);
    func->blocks.push_back(basic_block);
    func->block_nodes.push_back(node);

    return ref;
}

BlockRef Module::insert_block_after(FunctionRef func_ref, BlockRef other_ref)
{
    ZoneScoped;

    auto func = &this->functions[func_ref.id];

    auto other_node = func->block_nodes[other_ref.id];
    ACE_ASSERT(other_node);

    Block basic_block = {
        .inst_refs = Array<InstRef>::create(this->arena),
    };

    BlockRef ref = {.id = (uint32_t)func->blocks.len};

    auto node = func->block_refs.insert_after(other_node, ref);
    func->blocks.push_back(basic_block);
    func->block_nodes.push_back(node);

    return ref;
}

BlockRef Module::insert_block_before(FunctionRef func_ref, BlockRef other_ref)
{
    ZoneScoped;

    auto func = &this->functions[func_ref.id];

    auto other_node = func->block_nodes[other_ref.id];
    ACE_ASSERT(other_node);

    Block basic_block = {
        .inst_refs = Array<InstRef>::create(this->arena),
    };

    BlockRef ref = {(uint32_t)func->blocks.len};

    auto node = func->block_refs.insert_before(other_node, ref);
    func->blocks.push_back(basic_block);
    func->block_nodes.push_back(node);

    return ref;
}

Builder Builder::create(Module *module)
{
    return {
        .module = module,
        .current_func_ref = {UINT32_MAX},
        .current_block_ref = {UINT32_MAX},
    };
}

void Builder::set_function(FunctionRef func_ref)
{
    this->current_func_ref = func_ref;
}

void Builder::position_at_end(BlockRef block_ref)
{
    this->current_block_ref = block_ref;
}

static InstRef push_instruction(Builder *builder, Inst inst)
{
    ZoneScoped;

    Function *func = &builder->module->functions[builder->current_func_ref.id];
    Block *block = &func->blocks[builder->current_block_ref.id];

    InstRef ref = {(uint32_t)func->insts.len};

    func->insts.push_back(inst);
    block->inst_refs.push_back(ref);

    return ref;
}

InstRef Builder::insert_get_const(ConstRef const_ref)
{
    Const *constant = &this->module->consts[const_ref.id];

    Inst inst = {};
    inst.kind = InstKind_GetConst;
    inst.type = constant->type;
    inst.get_const.const_ref = const_ref;

    return push_instruction(this, inst);
}

InstRef Builder::insert_global_addr(GlobalRef global_ref)
{
    ZoneScoped;

    Global *global = &this->module->globals[global_ref.id];

    Inst inst = {};
    inst.kind = InstKind_GlobalAddr;
    inst.type = this->module->create_pointer_type(global->type);
    inst.global_addr = {global_ref};

    return push_instruction(this, inst);
}

InstRef Builder::insert_global_load(GlobalRef global_ref)
{
    ZoneScoped;

    Global *global = &this->module->globals[global_ref.id];

    Inst inst = {};
    inst.kind = InstKind_GlobalLoad;
    inst.type = global->type;
    inst.global_load = {global_ref};

    return push_instruction(this, inst);
}

void Builder::insert_global_store(GlobalRef global_ref, InstRef inst_ref)
{
    ZoneScoped;

    Function *func = &this->module->functions[this->current_func_ref.id];

    Inst inst = {};
    inst.kind = InstKind_GlobalStore;
    inst.type = func->insts[inst_ref.id].type;
    inst.global_store = {global_ref, inst_ref};

    push_instruction(this, inst);
}

InstRef Builder::insert_stack_addr(StackSlotRef ss_ref)
{
    ZoneScoped;

    Function *func = &this->module->functions[this->current_func_ref.id];
    StackSlot *ss = &func->stack_slots[ss_ref.id];

    Inst inst = {};
    inst.kind = InstKind_StackAddr;
    inst.type = this->module->create_pointer_type(ss->type);
    inst.stack_addr.ss_ref = {ss_ref};

    return push_instruction(this, inst);
}

InstRef Builder::insert_stack_load(StackSlotRef ss_ref)
{
    ZoneScoped;

    Function *func = &this->module->functions[this->current_func_ref.id];

    Inst inst = {};
    inst.kind = InstKind_StackLoad;
    inst.type = func->stack_slots[ss_ref.id].type;
    inst.stack_load = {ss_ref};

    return push_instruction(this, inst);
}

void Builder::insert_stack_store(StackSlotRef ss_ref, InstRef inst_ref)
{
    ZoneScoped;

    Inst inst = {};
    inst.kind = InstKind_StackStore;
    inst.stack_store = {ss_ref, inst_ref};

    push_instruction(this, inst);
}

InstRef Builder::insert_ptr_cast(Type *dest_type, InstRef inst_ref)
{
    ZoneScoped;

    ACE_ASSERT(dest_type->kind == TypeKind_Pointer);

    Inst inst = {};
    inst.kind = InstKind_PtrCast;
    inst.type = dest_type;
    inst.ptr_cast = {inst_ref};

    return push_instruction(this, inst);
}

InstRef Builder::insert_func_call(
    FunctionRef func_ref, const Slice<InstRef> &parameters)
{
    ZoneScoped;

    Function *called_function = &this->module->functions[func_ref.id];

    Inst inst = {};
    inst.kind = InstKind_FuncCall;
    inst.type = called_function->return_type;
    inst.func_call = {func_ref, this->module->arena->clone(parameters)};

    return push_instruction(this, inst);
}

void Builder::insert_jump(BlockRef block_ref)
{
    ZoneScoped;

    Inst inst = {};
    inst.kind = InstKind_Jump;
    inst.jump = {block_ref};

    push_instruction(this, inst);
}

void Builder::insert_return_value(InstRef inst_ref)
{
    ZoneScoped;

    Inst inst = {};
    inst.kind = InstKind_ReturnValue;
    inst.return_value.inst_ref = inst_ref;

    push_instruction(this, inst);
}

void Builder::insert_return_void()
{
    ZoneScoped;

    Inst inst = {};
    inst.kind = InstKind_ReturnVoid;

    push_instruction(this, inst);
}
