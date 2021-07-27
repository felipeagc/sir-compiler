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

const char *binop_to_string(BinaryOperation op)
{
    switch (op) {
    case BinaryOperation_Unknown:
    case BinaryOperation_MAX: break;
    case BinaryOperation_IAdd: return "iadd";
    case BinaryOperation_ISub: return "isub";
    case BinaryOperation_IMul: return "imul";
    case BinaryOperation_UDiv: return "udiv";
    case BinaryOperation_SDiv: return "sdiv";
    case BinaryOperation_URem: return "urem";
    case BinaryOperation_SRem: return "srem";

    case BinaryOperation_FAdd: return "fadd";
    case BinaryOperation_FSub: return "fsub";
    case BinaryOperation_FMul: return "fmul";
    case BinaryOperation_FDiv: return "fdiv";
    case BinaryOperation_FRem: return "frem";

    case BinaryOperation_IEQ: return "ieq";
    case BinaryOperation_INE: return "ine";
    case BinaryOperation_UGT: return "ugt";
    case BinaryOperation_UGE: return "uge";
    case BinaryOperation_ULT: return "ult";
    case BinaryOperation_ULE: return "ule";
    case BinaryOperation_SGT: return "sgt";
    case BinaryOperation_SGE: return "sge";
    case BinaryOperation_SLT: return "slt";
    case BinaryOperation_SLE: return "sle";

    case BinaryOperation_FEQ: return "feq";
    case BinaryOperation_FNE: return "fne";
    case BinaryOperation_FGT: return "fgt";
    case BinaryOperation_FGE: return "fge";
    case BinaryOperation_FLT: return "flt";
    case BinaryOperation_FLE: return "fle";

    case BinaryOperation_Shl: return "shl";
    case BinaryOperation_AShr: return "ashr";
    case BinaryOperation_LShr: return "lshr";

    case BinaryOperation_And: return "and";
    case BinaryOperation_Or: return "or";
    case BinaryOperation_Xor: return "xor";
    }

    return "<unknown op>";
}

static void
print_instruction(Module *module, InstRef inst_ref, StringBuilder *sb)
{
    ZoneScoped;

    (void)module;
    Inst inst = module->insts[inst_ref.id];

    switch (inst.kind) {
    case InstKind_Unknown: ACE_ASSERT(0); break;
    case InstKind_Function: ACE_ASSERT(0); break;
    case InstKind_FunctionParameter: ACE_ASSERT(0); break;
    case InstKind_Block: ACE_ASSERT(0); break;

    case InstKind_ImmediateInt: {
        String type_string = inst.type->to_string(module);
        sb->sprintf(
            "%%r%u = imm_int %.*s %lu",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.imm_int.u64);
        break;
    }

    case InstKind_ImmediateFloat: {
        String type_string = inst.type->to_string(module);
        sb->sprintf(
            "%%r%u = imm_float %.*s %lf",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.imm_float.f64);
        break;
    }

    case InstKind_ImmediateBool: {
        String type_string = inst.type->to_string(module);
        sb->sprintf(
            "%%r%u = imm_bool %.*s %s",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.imm_bool.value ? "true" : "false");
        break;
    }

    case InstKind_PtrCast: {
        String type_string = inst.type->to_string(module);
        sb->sprintf(
            "%%r%u = ptr_cast %.*s %%r%u",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.ptr_cast.inst_ref.id);
        break;
    }

    case InstKind_ZExt: {
        String type_string = inst.type->to_string(module);
        sb->sprintf(
            "%%r%u = zext %.*s %%r%u",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.zext.inst_ref.id);
        break;
    }

    case InstKind_SExt: {
        String type_string = inst.type->to_string(module);
        sb->sprintf(
            "%%r%u = sext %.*s %%r%u",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.sext.inst_ref.id);
        break;
    }

    case InstKind_Trunc: {
        String type_string = inst.type->to_string(module);
        sb->sprintf(
            "%%r%u = trunc %.*s %%r%u",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.trunc.inst_ref.id);
        break;
    }

    case InstKind_Load: {
        String type_string = inst.type->to_string(module);
        sb->sprintf(
            "%%r%u = load %.*s %%r%u",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.load.ptr_ref.id);
        break;
    }

    case InstKind_Binop: {
        String type_string = inst.type->to_string(module);
        sb->sprintf(
            "%%r%u = %s %.*s %%r%u %%r%u",
            inst_ref.id,
            binop_to_string(inst.binop.op),
            (int)type_string.len,
            type_string.ptr,
            inst.binop.left_ref.id,
            inst.binop.right_ref.id);
        break;
    }

    case InstKind_ArrayElemPtr: {
        String type_string = inst.type->to_string(module);
        sb->sprintf(
            "%%r%u = array_elem_ptr %.*s %%r%u %%r%u",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.array_elem_ptr.accessed_ref.id,
            inst.array_elem_ptr.index_ref.id);
        break;
    }

    case InstKind_StructElemPtr: {
        String type_string = inst.type->to_string(module);
        sb->sprintf(
            "%%r%u = struct_elem_ptr %.*s %%r%u %u",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.struct_elem_ptr.accessed_ref.id,
            inst.struct_elem_ptr.field_index);
        break;
    }

    case InstKind_FuncCall: {
        sb->sprintf(
            "%%r%u = func_call %%r%u (",
            inst_ref.id,
            inst.func_call.func_ref.id);
        for (size_t i = 0; i < inst.func_call.parameters.len; ++i) {
            if (i > 0) sb->append(", ");
            InstRef param_inst_ref = inst.func_call.parameters[i];
            sb->sprintf("%%r%u", param_inst_ref.id);
        }
        sb->append(")");
        break;
    }

    case InstKind_Global: {
        String type_string = inst.type->to_string(module);
        sb->sprintf(
            "%%r%u = global (%.*s)",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr);
        break;
    }
    case InstKind_StackSlot: {
        String type_string = inst.type->to_string(module);
        sb->sprintf(
            "%%r%u = stack_slot (%.*s)",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr);
        break;
    }
    case InstKind_Store: {
        String type_string =
            inst.store.value_ref.get(module).type->to_string(module);
        sb->sprintf(
            "store %.*s %%r%u %%r%u",
            (int)type_string.len,
            type_string.ptr,
            inst.store.ptr_ref.id,
            inst.store.value_ref.id);
        break;
    }
    case InstKind_ReturnVoid: {
        sb->sprintf("return_void");
        break;
    }
    case InstKind_ReturnValue: {
        String type_string =
            inst.return_value.inst_ref.get(module).type->to_string(module);
        sb->sprintf(
            "return_value %.*s %%r%u",
            (int)type_string.len,
            type_string.ptr,
            inst.return_value.inst_ref.id);
        break;
    }
    case InstKind_Jump: {
        sb->sprintf("jump %%r%u", inst.jump.block_ref.id);
        break;
    }
    case InstKind_Branch: {
        sb->sprintf(
            "branch %%r%u %%b%u %%b%u",
            inst.branch.cond_inst_ref.id,
            inst.branch.true_block_ref.id,
            inst.branch.false_block_ref.id);
        break;
    }
    }

    sb->append("\n");
}

static void print_block(Module *module, InstRef block_ref, StringBuilder *sb)
{
    ZoneScoped;

    Inst *block = &module->insts[block_ref.id];

    sb->sprintf("  block %%r%u:\n", block_ref.id);

    for (InstRef inst_ref : block->block.inst_refs) {
        sb->append("    ");
        print_instruction(module, inst_ref, sb);
    }

    sb->append("\n");
}

static void print_function(Module *module, InstRef func_ref, StringBuilder *sb)
{
    ZoneScoped;

    Function *func = module->insts[func_ref.id].func;

    String calling_conv_str =
        calling_convention_to_string(func->calling_convention);
    String linkage_str = linkage_to_string(func->linkage);
    sb->sprintf(
        "function %%r%u [%.*s, %.*s] \"%.*s\" (",
        func_ref.id,
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

    for (InstRef stack_slot_ref : func->stack_slots) {
        Inst *stack_slot = &module->insts[stack_slot_ref.id];
        ACE_ASSERT(stack_slot->type);
        String type_string = stack_slot->type->to_string(module);
        sb->sprintf(
            "  %%r%u: stack_slot %.*s\n",
            stack_slot_ref.id,
            (int)type_string.len,
            type_string.ptr);
    }

    if (func->stack_slots.len > 0) {
        sb->append("\n");
    }

    for (InstRef block_ref : func->blocks) {
        print_block(module, block_ref, sb);
    }

    sb->append("}\n\n");
}

Module *Module::create(TargetArch target_arch, Endianness endianness)
{
    auto parent_allocator = MallocAllocator::get_instance();

    auto arena = ArenaAllocator::create(parent_allocator);
    auto insts = Array<Inst>::create(MallocAllocator::get_instance());
    auto globals = Array<InstRef>::create(MallocAllocator::get_instance());
    auto functions = Array<InstRef>::create(MallocAllocator::get_instance());
    auto function_map = StringMap<InstRef>::create(
        MallocAllocator::get_instance());
    auto global_string_map = StringMap<InstRef>::create(
        MallocAllocator::get_instance());
    auto type_map = StringMap<Type *>::create(
        MallocAllocator::get_instance());

    insts.push_back({}); // 0th inst

    Module *module = parent_allocator->alloc<Module>();
    *module = {
        .arena = arena,
        .insts = insts,
        .globals = globals,
        .functions = functions,
        .function_map = function_map,
        .global_string_map = global_string_map,
        .type_map = type_map,
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
    this->insts.destroy();
    this->globals.destroy();
    this->functions.destroy();
    this->function_map.destroy();
    this->global_string_map.destroy();
    this->type_map.destroy();
    this->arena->destroy();

    auto parent_allocator = MallocAllocator::get_instance();
    parent_allocator->free(this);
}

String Module::print_alloc(Allocator *allocator)
{
    ZoneScoped;

    StringBuilder sb = StringBuilder::create(allocator);

    for (InstRef global_ref : this->globals) {
        print_instruction(this, global_ref, &sb);
    }

    sb.append("\n");

    for (InstRef func_ref : this->functions) {
        print_function(this, func_ref, &sb);
    }

    String result = sb.build_null_terminated(allocator);
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
    type->kind = TypeKind_Struct;
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

ACE_INLINE
static InstRef module_add_inst(Module *module, const Inst &inst)
{
    InstRef ref = {(uint32_t)module->insts.len};
    module->insts.push_back(inst);
    return ref;
}

InstRef Module::add_function(
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

    Function *function = this->arena->alloc<Function>();
    *function = Function{
        .name = func_name,
        .param_types = this->arena->clone(param_types),
        .return_type = return_type,
        .variadic = variadic,

        .stack_slots = Array<InstRef>::create(this->arena),
        .blocks = Array<InstRef>::create(this->arena),
        .param_insts = Array<InstRef>::create(this->arena),

        .linkage = linkage,
        .calling_convention = calling_convention,
    };

    for (uint32_t i = 0; i < param_types.len; ++i) {
        Inst param_inst = {
            .func_param = {.index = i},
            .kind = InstKind_FunctionParameter,
            .type = param_types[i],
        };
        InstRef inst_ref = module_add_inst(this, param_inst);
        function->param_insts.push_back(inst_ref);
    }

    Inst func_inst = {};
    func_inst.kind = InstKind_Function;
    func_inst.func = function;
    InstRef func_ref = module_add_inst(this, func_inst);

    this->functions.push_back(func_ref);
    this->function_map.set(function->name, func_ref);

    return func_ref;
}

InstRef Module::add_global(Type *type, uint32_t flags, Slice<uint8_t> data)
{
    ZoneScoped;

    ACE_ASSERT(type->size_of(this) == data.len);

    Inst global = {};
    global.kind = InstKind_Global;
    global.type = this->create_pointer_type(type);
    global.global.data = this->arena->clone(data);
    global.global.flags = flags;

    InstRef global_ref = module_add_inst(this, global);

    this->globals.push_back(global_ref);

    return global_ref;
}

InstRef Module::add_global_string(const String &str)
{
    ZoneScoped;

    InstRef existing_global_ref = {0};
    if (this->global_string_map.get(str, &existing_global_ref)) {
        return existing_global_ref;
    }

    Inst global = {};
    global.kind = InstKind_Global;
    global.type = this->create_pointer_type(this->i8_type);
    global.global.data = {
        (uint8_t *)this->arena->null_terminate(str), str.len + 1};
    global.global.flags = GlobalFlags_Initialized | GlobalFlags_ReadOnly;

    InstRef global_ref = module_add_inst(this, global);

    this->globals.push_back(global_ref);

    this->global_string_map.set(str, global_ref);

    return global_ref;
}

InstRef Module::add_stack_slot(InstRef func_ref, Type *type)
{
    ZoneScoped;

    Inst func_inst = func_ref.get(this);
    ACE_ASSERT(func_inst.kind == InstKind_Function);

    Function *func = func_inst.func;

    Inst stack_slot = {};
    stack_slot.kind = InstKind_StackSlot;
    stack_slot.type = this->create_pointer_type(type);
    InstRef stack_slot_ref = module_add_inst(this, stack_slot);

    func->stack_slots.push_back(stack_slot_ref);

    return stack_slot_ref;
}

InstRef Module::get_func_param(InstRef func_ref, uint32_t param_index)
{
    ZoneScoped;

    Inst func_inst = func_ref.get(this);
    ACE_ASSERT(func_inst.kind == InstKind_Function);
    Function *func = func_inst.func;

    return func->param_insts[param_index];
}

InstRef Module::insert_block_at_end(InstRef func_ref)
{
    ZoneScoped;

    Inst func_inst = func_ref.get(this);
    ACE_ASSERT(func_inst.kind == InstKind_Function);
    Function *func = func_inst.func;

    Inst block = {};
    block.kind = InstKind_Block;
    block.block.inst_refs = Array<InstRef>::create(this->arena);
    InstRef block_ref = module_add_inst(this, block);

    func->blocks.push_back(block_ref);

    return block_ref;
}

/* InstRef Module::insert_block_after(InstRef func_ref, InstRef other_ref) */
/* { */
/*     ZoneScoped; */

/*     Inst func_inst = func_ref.get(this); */
/*     ACE_ASSERT(func_inst.kind == InstKind_Function); */
/*     Function *func = func_inst.func; */

/*     Inst block = {}; */
/*     block.kind = InstKind_Block; */
/*     block.block.inst_refs = Array<InstRef>::create(this->arena); */
/*     InstRef block_ref = this->add_inst(block); */

/*     auto other_node = func->block_nodes[other_ref.id]; */
/*     ACE_ASSERT(other_node); */

/*     Block basic_block = { */
/*         .inst_refs = Array<InstRef>::create(this->arena), */
/*     }; */

/*     BlockRef ref = {.id = (uint32_t)func->blocks.len}; */

/*     auto node = func->block_refs.insert_after(other_node, ref); */
/*     func->blocks.push_back(basic_block); */
/*     func->block_nodes.push_back(node); */

/*     return ref; */
/* } */

/* BlockRef Module::insert_block_before(FunctionRef func_ref, BlockRef
 * other_ref) */
/* { */
/*     ZoneScoped; */

/*     auto func = &this->functions[func_ref.id]; */

/*     auto other_node = func->block_nodes[other_ref.id]; */
/*     ACE_ASSERT(other_node); */

/*     Block basic_block = { */
/*         .inst_refs = Array<InstRef>::create(this->arena), */
/*     }; */

/*     BlockRef ref = {(uint32_t)func->blocks.len}; */

/*     auto node = func->block_refs.insert_before(other_node, ref); */
/*     func->blocks.push_back(basic_block); */
/*     func->block_nodes.push_back(node); */

/*     return ref; */
/* } */

Builder Builder::create(Module *module)
{
    return {
        .module = module,
        .current_func_ref = {UINT32_MAX},
        .current_block_ref = {UINT32_MAX},
    };
}

void Builder::set_function(InstRef func_ref)
{
    this->current_func_ref = func_ref;
}

void Builder::position_at_end(InstRef block_ref)
{
    this->current_block_ref = block_ref;
}

static InstRef builder_insert_inst(Builder *builder, const Inst &inst)
{
    ZoneScoped;

    InstRef ref = module_add_inst(builder->module, inst);

    Inst *block = &builder->module->insts[builder->current_block_ref.id];
    block->block.inst_refs.push_back(ref);

    return ref;
}

InstRef Builder::insert_imm_int(Type *type, uint64_t value)
{
    ZoneScoped;

    Inst inst = {};
    inst.kind = InstKind_ImmediateInt;
    inst.type = type;
    inst.imm_int.u64 = value;

    return builder_insert_inst(this, inst);
}

InstRef Builder::insert_imm_float(Type *type, double value)
{
    ZoneScoped;

    Inst inst = {};
    inst.kind = InstKind_ImmediateFloat;
    inst.type = type;
    inst.imm_float.f64 = value;

    return builder_insert_inst(this, inst);
}

InstRef Builder::insert_imm_bool(bool value)
{
    ZoneScoped;

    Inst inst = {};
    inst.kind = InstKind_ImmediateBool;
    inst.type = this->module->bool_type;
    inst.imm_bool.value = value;

    return builder_insert_inst(this, inst);
}

InstRef Builder::insert_array_elem_ptr(InstRef accessed_ref, InstRef index_ref)
{
    ZoneScoped;

    Inst accessed_inst = accessed_ref.get(this->module);
    ACE_ASSERT(accessed_inst.type->kind == TypeKind_Pointer);
    ACE_ASSERT(accessed_inst.type->pointer.sub->kind == TypeKind_Array);

    Inst inst = {};
    inst.kind = InstKind_ArrayElemPtr;
    inst.type = this->module->create_pointer_type(
        accessed_inst.type->pointer.sub->array.sub);
    inst.array_elem_ptr.accessed_ref = accessed_ref;
    inst.array_elem_ptr.index_ref = index_ref;

    return builder_insert_inst(this, inst);
}

InstRef
Builder::insert_struct_elem_ptr(InstRef accessed_ref, uint32_t field_index)
{
    ZoneScoped;

    Inst accessed_inst = accessed_ref.get(this->module);
    ACE_ASSERT(accessed_inst.type->kind == TypeKind_Pointer);
    ACE_ASSERT(accessed_inst.type->pointer.sub->kind == TypeKind_Struct);

    Inst inst = {};
    inst.kind = InstKind_StructElemPtr;
    inst.type = this->module->create_pointer_type(
        accessed_inst.type->pointer.sub->struct_.fields[field_index]);
    inst.struct_elem_ptr.accessed_ref = accessed_ref;
    inst.struct_elem_ptr.field_index = field_index;

    return builder_insert_inst(this, inst);
}

void Builder::insert_store(InstRef ptr_ref, InstRef value_ref)
{
    ZoneScoped;

    ACE_ASSERT(ptr_ref.get(this->module).type->kind == TypeKind_Pointer);
    ACE_ASSERT(
        ptr_ref.get(this->module).type->pointer.sub ==
        value_ref.get(this->module).type);

    Inst inst = {};
    inst.kind = InstKind_Store;
    inst.store.ptr_ref = ptr_ref;
    inst.store.value_ref = value_ref;

    builder_insert_inst(this, inst);
}

InstRef Builder::insert_load(InstRef ptr_ref)
{
    ZoneScoped;

    Inst inst = {};
    inst.kind = InstKind_Load;
    inst.type = ptr_ref.get(this->module).type->pointer.sub;
    inst.load.ptr_ref = ptr_ref;

    return builder_insert_inst(this, inst);
}

InstRef Builder::insert_ptr_cast(Type *dest_type, InstRef inst_ref)
{
    ZoneScoped;

    ACE_ASSERT(dest_type->kind == TypeKind_Pointer);

    Inst inst = {};
    inst.kind = InstKind_PtrCast;
    inst.type = dest_type;
    inst.ptr_cast = {inst_ref};

    return builder_insert_inst(this, inst);
}

InstRef Builder::insert_zext(Type *dest_type, InstRef inst_ref)
{
    ZoneScoped;

    ACE_ASSERT(dest_type->kind == TypeKind_Int);

    Inst inst = {};
    inst.kind = InstKind_ZExt;
    inst.type = dest_type;
    inst.zext = {inst_ref};

    return builder_insert_inst(this, inst);
}

InstRef Builder::insert_sext(Type *dest_type, InstRef inst_ref)
{
    ZoneScoped;

    ACE_ASSERT(dest_type->kind == TypeKind_Int);

    Inst inst = {};
    inst.kind = InstKind_SExt;
    inst.type = dest_type;
    inst.sext = {inst_ref};

    return builder_insert_inst(this, inst);
}

InstRef Builder::insert_trunc(Type *dest_type, InstRef inst_ref)
{
    ZoneScoped;

    ACE_ASSERT(dest_type->kind == TypeKind_Int);

    Inst inst = {};
    inst.kind = InstKind_Trunc;
    inst.type = dest_type;
    inst.trunc = {inst_ref};

    return builder_insert_inst(this, inst);
}

InstRef
Builder::insert_binop(BinaryOperation op, InstRef left_ref, InstRef right_ref)
{
    ZoneScoped;

    Inst inst = {};
    inst.kind = InstKind_Binop;
    switch (op) {
    case BinaryOperation_Unknown:
    case BinaryOperation_MAX: ACE_ASSERT(0); break;

    case BinaryOperation_IAdd:
    case BinaryOperation_ISub:
    case BinaryOperation_IMul:
    case BinaryOperation_SDiv:
    case BinaryOperation_UDiv:
    case BinaryOperation_SRem:
    case BinaryOperation_URem:

    case BinaryOperation_FAdd:
    case BinaryOperation_FSub:
    case BinaryOperation_FMul:
    case BinaryOperation_FDiv:
    case BinaryOperation_FRem: {
        inst.type = left_ref.get(this->module).type;
        break;
    }

    case BinaryOperation_IEQ:
    case BinaryOperation_INE:
    case BinaryOperation_UGT:
    case BinaryOperation_UGE:
    case BinaryOperation_ULT:
    case BinaryOperation_ULE:
    case BinaryOperation_SGT:
    case BinaryOperation_SGE:
    case BinaryOperation_SLT:
    case BinaryOperation_SLE:

    case BinaryOperation_FEQ:
    case BinaryOperation_FNE:
    case BinaryOperation_FGT:
    case BinaryOperation_FGE:
    case BinaryOperation_FLT:
    case BinaryOperation_FLE: {
        inst.type = this->module->bool_type;
        break;
    }

    case BinaryOperation_Shl:
    case BinaryOperation_AShr:
    case BinaryOperation_LShr: {
        inst.type = left_ref.get(this->module).type;
        break;
    }

    case BinaryOperation_And:
    case BinaryOperation_Or:
    case BinaryOperation_Xor: {
        inst.type = left_ref.get(this->module).type;
        break;
    }
    }

    ACE_ASSERT(inst.type);

    inst.binop.op = op;
    inst.binop.left_ref = left_ref;
    inst.binop.right_ref = right_ref;

    return builder_insert_inst(this, inst);
}

InstRef
Builder::insert_func_call(InstRef func_ref, const Slice<InstRef> &parameters)
{
    ZoneScoped;

    Inst func_inst = func_ref.get(this->module);
    ACE_ASSERT(func_inst.kind == InstKind_Function);
    Function *called_function = func_inst.func;

    Inst inst = {};
    inst.kind = InstKind_FuncCall;
    inst.type = called_function->return_type;
    inst.func_call = {func_ref, this->module->arena->clone(parameters)};

    return builder_insert_inst(this, inst);
}

void Builder::insert_jump(InstRef block_ref)
{
    ZoneScoped;

    Inst inst = {};
    inst.kind = InstKind_Jump;
    inst.jump = {block_ref};

    builder_insert_inst(this, inst);
}

void Builder::insert_branch(
    InstRef cond_ref, InstRef true_block_ref, InstRef false_block_ref)
{
    ZoneScoped;

    ACE_ASSERT(cond_ref.get(this->module).type->kind == TypeKind_Bool);
    ACE_ASSERT(true_block_ref.get(this->module).kind == InstKind_Block);
    ACE_ASSERT(false_block_ref.get(this->module).kind == InstKind_Block);

    Inst inst = {};
    inst.kind = InstKind_Branch;
    inst.branch.cond_inst_ref = cond_ref;
    inst.branch.true_block_ref = true_block_ref;
    inst.branch.false_block_ref = false_block_ref;

    builder_insert_inst(this, inst);
}

void Builder::insert_return_value(InstRef inst_ref)
{
    ZoneScoped;

    Inst inst = {};
    inst.kind = InstKind_ReturnValue;
    inst.return_value.inst_ref = inst_ref;

    builder_insert_inst(this, inst);
}

void Builder::insert_return_void()
{
    ZoneScoped;

    Inst inst = {};
    inst.kind = InstKind_ReturnVoid;

    builder_insert_inst(this, inst);
}
