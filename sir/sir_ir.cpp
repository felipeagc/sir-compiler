#include "sir_ir.hpp"

SIRString SIRCallingConventionToString(SIRCallingConvention calling_convention)
{
    switch (calling_convention) {
    case SIRCallingConvention_SystemV: return SIR_STR("system_v");
    }

    SIR_ASSERT(0);
    return (SIRString){};
}

SIRString SIRLinkageToString(SIRLinkage linkage)
{
    switch (linkage) {
    case SIRLinkage_Internal: return SIR_STR("internal");
    case SIRLinkage_External: return SIR_STR("external");
    case SIRLinkage_Interpeter: return SIR_STR("interpreted");
    }

    SIR_ASSERT(0);
    return (SIRString){};
}

static const char *binop_to_string(SIRBinaryOperation op)
{
    switch (op) {
    case SIRBinaryOperation_Unknown:
    case SIRBinaryOperation_MAX: break;
    case SIRBinaryOperation_IAdd: return "iadd";
    case SIRBinaryOperation_ISub: return "isub";
    case SIRBinaryOperation_IMul: return "imul";
    case SIRBinaryOperation_UDiv: return "udiv";
    case SIRBinaryOperation_SDiv: return "sdiv";
    case SIRBinaryOperation_URem: return "urem";
    case SIRBinaryOperation_SRem: return "srem";

    case SIRBinaryOperation_FAdd: return "fadd";
    case SIRBinaryOperation_FSub: return "fsub";
    case SIRBinaryOperation_FMul: return "fmul";
    case SIRBinaryOperation_FDiv: return "fdiv";

    case SIRBinaryOperation_IEQ: return "ieq";
    case SIRBinaryOperation_INE: return "ine";
    case SIRBinaryOperation_UGT: return "ugt";
    case SIRBinaryOperation_UGE: return "uge";
    case SIRBinaryOperation_ULT: return "ult";
    case SIRBinaryOperation_ULE: return "ule";
    case SIRBinaryOperation_SGT: return "sgt";
    case SIRBinaryOperation_SGE: return "sge";
    case SIRBinaryOperation_SLT: return "slt";
    case SIRBinaryOperation_SLE: return "sle";

    case SIRBinaryOperation_FEQ: return "feq";
    case SIRBinaryOperation_FNE: return "fne";
    case SIRBinaryOperation_FGT: return "fgt";
    case SIRBinaryOperation_FGE: return "fge";
    case SIRBinaryOperation_FLT: return "flt";
    case SIRBinaryOperation_FLE: return "fle";

    case SIRBinaryOperation_Shl: return "shl";
    case SIRBinaryOperation_AShr: return "ashr";
    case SIRBinaryOperation_LShr: return "lshr";

    case SIRBinaryOperation_And: return "and";
    case SIRBinaryOperation_Or: return "or";
    case SIRBinaryOperation_Xor: return "xor";
    }

    return "<unknown op>";
}

static void print_instruction(
    SIRModule *module,
    SIRInstRef inst_ref,
    SIRStringBuilder *sb,
    void *user_data,
    SIRAuxInstPrinter *aux_printer)
{
    ZoneScoped;

    SIRInst inst = module->insts[inst_ref.id];

    int64_t start_pos = sb->array.len;

    switch (inst.kind) {
    case SIRInstKind_Unknown: SIR_ASSERT(0); break;
    case SIRInstKind_Function: SIR_ASSERT(0); break;
    case SIRInstKind_FunctionParameter: SIR_ASSERT(0); break;
    case SIRInstKind_Block: SIR_ASSERT(0); break;

    case SIRInstKind_Alias: {
        sb->sprintf("%%r%u = alias %%r%u", inst_ref.id, inst.op1.id);
        break;
    }

    case SIRInstKind_PushFunctionParameter: {
        sb->sprintf("push_func_param %%r%u", inst.op1.id);
        break;
    }

    case SIRInstKind_SetCond: {
        sb->sprintf("set_cond %%r%u", inst.op1.id);
        break;
    }

    case SIRInstKind_ConstInt: {
        SIRString type_string = SIRTypeToString(module, inst.type);
        sb->sprintf(
            "%%r%u = const_int %.*s %lu",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.const_int.u64);
        break;
    }

    case SIRInstKind_ConstFloat: {
        SIRString type_string = SIRTypeToString(module, inst.type);
        sb->sprintf(
            "%%r%u = const_float %.*s %lf",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.const_float.f64);
        break;
    }

    case SIRInstKind_ConstBool: {
        SIRString type_string = SIRTypeToString(module, inst.type);
        sb->sprintf(
            "%%r%u = const_bool %.*s %s",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.const_bool.value ? "true" : "false");
        break;
    }

    case SIRInstKind_BitCast: {
        SIRString type_string = SIRTypeToString(module, inst.type);
        sb->sprintf(
            "%%r%u = bit_cast %.*s %%r%u",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.op1.id);
        break;
    }

    case SIRInstKind_ZExt: {
        SIRString type_string = SIRTypeToString(module, inst.type);
        sb->sprintf(
            "%%r%u = zext %.*s %%r%u",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.op1.id);
        break;
    }

    case SIRInstKind_SExt: {
        SIRString type_string = SIRTypeToString(module, inst.type);
        sb->sprintf(
            "%%r%u = sext %.*s %%r%u",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.op1.id);
        break;
    }

    case SIRInstKind_Trunc: {
        SIRString type_string = SIRTypeToString(module, inst.type);
        sb->sprintf(
            "%%r%u = trunc %.*s %%r%u",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.op1.id);
        break;
    }

    case SIRInstKind_FPTrunc: {
        SIRString type_string = SIRTypeToString(module, inst.type);
        sb->sprintf(
            "%%r%u = fptrunc %.*s %%r%u",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.op1.id);
        break;
    }

    case SIRInstKind_FPExt: {
        SIRString type_string = SIRTypeToString(module, inst.type);
        sb->sprintf(
            "%%r%u = fpext %.*s %%r%u",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.op1.id);
        break;
    }

    case SIRInstKind_SIToFP: {
        SIRString type_string = SIRTypeToString(module, inst.type);
        sb->sprintf(
            "%%r%u = sitofp %.*s %%r%u",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.op1.id);
        break;
    }

    case SIRInstKind_UIToFP: {
        SIRString type_string = SIRTypeToString(module, inst.type);
        sb->sprintf(
            "%%r%u = uitofp %.*s %%r%u",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.op1.id);
        break;
    }

    case SIRInstKind_FPToSI: {
        SIRString type_string = SIRTypeToString(module, inst.type);
        sb->sprintf(
            "%%r%u = fptosi %.*s %%r%u",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.op1.id);
        break;
    }

    case SIRInstKind_FPToUI: {
        SIRString type_string = SIRTypeToString(module, inst.type);
        sb->sprintf(
            "%%r%u = fptoui %.*s %%r%u",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.op1.id);
        break;
    }

    case SIRInstKind_FNeg: {
        SIRString type_string = SIRTypeToString(module, inst.type);
        sb->sprintf(
            "%%r%u = fneg %.*s %%r%u",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.op1.id);
        break;
    }

    case SIRInstKind_Load: {
        SIRString type_string = SIRTypeToString(module, inst.type);
        sb->sprintf(
            "%%r%u = load %.*s %%r%u",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.op1.id);
        break;
    }

    case SIRInstKind_Binop: {
        SIRString type_string = SIRTypeToString(module, inst.type);
        sb->sprintf(
            "%%r%u = %s %.*s %%r%u %%r%u",
            inst_ref.id,
            binop_to_string(inst.binop),
            (int)type_string.len,
            type_string.ptr,
            inst.op1.id,
            inst.op2.id);
        break;
    }

    case SIRInstKind_ArrayElemPtr: {
        SIRString type_string = SIRTypeToString(module, inst.type);
        sb->sprintf(
            "%%r%u = array_elem_ptr %.*s %%r%u %%r%u",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.array_elem_ptr.accessed_ref.id,
            inst.array_elem_ptr.index_ref.id);
        break;
    }

    case SIRInstKind_StructElemPtr: {
        SIRString type_string = SIRTypeToString(module, inst.type);
        sb->sprintf(
            "%%r%u = struct_elem_ptr %.*s %%r%u %%r%u",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.struct_elem_ptr.accessed_ref.id,
            inst.struct_elem_ptr.field_index_ref.id);
        break;
    }

    case SIRInstKind_ExtractArrayElem: {
        SIRString type_string = SIRTypeToString(module, inst.type);
        sb->sprintf(
            "%%r%u = extract_array_elem %.*s %%r%u %%r%u",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.extract_array_elem.accessed_ref.id,
            inst.extract_array_elem.index_ref.id);
        break;
    }

    case SIRInstKind_ExtractStructElem: {
        SIRString type_string = SIRTypeToString(module, inst.type);
        sb->sprintf(
            "%%r%u = extract_struct_elem %.*s %%r%u %%r%u",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr,
            inst.extract_struct_elem.accessed_ref.id,
            inst.extract_struct_elem.field_index_ref.id);
        break;
    }

    case SIRInstKind_FuncCall: {
        sb->sprintf("%%r%u = func_call %%r%u", inst_ref.id, inst.op1.id);
        break;
    }

    case SIRInstKind_Global: {
        SIRString type_string = SIRTypeToString(module, inst.type);
        sb->sprintf(
            "%%r%u = global (%.*s)",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr);
        break;
    }
    case SIRInstKind_StackSlot: {
        SIRString type_string = SIRTypeToString(module, inst.type);
        sb->sprintf(
            "%%r%u = stack_slot (%.*s)",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr);
        break;
    }
    case SIRInstKind_Store: {
        SIRString type_string = SIRTypeToString(
            module, SIRModuleGetInst(module, inst.store.value_ref).type);
        sb->sprintf(
            "store %.*s %%r%u %%r%u",
            (int)type_string.len,
            type_string.ptr,
            inst.store.ptr_ref.id,
            inst.store.value_ref.id);
        break;
    }
    case SIRInstKind_ReturnVoid: {
        sb->sprintf("return_void");
        break;
    }
    case SIRInstKind_ReturnValue: {
        SIRString type_string =
            SIRTypeToString(module, SIRModuleGetInst(module, inst.op1).type);
        sb->sprintf(
            "return_value %.*s %%r%u",
            (int)type_string.len,
            type_string.ptr,
            inst.op1.id);
        break;
    }
    case SIRInstKind_Jump: {
        sb->sprintf("jump %%b%u", inst.op1.id);
        break;
    }
    case SIRInstKind_Branch: {
        sb->sprintf("branch %%b%u %%b%u", inst.op1.id, inst.op2.id);
        break;
    }
    case SIRInstKind_Phi: {
        SIRString type_string = SIRTypeToString(module, inst.type);
        sb->sprintf(
            "%%r%u = phi %.*s",
            inst_ref.id,
            (int)type_string.len,
            type_string.ptr);
        break;
    }
    case SIRInstKind_PhiIncoming: {
        sb->sprintf(
            "%%r%u = phi_incoming %%b%u %%r%u",
            inst_ref.id,
            inst.op1.id,
            inst.op2.id);
        break;
    }
    }

    if (aux_printer) {
        int64_t end_pos = sb->array.len;
        int64_t inst_len = end_pos - start_pos;

        int64_t padding = SIR_MAX(0, 50 - inst_len);
        for (int64_t i = 0; i < padding; ++i) {
            sb->append(' ');
        }

        sb->append(SIR_STR(" ; "));
        aux_printer(user_data, inst_ref, sb);
    }

    sb->append(SIR_STR("\n"));
}

static void print_block(
    SIRModule *module,
    SIRInstRef block_ref,
    SIRStringBuilder *sb,
    void *user_data,
    SIRAuxInstPrinter *aux_printer)
{
    ZoneScoped;

    SIRInst *block = &module->insts[block_ref.id];

    sb->sprintf("  block %%b%u:\n", block_ref.id);

    for (SIRInstRef inst_ref : block->block->inst_refs) {
        sb->append(SIR_STR("    "));
        print_instruction(module, inst_ref, sb, user_data, aux_printer);
    }

    sb->append(SIR_STR("\n"));
}

static void print_function(
    SIRModule *module,
    SIRInstRef func_ref,
    SIRStringBuilder *sb,
    void *user_data,
    SIRAuxInstPrinter *aux_printer)
{
    ZoneScoped;

    SIRFunction *func = module->insts[func_ref.id].func;

    SIRString calling_conv_str =
        SIRCallingConventionToString(func->calling_convention);
    SIRString linkage_str = SIRLinkageToString(func->linkage);
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
        SIRType *param_type = func->param_types[i];
        SIRInstRef param_inst_ref = func->param_insts[i];

        if (i != 0) sb->append(SIR_STR(", "));

        sb->sprintf("%%r%u: ", param_inst_ref.id);
        sb->append(SIRTypeToString(module, param_type));
    }
    sb->append(SIR_STR(") -> "));
    sb->append(SIRTypeToString(module, func->return_type));
    sb->append(SIR_STR(" {\n"));

    for (SIRInstRef stack_slot_ref : func->stack_slots) {
        SIRInst *stack_slot = &module->insts[stack_slot_ref.id];
        SIR_ASSERT(stack_slot->type);
        SIRString type_string = SIRTypeToString(module, stack_slot->type);
        sb->sprintf(
            "  %%r%u: stack_slot %.*s\n",
            stack_slot_ref.id,
            (int)type_string.len,
            type_string.ptr);
    }

    if (func->stack_slots.len > 0) {
        sb->append(SIR_STR("\n"));
    }

    for (SIRInstRef block_ref : func->blocks) {
        print_block(module, block_ref, sb, user_data, aux_printer);
    }

    sb->append(SIR_STR("}\n\n"));
}

SIRModule *SIRModuleCreate(SIRTargetArch target_arch, SIREndianness endianness)
{
    SIRAllocator *parent_allocator = &SIR_MALLOC_ALLOCATOR;

    SIRArenaAllocator *arena = SIRArenaAllocatorCreate(&SIR_MALLOC_ALLOCATOR);
    auto insts = SIRArray<SIRInst>::create(&SIR_MALLOC_ALLOCATOR);
    auto globals = SIRArray<SIRInstRef>::create(&SIR_MALLOC_ALLOCATOR);
    auto consts = SIRArray<SIRInstRef>::create(&SIR_MALLOC_ALLOCATOR);
    auto functions = SIRArray<SIRInstRef>::create(&SIR_MALLOC_ALLOCATOR);
    SIRStringMap function_map = SIRStringMapCreate(&SIR_MALLOC_ALLOCATOR, 0);
    SIRStringMap global_string_map =
        SIRStringMapCreate(&SIR_MALLOC_ALLOCATOR, 0);
    SIRStringMap type_map = SIRStringMapCreate(&SIR_MALLOC_ALLOCATOR, 0);
    SIRStringMap named_struct_map =
        SIRStringMapCreate(&SIR_MALLOC_ALLOCATOR, 0);

    insts.push_back({}); // 0th inst

    SIRModule *module = SIRAllocInit(parent_allocator, SIRModule);
    *module = {
        .arena = arena,
        .insts = insts,
        .globals = globals,
        .consts = consts,
        .functions = functions,
        .function_map = function_map,
        .global_string_map = global_string_map,
        .type_map = type_map,
        .named_struct_map = named_struct_map,
        .target_arch = target_arch,
        .endianness = endianness,

        .void_type = nullptr,
        .bool_type = nullptr,
        .i8_type = nullptr,
        .i16_type = nullptr,
        .i32_type = nullptr,
        .i64_type = nullptr,
        .u8_type = nullptr,
        .u16_type = nullptr,
        .u32_type = nullptr,
        .u64_type = nullptr,
        .f32_type = nullptr,
        .f64_type = nullptr,
    };

    {
        SIRType *void_type = SIRAllocInit(module->arena, SIRType);
        void_type->kind = SIRTypeKind_Void;
        module->void_type = SIRModuleGetCachedType(module, void_type);
    }

    {
        SIRType *bool_type = SIRAllocInit(module->arena, SIRType);
        bool_type->kind = SIRTypeKind_Bool;
        module->bool_type = SIRModuleGetCachedType(module, bool_type);
    }

    {
        SIRType *i8_type = SIRAllocInit(module->arena, SIRType);
        i8_type->kind = SIRTypeKind_Int;
        i8_type->int_.bits = 8;
        i8_type->int_.is_signed = true;
        module->i8_type = SIRModuleGetCachedType(module, i8_type);
    }

    {
        SIRType *i16_type = SIRAllocInit(module->arena, SIRType);
        i16_type->kind = SIRTypeKind_Int;
        i16_type->int_.bits = 16;
        i16_type->int_.is_signed = true;
        module->i16_type = SIRModuleGetCachedType(module, i16_type);
    }

    {
        SIRType *i32_type = SIRAllocInit(module->arena, SIRType);
        i32_type->kind = SIRTypeKind_Int;
        i32_type->int_.bits = 32;
        i32_type->int_.is_signed = true;
        module->i32_type = SIRModuleGetCachedType(module, i32_type);
    }

    {
        SIRType *i64_type = SIRAllocInit(module->arena, SIRType);
        i64_type->kind = SIRTypeKind_Int;
        i64_type->int_.bits = 64;
        i64_type->int_.is_signed = true;
        module->i64_type = SIRModuleGetCachedType(module, i64_type);
    }

    {
        SIRType *u8_type = SIRAllocInit(module->arena, SIRType);
        u8_type->kind = SIRTypeKind_Int;
        u8_type->int_.bits = 8;
        u8_type->int_.is_signed = false;
        module->u8_type = SIRModuleGetCachedType(module, u8_type);
    }

    {
        SIRType *u16_type = SIRAllocInit(module->arena, SIRType);
        u16_type->kind = SIRTypeKind_Int;
        u16_type->int_.bits = 16;
        u16_type->int_.is_signed = false;
        module->u16_type = SIRModuleGetCachedType(module, u16_type);
    }

    {
        SIRType *u32_type = SIRAllocInit(module->arena, SIRType);
        u32_type->kind = SIRTypeKind_Int;
        u32_type->int_.bits = 32;
        u32_type->int_.is_signed = false;
        module->u32_type = SIRModuleGetCachedType(module, u32_type);
    }

    {
        SIRType *u64_type = SIRAllocInit(module->arena, SIRType);
        u64_type->kind = SIRTypeKind_Int;
        u64_type->int_.bits = 64;
        u64_type->int_.is_signed = false;
        module->u64_type = SIRModuleGetCachedType(module, u64_type);
    }

    {
        SIRType *f32_type = SIRAllocInit(module->arena, SIRType);
        f32_type->kind = SIRTypeKind_Float;
        f32_type->float_.bits = 32;
        module->f32_type = SIRModuleGetCachedType(module, f32_type);
    }

    {
        SIRType *f64_type = SIRAllocInit(module->arena, SIRType);
        f64_type->kind = SIRTypeKind_Float;
        f64_type->float_.bits = 64;
        module->f64_type = SIRModuleGetCachedType(module, f64_type);
    }

    return module;
}

void SIRModuleDestroy(SIRModule *module)
{
    module->insts.destroy();
    module->consts.destroy();
    module->functions.destroy();
    SIRStringMapDestroy(&module->function_map);
    SIRStringMapDestroy(&module->global_string_map);
    SIRStringMapDestroy(&module->type_map);
    SIRStringMapDestroy(&module->named_struct_map);
    SIRArenaAllocatorDestroy(module->arena);

    SIRAllocator *parent_allocator = &SIR_MALLOC_ALLOCATOR;
    SIRFree(parent_allocator, module);
}

char *SIRModulePrintToStringWithAux(
    SIRModule *module,
    size_t *str_len,
    void *user_data,
    SIRAuxInstPrinter *aux_printer)
{
    ZoneScoped;

    SIRStringBuilder sb = SIRStringBuilder::create(&SIR_MALLOC_ALLOCATOR);

    for (SIRInstRef const_ref : module->consts) {
        print_instruction(module, const_ref, &sb, user_data, aux_printer);
    }

    sb.append(SIR_STR("\n"));

    for (SIRInstRef global_ref : module->globals) {
        print_instruction(module, global_ref, &sb, user_data, aux_printer);
    }

    sb.append(SIR_STR("\n"));

    for (SIRInstRef func_ref : module->functions) {
        print_function(module, func_ref, &sb, user_data, aux_printer);
    }

    SIRString result = sb.build_null_terminated(&SIR_MALLOC_ALLOCATOR);
    sb.destroy();
    *str_len = result.len;
    return (char *)result.ptr;
}

char *SIRModulePrintToString(SIRModule *module, size_t *str_len)
{
    ZoneScoped;
    return SIRModulePrintToStringWithAux(module, str_len, NULL, NULL);
}

SIRType *SIRModuleGetVoidType(SIRModule *module)
{
    return module->void_type;
}

SIRType *SIRModuleGetBoolType(SIRModule *module)
{
    return module->bool_type;
}

SIRType *SIRModuleGetI8Type(SIRModule *module)
{
    return module->i8_type;
}

SIRType *SIRModuleGetI16Type(SIRModule *module)
{
    return module->i16_type;
}

SIRType *SIRModuleGetI32Type(SIRModule *module)
{
    return module->i32_type;
}

SIRType *SIRModuleGetI64Type(SIRModule *module)
{
    return module->i64_type;
}

SIRType *SIRModuleGetU8Type(SIRModule *module)
{
    return module->u8_type;
}

SIRType *SIRModuleGetU16Type(SIRModule *module)
{
    return module->u16_type;
}

SIRType *SIRModuleGetU32Type(SIRModule *module)
{
    return module->u32_type;
}

SIRType *SIRModuleGetU64Type(SIRModule *module)
{
    return module->u64_type;
}

SIRType *SIRModuleGetF32Type(SIRModule *module)
{
    return module->f32_type;
}

SIRType *SIRModuleGetF64Type(SIRModule *module)
{
    return module->f64_type;
}

SIRType *SIRModuleCreatePointerType(SIRModule *module, SIRType *sub)
{
    SIRType *type = SIRAllocInit(module->arena, SIRType);
    type->kind = SIRTypeKind_Pointer;
    type->pointer.sub = sub;
    return SIRModuleGetCachedType(module, type);
}

SIRType *
SIRModuleCreateArrayType(SIRModule *module, SIRType *sub, uint64_t count)
{
    SIRType *type = SIRAllocInit(module->arena, SIRType);
    type->kind = SIRTypeKind_Array;
    type->array.sub = sub;
    type->array.count = count;
    return SIRModuleGetCachedType(module, type);
}

SIRType *SIRModuleCreateStructType(
    SIRModule *module, SIRType **fields, size_t field_count, bool packed)
{
    SIRType *type = SIRAllocInit(module->arena, SIRType);
    type->kind = SIRTypeKind_Struct;
    type->struct_.fields_len = field_count;
    type->struct_.fields =
        (SIRType **)SIRAllocSliceClone(module->arena, fields, field_count);
    type->struct_.packed = packed;
    return SIRModuleGetCachedType(module, type);
}

SIRType *SIRModuleCreateNamedStructType(
    SIRModule *module, const char *name, size_t name_len)
{
    SIRType *type = SIRAllocInit(module->arena, SIRType);
    type->kind = SIRTypeKind_Struct;

    SIRString chosen_name;
    chosen_name.ptr =
        (const char *)SIRAllocSliceClone(module->arena, name, name_len);
    chosen_name.len = name_len;

    uint32_t type_index = 0;
    while (SIRStringMapGet(&module->named_struct_map, chosen_name, NULL)) {
        type_index++;
        chosen_name = SIRAllocSprintf(
            module->arena, "%.*s.%u", (int)name_len, name, type_index);
    }

    SIRStringMapSet(&module->named_struct_map, chosen_name, (uintptr_t)type);

    type->str = SIRAllocSprintf(
        module->arena,
        "@named_struct(%.*s)",
        (int)chosen_name.len,
        chosen_name.ptr);
    return SIRModuleGetCachedType(module, type);
}

void SIRStructTypeSetBody(
    SIRModule *module,
    SIRType *struct_type,
    SIRType **fields,
    size_t field_count,
    bool packed)
{
    SIR_ASSERT(struct_type->struct_.fields_len == 0);
    SIR_ASSERT(struct_type->struct_.fields == NULL);
    struct_type->struct_.fields_len = field_count;
    struct_type->struct_.fields =
        (SIRType **)SIRAllocSliceClone(module->arena, fields, field_count);
    struct_type->struct_.packed = packed;
}

SIRType *SIRModuleGetCachedType(SIRModule *module, SIRType *type)
{
    ZoneScoped;

    SIRString type_string = SIRTypeToString(module, type);
    uintptr_t existing_type_addr = 0;
    if (SIRStringMapGet(&module->type_map, type_string, &existing_type_addr)) {
        return (SIRType *)(existing_type_addr);
    }

    SIRStringMapSet(&module->type_map, type_string, (uintptr_t)type);
    return type;
}

SIR_INLINE
static SIRInstRef module_add_inst(SIRModule *module, const SIRInst &inst)
{
    SIRInstRef ref = {(uint32_t)module->insts.len};
    module->insts.push_back(inst);
    return ref;
}

SIRInstRef
SIRModuleAddConstInt(SIRModule *module, SIRType *type, uint64_t value)
{
    ZoneScoped;

    SIR_ASSERT(type->kind == SIRTypeKind_Int);

    SIRInst const_ = {};
    const_.kind = SIRInstKind_ConstInt;
    const_.type = type;
    const_.const_int.u64 = value;

    SIRInstRef const_ref = module_add_inst(module, const_);

    module->consts.push_back(const_ref);

    return const_ref;
}

SIRInstRef
SIRModuleAddConstFloat(SIRModule *module, SIRType *type, double value)
{
    ZoneScoped;

    SIR_ASSERT(type->kind == SIRTypeKind_Float);

    SIRInst const_ = {};
    const_.kind = SIRInstKind_ConstFloat;
    const_.type = type;
    const_.const_float.f64 = value;

    SIRInstRef const_ref = module_add_inst(module, const_);

    module->consts.push_back(const_ref);

    return const_ref;
}

SIRInstRef SIRModuleAddConstBool(SIRModule *module, bool value)
{
    ZoneScoped;

    SIRInst const_ = {};
    const_.kind = SIRInstKind_ConstBool;
    const_.type = SIRModuleGetBoolType(module);
    const_.const_bool.value = value;

    SIRInstRef const_ref = module_add_inst(module, const_);

    module->consts.push_back(const_ref);

    return const_ref;
}

SIRInstRef SIRModuleAddFunction(
    SIRModule *module,
    const char *name,
    size_t name_len,
    SIRCallingConvention calling_convention,
    SIRLinkage linkage,
    bool variadic,
    SIRType **param_types,
    size_t param_types_len,
    SIRType *return_type)
{
    ZoneScoped;

    SIRString func_name = {};
    if (name_len > 0) {
        func_name.ptr =
            (const char *)SIRAllocSliceClone(module->arena, name, name_len);
        func_name.len = name_len;

        uint32_t func_index = 0;
        while (SIRStringMapGet(&module->function_map, func_name, NULL)) {
            func_index++;
            func_name = SIRAllocSprintf(
                module->arena, "%.*s.%u", (int)name_len, name, func_index);
        }
    }

    SIRFunction *function = SIRAlloc(module->arena, SIRFunction);
    *function = SIRFunction{
        .name = func_name,
        .param_types = (SIRType **)SIRAllocSliceClone(
            module->arena, param_types, param_types_len),
        .param_types_len = param_types_len,
        .return_type = return_type,
        .variadic = variadic,

        .stack_slots =
            SIRArray<SIRInstRef>::create((SIRAllocator *)module->arena),
        .blocks = SIRArray<SIRInstRef>::create((SIRAllocator *)module->arena),
        .param_insts =
            SIRArray<SIRInstRef>::create((SIRAllocator *)module->arena),

        .linkage = linkage,
        .calling_convention = calling_convention,
    };

    for (uint32_t i = 0; i < param_types_len; ++i) {
        SIRInst param_inst = {};
        param_inst.kind = SIRInstKind_FunctionParameter;
        param_inst.type = param_types[i];
        param_inst.func_param.index = i;

        SIRInstRef inst_ref = module_add_inst(module, param_inst);
        function->param_insts.push_back(inst_ref);
    }

    SIRInst func_inst = {};
    func_inst.kind = SIRInstKind_Function;
    func_inst.func = function;
    SIRInstRef func_ref = module_add_inst(module, func_inst);

    module->functions.push_back(func_ref);

    if (function->name.len > 0) {
        SIRStringMapSet(
            &module->function_map, function->name, (uintptr_t)func_ref.id);
    }

    return func_ref;
}

SIRInstRef SIRModuleAddGlobal(
    SIRModule *module,
    SIRType *type,
    uint32_t flags,
    const uint8_t *data,
    size_t data_len)
{
    ZoneScoped;

    SIR_ASSERT(SIRTypeSizeOf(module, type) == data_len);

    SIRInst global = {};
    global.kind = SIRInstKind_Global;
    global.type = SIRModuleCreatePointerType(module, type);
    global.global = SIRAllocInit(module->arena, SIRGlobal);
    global.global->data =
        (uint8_t *)SIRAllocSliceClone(module->arena, data, data_len);
    global.global->data_len = data_len;
    global.global->flags = flags;

    SIRInstRef global_ref = module_add_inst(module, global);

    module->globals.push_back(global_ref);

    return global_ref;
}

SIRInstRef
SIRModuleAddGlobalString(SIRModule *module, const char *str, size_t str_len)
{
    ZoneScoped;

    SIRString sir_str;
    sir_str.ptr = str;
    sir_str.len = str_len;

    uintptr_t existing_global_ref_id = 0;
    if (SIRStringMapGet(
            &module->global_string_map, sir_str, &existing_global_ref_id)) {
        return (SIRInstRef){(uint32_t)existing_global_ref_id};
    }

    SIRInst global = {};
    global.kind = SIRInstKind_Global;
    global.type = SIRModuleCreatePointerType(module, module->u8_type);
    global.global = SIRAllocInit(module->arena, SIRGlobal);
    global.global->data =
        (uint8_t *)SIRAllocNullTerminate(module->arena, sir_str);
    global.global->data_len = sir_str.len + 1;
    global.global->flags = SIRGlobalFlags_Initialized | SIRGlobalFlags_ReadOnly;

    SIRInstRef global_ref = module_add_inst(module, global);

    module->globals.push_back(global_ref);

    SIRStringMapSet(&module->global_string_map, sir_str, global_ref.id);

    return global_ref;
}

SIRInstRef
SIRModuleAddStackSlot(SIRModule *module, SIRInstRef func_ref, SIRType *type)
{
    ZoneScoped;

    SIRInst func_inst = SIRModuleGetInst(module, func_ref);
    SIR_ASSERT(func_inst.kind == SIRInstKind_Function);

    SIRFunction *func = func_inst.func;

    SIRInst stack_slot = {};
    stack_slot.kind = SIRInstKind_StackSlot;
    stack_slot.type = SIRModuleCreatePointerType(module, type);
    SIRInstRef stack_slot_ref = module_add_inst(module, stack_slot);

    func->stack_slots.push_back(stack_slot_ref);

    return stack_slot_ref;
}

SIRInstRef SIRModuleGetFuncParam(
    SIRModule *module, SIRInstRef func_ref, uint32_t param_index)
{
    ZoneScoped;

    SIRInst func_inst = SIRModuleGetInst(module, func_ref);
    SIR_ASSERT(func_inst.kind == SIRInstKind_Function);
    SIRFunction *func = func_inst.func;

    return func->param_insts[param_index];
}

SIRInstRef SIRModuleInsertBlockAtEnd(SIRModule *module, SIRInstRef func_ref)
{
    ZoneScoped;

    SIRInst func_inst = SIRModuleGetInst(module, func_ref);
    SIR_ASSERT(func_inst.kind == SIRInstKind_Function);
    SIRFunction *func = func_inst.func;

    SIRInst block = {};
    block.kind = SIRInstKind_Block;
    block.block = SIRAllocInit(module->arena, SIRBlock);
    block.block->inst_refs =
        SIRArray<SIRInstRef>::create((SIRAllocator *)module->arena);
    SIRInstRef block_ref = module_add_inst(module, block);

    func->blocks.push_back(block_ref);

    return block_ref;
}

SIRBuilder *SIRBuilderCreate(SIRModule *module)
{
    SIRBuilder *builder = SIRAlloc(module->arena, SIRBuilder);
    *builder = SIRBuilder{
        .module = module,
        .current_func_ref = {UINT32_MAX},
        .current_block_ref = {UINT32_MAX},
    };
    return builder;
}

void SIRBuilderSetFunction(SIRBuilder *builder, SIRInstRef func_ref)
{
    builder->current_func_ref = func_ref;
}

void SIRBuilderPositionAtEnd(SIRBuilder *builder, SIRInstRef block_ref)
{
    builder->current_block_ref = block_ref;
}

SIRInstRef SIRBuilderGetCurrentFunction(SIRBuilder *builder)
{
    return builder->current_func_ref;
}

SIRInstRef SIRBuilderGetCurrentBlock(SIRBuilder *builder)
{
    return builder->current_block_ref;
}

static SIRInstRef builder_insert_inst(SIRBuilder *builder, const SIRInst &inst)
{
    ZoneScoped;

    SIRInstRef ref = module_add_inst(builder->module, inst);

    SIRInst *block = &builder->module->insts[builder->current_block_ref.id];
    block->block->inst_refs.push_back(ref);

    return ref;
}

SIRInstRef SIRBuilderInsertArrayElemPtr(
    SIRBuilder *builder, SIRInstRef accessed_ref, SIRInstRef index_ref)
{
    ZoneScoped;

    SIRInst accessed_inst = SIRModuleGetInst(builder->module, accessed_ref);
    SIR_ASSERT(accessed_inst.type->kind == SIRTypeKind_Pointer);
    SIR_ASSERT(accessed_inst.type->pointer.sub->kind == SIRTypeKind_Array);

    SIRInst inst = {};
    inst.kind = SIRInstKind_ArrayElemPtr;
    inst.type = SIRModuleCreatePointerType(
        builder->module, accessed_inst.type->pointer.sub->array.sub);
    inst.array_elem_ptr.accessed_ref = accessed_ref;
    inst.array_elem_ptr.index_ref = index_ref;

    return builder_insert_inst(builder, inst);
}

SIRInstRef SIRBuilderInsertStructElemPtr(
    SIRBuilder *builder, SIRInstRef accessed_ref, uint32_t field_index)
{
    ZoneScoped;

    SIRInst accessed_inst = SIRModuleGetInst(builder->module, accessed_ref);
    SIR_ASSERT(accessed_inst.type->kind == SIRTypeKind_Pointer);
    SIR_ASSERT(accessed_inst.type->pointer.sub->kind == SIRTypeKind_Struct);

    SIRInst inst = {};
    inst.kind = SIRInstKind_StructElemPtr;
    inst.type = SIRModuleCreatePointerType(
        builder->module,
        accessed_inst.type->pointer.sub->struct_.fields[field_index]);
    inst.struct_elem_ptr.accessed_ref = accessed_ref;
    inst.struct_elem_ptr.field_index_ref = SIRModuleAddConstInt(
        builder->module, builder->module->u32_type, field_index);

    return builder_insert_inst(builder, inst);
}

SIRInstRef SIRBuilderInsertExtractArrayElem(
    SIRBuilder *builder, SIRInstRef accessed_ref, uint32_t elem_index)
{
    ZoneScoped;

    SIRInst accessed_inst = SIRModuleGetInst(builder->module, accessed_ref);
    SIR_ASSERT(accessed_inst.type->kind == SIRTypeKind_Array);

    SIRInst inst = {};
    inst.kind = SIRInstKind_ExtractArrayElem;
    inst.type = accessed_inst.type->array.sub;
    inst.extract_array_elem.accessed_ref = accessed_ref;
    inst.extract_array_elem.index_ref = SIRModuleAddConstInt(
        builder->module, builder->module->u32_type, elem_index);

    return builder_insert_inst(builder, inst);
}

SIRInstRef SIRBuilderInsertExtractStructElem(
    SIRBuilder *builder, SIRInstRef accessed_ref, uint32_t field_index)
{
    ZoneScoped;

    SIRInst accessed_inst = SIRModuleGetInst(builder->module, accessed_ref);
    SIR_ASSERT(accessed_inst.type->kind == SIRTypeKind_Struct);

    SIRInst inst = {};
    inst.kind = SIRInstKind_ExtractStructElem;
    inst.type = accessed_inst.type->struct_.fields[field_index];
    inst.extract_struct_elem.accessed_ref = accessed_ref;
    inst.extract_struct_elem.field_index_ref = SIRModuleAddConstInt(
        builder->module, builder->module->u32_type, field_index);

    return builder_insert_inst(builder, inst);
}

void SIRBuilderInsertStore(
    SIRBuilder *builder, SIRInstRef ptr_ref, SIRInstRef value_ref)
{
    ZoneScoped;

    SIR_ASSERT(
        SIRModuleGetInst(builder->module, ptr_ref).type->kind ==
        SIRTypeKind_Pointer);
    SIR_ASSERT(
        SIRModuleGetInst(builder->module, ptr_ref).type->pointer.sub ==
        SIRModuleGetInst(builder->module, value_ref).type);

    SIRInst inst = {};
    inst.kind = SIRInstKind_Store;
    inst.store.ptr_ref = ptr_ref;
    inst.store.value_ref = value_ref;

    builder_insert_inst(builder, inst);
}

SIRInstRef SIRBuilderInsertLoad(SIRBuilder *builder, SIRInstRef ptr_ref)
{
    ZoneScoped;

    SIRInst inst = {};
    inst.kind = SIRInstKind_Load;
    inst.type = SIRModuleGetInst(builder->module, ptr_ref).type->pointer.sub;
    inst.op1 = ptr_ref;

    return builder_insert_inst(builder, inst);
}

SIRInstRef SIRBuilderInsertBitCast(
    SIRBuilder *builder, SIRType *dest_type, SIRInstRef inst_ref)
{
    ZoneScoped;

    SIR_ASSERT(
        SIRTypeSizeOf(builder->module, dest_type) ==
        SIRTypeSizeOf(
            builder->module, SIRModuleGetInst(builder->module, inst_ref).type));

    SIRInst inst = {};
    inst.kind = SIRInstKind_BitCast;
    inst.type = dest_type;
    inst.op1 = inst_ref;

    return builder_insert_inst(builder, inst);
}

SIRInstRef SIRBuilderInsertZext(
    SIRBuilder *builder, SIRType *dest_type, SIRInstRef inst_ref)
{
    ZoneScoped;

    SIR_ASSERT(dest_type->kind == SIRTypeKind_Int);

    SIRInst inst = {};
    inst.kind = SIRInstKind_ZExt;
    inst.type = dest_type;
    inst.op1 = inst_ref;

    return builder_insert_inst(builder, inst);
}

SIRInstRef SIRBuilderInsertSext(
    SIRBuilder *builder, SIRType *dest_type, SIRInstRef inst_ref)
{
    ZoneScoped;

    SIR_ASSERT(dest_type->kind == SIRTypeKind_Int);

    SIRInst inst = {};
    inst.kind = SIRInstKind_SExt;
    inst.type = dest_type;
    inst.op1 = inst_ref;

    return builder_insert_inst(builder, inst);
}

SIRInstRef SIRBuilderInsertTrunc(
    SIRBuilder *builder, SIRType *dest_type, SIRInstRef inst_ref)
{
    ZoneScoped;

    SIR_ASSERT(
        dest_type->kind == SIRTypeKind_Int ||
        dest_type->kind == SIRTypeKind_Bool);

    SIRInst inst = {};
    inst.kind = SIRInstKind_Trunc;
    inst.type = dest_type;
    inst.op1 = inst_ref;

    return builder_insert_inst(builder, inst);
}

SIRInstRef SIRBuilderInsertFPTrunc(
    SIRBuilder *builder, SIRType *dest_type, SIRInstRef inst_ref)
{
    ZoneScoped;

    SIR_ASSERT(dest_type->kind == SIRTypeKind_Float);

    SIRInst inst = {};
    inst.kind = SIRInstKind_FPTrunc;
    inst.type = dest_type;
    inst.op1 = inst_ref;

    return builder_insert_inst(builder, inst);
}

SIRInstRef SIRBuilderInsertFPExt(
    SIRBuilder *builder, SIRType *dest_type, SIRInstRef inst_ref)
{
    ZoneScoped;

    SIR_ASSERT(dest_type->kind == SIRTypeKind_Float);

    SIRInst inst = {};
    inst.kind = SIRInstKind_FPExt;
    inst.type = dest_type;
    inst.op1 = inst_ref;

    return builder_insert_inst(builder, inst);
}

SIRInstRef SIRBuilderInsertFPToSI(
    SIRBuilder *builder, SIRType *dest_type, SIRInstRef inst_ref)
{
    ZoneScoped;

    SIR_ASSERT(dest_type->kind == SIRTypeKind_Int && dest_type->int_.is_signed);

    SIRInst inst = {};
    inst.kind = SIRInstKind_FPToSI;
    inst.type = dest_type;
    inst.op1 = inst_ref;

    return builder_insert_inst(builder, inst);
}

SIRInstRef SIRBuilderInsertFPToUI(
    SIRBuilder *builder, SIRType *dest_type, SIRInstRef inst_ref)
{
    ZoneScoped;

    SIR_ASSERT(
        dest_type->kind == SIRTypeKind_Int && !dest_type->int_.is_signed);

    SIRInst inst = {};
    inst.kind = SIRInstKind_FPToUI;
    inst.type = dest_type;
    inst.op1 = inst_ref;

    return builder_insert_inst(builder, inst);
}

SIRInstRef SIRBuilderInsertSIToFP(
    SIRBuilder *builder, SIRType *dest_type, SIRInstRef inst_ref)
{
    ZoneScoped;

    SIR_ASSERT(dest_type->kind == SIRTypeKind_Float);

    SIRInst inst = {};
    inst.kind = SIRInstKind_SIToFP;
    inst.type = dest_type;
    inst.op1 = inst_ref;

    return builder_insert_inst(builder, inst);
}

SIRInstRef SIRBuilderInsertUIToFP(
    SIRBuilder *builder, SIRType *dest_type, SIRInstRef inst_ref)
{
    ZoneScoped;

    SIR_ASSERT(dest_type->kind == SIRTypeKind_Float);

    SIRInst inst = {};
    inst.kind = SIRInstKind_UIToFP;
    inst.type = dest_type;
    inst.op1 = inst_ref;

    return builder_insert_inst(builder, inst);
}

SIRInstRef SIRBuilderInsertFNeg(SIRBuilder *builder, SIRInstRef inst_ref)
{
    ZoneScoped;

    SIRType *type = SIRModuleGetInst(builder->module, inst_ref).type;
    SIR_ASSERT(type->kind == SIRTypeKind_Float);

    SIRInst inst = {};
    inst.kind = SIRInstKind_FNeg;
    inst.type = type;
    inst.op1 = inst_ref;

    return builder_insert_inst(builder, inst);
}

SIRInstRef SIRBuilderInsertBinop(
    SIRBuilder *builder,
    SIRBinaryOperation op,
    SIRInstRef left_ref,
    SIRInstRef right_ref)
{
    ZoneScoped;

    SIRInst inst = {};
    inst.kind = SIRInstKind_Binop;
    switch (op) {
    case SIRBinaryOperation_Unknown:
    case SIRBinaryOperation_MAX: SIR_ASSERT(0); break;

    case SIRBinaryOperation_IAdd:
    case SIRBinaryOperation_ISub:
    case SIRBinaryOperation_IMul:
    case SIRBinaryOperation_SDiv:
    case SIRBinaryOperation_UDiv:
    case SIRBinaryOperation_SRem:
    case SIRBinaryOperation_URem:

    case SIRBinaryOperation_FAdd:
    case SIRBinaryOperation_FSub:
    case SIRBinaryOperation_FMul:
    case SIRBinaryOperation_FDiv: {
        inst.type = SIRModuleGetInst(builder->module, left_ref).type;
        break;
    }

    case SIRBinaryOperation_IEQ:
    case SIRBinaryOperation_INE:
    case SIRBinaryOperation_UGT:
    case SIRBinaryOperation_UGE:
    case SIRBinaryOperation_ULT:
    case SIRBinaryOperation_ULE:
    case SIRBinaryOperation_SGT:
    case SIRBinaryOperation_SGE:
    case SIRBinaryOperation_SLT:
    case SIRBinaryOperation_SLE:

    case SIRBinaryOperation_FEQ:
    case SIRBinaryOperation_FNE:
    case SIRBinaryOperation_FGT:
    case SIRBinaryOperation_FGE:
    case SIRBinaryOperation_FLT:
    case SIRBinaryOperation_FLE: {
        inst.type = builder->module->bool_type;
        break;
    }

    case SIRBinaryOperation_Shl:
    case SIRBinaryOperation_AShr:
    case SIRBinaryOperation_LShr: {
        inst.type = SIRModuleGetInst(builder->module, left_ref).type;
        break;
    }

    case SIRBinaryOperation_And:
    case SIRBinaryOperation_Or:
    case SIRBinaryOperation_Xor: {
        inst.type = SIRModuleGetInst(builder->module, left_ref).type;
        break;
    }
    }

    SIR_ASSERT(inst.type);

    inst.binop = op;
    inst.op1 = left_ref;
    inst.op2 = right_ref;

    return builder_insert_inst(builder, inst);
}

SIRInstRef SIRBuilderInsertFuncCall(
    SIRBuilder *builder,
    SIRInstRef func_ref,
    const SIRInstRef *params,
    size_t params_len)
{
    ZoneScoped;

    SIRInst func_inst = SIRModuleGetInst(builder->module, func_ref);
    SIR_ASSERT(func_inst.kind == SIRInstKind_Function);
    SIRFunction *called_function = func_inst.func;

    for (size_t i = 0; i < params_len; ++i) {
        SIRInst inst = {};
        inst.kind = SIRInstKind_PushFunctionParameter;
        inst.op1 = params[i];
        builder_insert_inst(builder, inst);
    }

    SIRInst inst = {};
    inst.kind = SIRInstKind_FuncCall;
    inst.type = called_function->return_type;
    inst.op1 = func_ref;

    return builder_insert_inst(builder, inst);
}

void SIRBuilderInsertJump(SIRBuilder *builder, SIRInstRef block_ref)
{
    ZoneScoped;

    SIRInst inst = {};
    inst.kind = SIRInstKind_Jump;
    inst.op1 = block_ref;

    builder_insert_inst(builder, inst);
}

void SIRBuilderInsertBranch(
    SIRBuilder *builder,
    SIRInstRef cond_ref,
    SIRInstRef true_block_ref,
    SIRInstRef false_block_ref)
{
    ZoneScoped;

    SIR_ASSERT(
        SIRModuleGetInst(builder->module, cond_ref).type->kind ==
        SIRTypeKind_Bool);
    SIR_ASSERT(
        SIRModuleGetInst(builder->module, true_block_ref).kind ==
        SIRInstKind_Block);
    SIR_ASSERT(
        SIRModuleGetInst(builder->module, false_block_ref).kind ==
        SIRInstKind_Block);

    {
        SIRInst inst = {};
        inst.kind = SIRInstKind_SetCond;
        inst.op1 = cond_ref;
        builder_insert_inst(builder, inst);
    }

    SIRInst inst = {};
    inst.kind = SIRInstKind_Branch;
    inst.op1 = true_block_ref;
    inst.op2 = false_block_ref;

    builder_insert_inst(builder, inst);
}

SIRInstRef SIRBuilderInsertPhi(SIRBuilder *builder, SIRType *type)
{
    ZoneScoped;

    SIRInst inst = {};
    inst.kind = SIRInstKind_Phi;
    inst.type = type;

    return builder_insert_inst(builder, inst);
}

void SIRPhiAddIncoming(
    SIRBuilder *builder, SIRInstRef block_ref, SIRInstRef value_ref)
{
    ZoneScoped;

    SIRInst inst = {};
    inst.kind = SIRInstKind_PhiIncoming;
    inst.op1 = block_ref;
    inst.op2 = value_ref;

    builder_insert_inst(builder, inst);
}

void SIRBuilderInsertReturnValue(SIRBuilder *builder, SIRInstRef inst_ref)
{
    ZoneScoped;

    SIRInst inst = {};
    inst.kind = SIRInstKind_ReturnValue;
    inst.op1 = inst_ref;

    builder_insert_inst(builder, inst);
}

void SIRBuilderInsertReturnVoid(SIRBuilder *builder)
{
    ZoneScoped;

    SIRInst inst = {};
    inst.kind = SIRInstKind_ReturnVoid;

    builder_insert_inst(builder, inst);
}

SIRInstKind SIRModuleGetInstKind(SIRModule *module, SIRInstRef inst_ref)
{
    return module->insts[inst_ref.id].kind;
}

SIRType *SIRModuleGetInstType(SIRModule *module, SIRInstRef inst_ref)
{
    return module->insts[inst_ref.id].type;
}

SIRTypeKind SIRModuleGetTypeKind(SIRModule *module, SIRType *type)
{
    (void)module;
    return type->kind;
}

uint32_t
SIRModuleGetBlockInstructionCount(SIRModule *module, SIRInstRef block_ref)
{
    return (uint32_t)module->insts[block_ref.id].block->inst_refs.len;
}

SIRInstRef SIRModuleGetBlockInstruction(
    SIRModule *module, SIRInstRef block_ref, uint32_t inst_index)
{
    if (module->insts[block_ref.id].block->inst_refs.len <= inst_index) {
        return (SIRInstRef){0};
    }
    return module->insts[block_ref.id].block->inst_refs[inst_index];
}

SIRString SIRTypeToString(SIRModule *module, SIRType *type)
{
    if (type->str.len > 0) {
        return type->str;
    }

    switch (type->kind) {
    case SIRTypeKind_Void: {
        type->str = SIR_STR("@void");
        break;
    }
    case SIRTypeKind_Bool: {
        type->str = SIR_STR("@bool");
        break;
    }
    case SIRTypeKind_Pointer: {
        SIRString sub_string = SIRTypeToString(module, type->pointer.sub);
        type->str = SIRAllocSprintf(
            module->arena, "@ptr(%.*s)", (int)sub_string.len, sub_string.ptr);
        break;
    }
    case SIRTypeKind_Int: {
        if (type->int_.is_signed) {
            type->str =
                SIRAllocSprintf(module->arena, "@int(%u)", type->int_.bits);
        } else {
            type->str =
                SIRAllocSprintf(module->arena, "@uint(%u)", type->int_.bits);
        }
        break;
    }
    case SIRTypeKind_Float: {
        type->str =
            SIRAllocSprintf(module->arena, "@float(%u)", type->float_.bits);
        break;
    }
    case SIRTypeKind_Array: {
        SIRString sub_string = SIRTypeToString(module, type->array.sub);
        type->str = SIRAllocSprintf(
            module->arena,
            "@array(%lu, %.*s)",
            type->array.count,
            (int)sub_string.len,
            sub_string.ptr);
        break;
    }
    case SIRTypeKind_Struct: {
        SIRStringBuilder sb = SIRStringBuilder::create(&SIR_MALLOC_ALLOCATOR);

        if (type->struct_.packed) {
            sb.append(SIR_STR("@packed_struct("));
        } else {
            sb.append(SIR_STR("@struct("));
        }

        for (size_t i = 0; i < type->struct_.fields_len; ++i) {
            SIRType *field_type = type->struct_.fields[i];
            if (i != 0) {
                sb.append(SIR_STR(", "));
            }
            SIRString field_str = SIRTypeToString(module, field_type);
            sb.append(field_str);
        }

        sb.append(SIR_STR(")"));

        type->str = sb.build_null_terminated((SIRAllocator *)module->arena);

        sb.destroy();
        break;
    }
    }

    return type->str;
}

uint32_t SIRTypeSizeOf(SIRModule *module, SIRType *type)
{
    if (type->size > 0) return type->size;

    switch (type->kind) {
    case SIRTypeKind_Void: type->size = 0; break;
    case SIRTypeKind_Bool: type->size = 1; break;
    case SIRTypeKind_Int: type->size = type->int_.bits / 8; break;
    case SIRTypeKind_Float: type->size = type->float_.bits / 8; break;
    case SIRTypeKind_Pointer: {
        switch (module->target_arch) {
        case SIRTargetArch_X86_64: type->size = 8; break;
        }
        break;
    }
    case SIRTypeKind_Array: {
        uint32_t elem_size = SIRTypeSizeOf(module, type->array.sub);
        uint32_t elem_alignment = SIRTypeAlignOf(module, type->array.sub);
        uint32_t stride = SIR_ROUND_UP(elem_alignment, elem_size);
        type->size = stride * type->array.count;
        break;
    }
    case SIRTypeKind_Struct: {
        type->size = 0;

        for (size_t i = 0; i < type->struct_.fields_len; ++i) {
            SIRType *field_type = type->struct_.fields[i];
            uint32_t field_align = SIRTypeAlignOf(module, field_type);
            type->size = SIR_ROUND_UP(field_align, type->size); // Add padding

            uint32_t field_size = SIRTypeSizeOf(module, field_type);
            type->size += field_size;
        }

        break;
    }
    }

    uint32_t self_alignment = SIRTypeAlignOf(module, type);
    type->size =
        SIR_ROUND_UP(self_alignment, type->size); // Round size up for alignment

    return type->size;
}

uint32_t SIRTypeAlignOf(SIRModule *module, SIRType *type)
{
    if (type->alignment > 0) return type->alignment;

    switch (type->kind) {
    case SIRTypeKind_Void: type->alignment = 1; break;
    case SIRTypeKind_Bool: type->alignment = 1; break;
    case SIRTypeKind_Int: type->alignment = type->int_.bits / 8; break;
    case SIRTypeKind_Float: type->alignment = type->float_.bits / 8; break;
    case SIRTypeKind_Pointer: {
        switch (module->target_arch) {
        case SIRTargetArch_X86_64: type->alignment = 8; break;
        }
        break;
    }
    case SIRTypeKind_Array: {
        type->alignment = SIRTypeAlignOf(module, type->array.sub);
        break;
    }
    case SIRTypeKind_Struct: {
        type->alignment = 0;

        for (size_t i = 0; i < type->struct_.fields_len; ++i) {
            SIRType *field_type = type->struct_.fields[i];
            uint32_t field_align = SIRTypeAlignOf(module, field_type);
            if (field_align > type->alignment) {
                type->alignment = field_align;
            }
        }

        break;
    }
    }

    return type->alignment;
}

uint32_t SIRTypeStructOffsetOf(
    SIRModule *module, SIRType *struct_type, uint32_t field_index)
{
    uint32_t field_offset = 0;
    for (uint32_t i = 0; i <= field_index; ++i) {
        SIRType *field_type = struct_type->struct_.fields[i];
        uint32_t field_align = SIRTypeAlignOf(module, field_type);
        field_offset = SIR_ROUND_UP(field_align, field_offset); // Add padding

        if (i != field_index) {
            field_offset += SIRTypeSizeOf(module, field_type);
        }
    }

    return field_offset;
}
