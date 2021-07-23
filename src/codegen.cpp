#include "compiler.hpp"
#include <Tracy.hpp>

#include <ace_ir.hpp>
#include <ace_obj.hpp>

struct CodegenContext;

struct CodegenValue {
    bool is_lvalue;
    ace::InstRef inst_ref;
};

struct CodegenContext {
    FileRef file_ref;
    ace::Module *module;
    ace::Builder builder;
    ace::Array<ace::Type *> type_values;
    ace::Array<CodegenValue> expr_values;
    ace::Array<CodegenValue> decl_values;

    ace::Array<ace::InstRef> function_stack;
};

static void
codegen_decl(Compiler *compiler, CodegenContext *ctx, DeclRef decl_ref);

static ace::InstRef load_lvalue(CodegenContext *ctx, const CodegenValue &value)
{
    if (value.is_lvalue) {
        return ctx->builder.insert_load(value.inst_ref);
    }
    return value.inst_ref;
}

static ace::Type *
get_ir_type(Compiler *compiler, ace::Module *module, const Type &type)
{
    (void)compiler;

    switch (type.kind) {
    case TypeKind_Unknown:
    case TypeKind_Function:
    case TypeKind_UntypedInt:
    case TypeKind_UntypedFloat:
    case TypeKind_Type: return nullptr;

    case TypeKind_Void: {
        return module->void_type;
    }
    case TypeKind_Bool: {
        return module->i8_type;
    }
    case TypeKind_Int: {
        switch (type.int_.bits) {
        case 8: return module->i8_type;
        case 16: return module->i16_type;
        case 32: return module->i32_type;
        case 64: return module->i64_type;
        }
        break;
    }
    case TypeKind_Float: {
        switch (type.float_.bits) {
        case 32: return module->f32_type;
        case 64: return module->f64_type;
        }
        break;
    }
    case TypeKind_Pointer: {
        ace::Type *subtype =
            get_ir_type(compiler, module, type.pointer.sub_type.get(compiler));
        return module->create_pointer_type(subtype);
    }
    case TypeKind_Slice: {
        /* ACE_ASSERT(!"unimplemented"); */
        break;
    }
    case TypeKind_Array: {
        ace::Type *subtype =
            get_ir_type(compiler, module, type.array.sub_type.get(compiler));
        return module->create_array_type(subtype, type.array.size);
    }
    case TypeKind_Tuple: {
        /* ACE_ASSERT(!"unimplemented"); */
        break;
    }
    case TypeKind_Struct: {
        /* ACE_ASSERT(!"unimplemented"); */
        break;
    }
    }

    return nullptr;
}

static CodegenValue
codegen_expr(Compiler *compiler, CodegenContext *ctx, ExprRef expr_ref)
{
    ZoneScoped;

    Expr expr = expr_ref.get(compiler);
    CodegenValue value = {};

    switch (expr.kind) {
    case ExprKind_Unknown: ACE_ASSERT(0); break;

    case ExprKind_VoidLiteral:
    case ExprKind_VoidType:
    case ExprKind_PointerType:
    case ExprKind_BoolType:
    case ExprKind_FloatType:
    case ExprKind_IntType:
    case ExprKind_SliceType:
    case ExprKind_ArrayType: break;

    case ExprKind_BoolLiteral: {
        value = {false, ctx->builder.insert_imm_bool(expr.bool_literal.bool_)};
        break;
    }

    case ExprKind_IntLiteral: {
        Type type = expr.expr_type_ref.get(compiler);

        switch (type.kind) {
        case TypeKind_Int: {
            value = {
                false,
                ctx->builder.insert_imm_int(
                    ctx->type_values[expr.expr_type_ref.id],
                    expr.int_literal.i64)};
            break;
        }
        case TypeKind_Float: {
            value = {
                false,
                ctx->builder.insert_imm_float(
                    ctx->type_values[expr.expr_type_ref.id],
                    (double)expr.int_literal.i64)};
            break;
        }
        default: ACE_ASSERT(0);
        }

        break;
    }

    case ExprKind_FloatLiteral: {
        ACE_ASSERT(expr.expr_type_ref.get(compiler).kind == TypeKind_Float);

        value = {
            false,
            ctx->builder.insert_imm_float(
                ctx->type_values[expr.expr_type_ref.id],
                (double)expr.float_literal.f64)};

        break;
    }

    case ExprKind_NullLiteral: {
        ACE_ASSERT(!"unimplemented");
        break;
    }

    case ExprKind_StringLiteral: {
        Type type = expr.expr_type_ref.get(compiler);

        switch (type.kind) {
        case TypeKind_Pointer: {
            ACE_ASSERT(type.pointer.sub_type.id == compiler->u8_type.id);

            value = {
                false, ctx->module->add_global_string(expr.str_literal.str)};

            break;
        }

        case TypeKind_Slice: {
            ACE_ASSERT(!"unimplemented");
            break;
        }

        default: ACE_ASSERT(0); break;
        }

        break;
    }

    case ExprKind_Identifier: {
        ACE_ASSERT(expr.ident.decl_ref.id > 0);

        Decl decl = expr.ident.decl_ref.get(compiler);
        switch (decl.kind) {
        case DeclKind_Function:
        case DeclKind_FunctionParameter: {
            value = ctx->decl_values[expr.ident.decl_ref.id];
            break;
        }

        case DeclKind_GlobalVarDecl: {
            value = ctx->decl_values[expr.ident.decl_ref.id];
            break;
        }

        case DeclKind_LocalVarDecl: {
            value = ctx->decl_values[expr.ident.decl_ref.id];
            break;
        }

        default: ACE_ASSERT(0); break;
        }

        break;
    }

    case ExprKind_Function: {
        ACE_ASSERT(!"unimplemented");
        break;
    }

    case ExprKind_FunctionCall: {
        Expr func_expr = expr.func_call.func_expr_ref.get(compiler);
        Type func_type = func_expr.expr_type_ref.get(compiler);

        switch (func_type.kind) {
        case TypeKind_Function: {
            ace::Slice<ace::InstRef> params =
                compiler->arena->alloc<ace::InstRef>(
                    expr.func_call.param_refs.len);

            for (size_t i = 0; i < expr.func_call.param_refs.len; ++i) {
                CodegenValue param_value =
                    codegen_expr(compiler, ctx, expr.func_call.param_refs[i]);

                params[i] = load_lvalue(ctx, param_value);
            }

            CodegenValue func_value =
                codegen_expr(compiler, ctx, expr.func_call.func_expr_ref);

            ace::Inst func_inst = func_value.inst_ref.get(ctx->module);
            switch (func_inst.kind) {
            case ace::InstKind_Function: {
                value = {
                    false,
                    ctx->builder.insert_func_call(
                        load_lvalue(ctx, func_value), params)};
                break;
            }

            default: ACE_ASSERT(0); break;
            }

            break;
        }

        case TypeKind_Type: {
            ACE_ASSERT(expr.func_call.param_refs.len == 1);
            value = codegen_expr(compiler, ctx, expr.func_call.param_refs[0]);
            break;
        }

        default: ACE_ASSERT(0); break;
        }

        break;
    }

    case ExprKind_BuiltinCall: {
        switch (expr.builtin_call.builtin) {
        case BuiltinFunction_Unknown: ACE_ASSERT(0); break;
        case BuiltinFunction_Sizeof: {
            Expr param0 = expr.builtin_call.param_refs[0].get(compiler);
            uint64_t size = param0.as_type_ref.get(compiler).size_of(compiler);

            value = {
                false,
                ctx->builder.insert_imm_int(
                    ctx->type_values[expr.expr_type_ref.id], size)};

            break;
        }
        case BuiltinFunction_Alignof: {
            Expr param0 = expr.builtin_call.param_refs[0].get(compiler);
            uint64_t size = param0.as_type_ref.get(compiler).align_of(compiler);

            value = {
                false,
                ctx->builder.insert_imm_int(
                    ctx->type_values[expr.expr_type_ref.id], size)};

            break;
        }
        case BuiltinFunction_PtrCast: {
            Expr param0 = expr.builtin_call.param_refs[0].get(compiler);

            CodegenValue ptr_value =
                codegen_expr(compiler, ctx, expr.builtin_call.param_refs[1]);

            value = {
                false,
                ctx->builder.insert_ptr_cast(
                    ctx->type_values[param0.as_type_ref.id],
                    load_lvalue(ctx, ptr_value))};

            break;
        }
        }

        break;
    }

    case ExprKind_Subscript: {
        CodegenValue indexed_ref =
            codegen_expr(compiler, ctx, expr.subscript.left_ref);
        CodegenValue index_ref =
            codegen_expr(compiler, ctx, expr.subscript.right_ref);

        ACE_ASSERT(indexed_ref.is_lvalue);

        value = {
            true,
            ctx->builder.insert_array_elem_ptr(
                indexed_ref.inst_ref, load_lvalue(ctx, index_ref))};

        break;
    }

    case ExprKind_Unary: {

        switch (expr.unary.op) {
        case UnaryOp_Unknown: ACE_ASSERT(0); break;
        case UnaryOp_AddressOf: {
            CodegenValue operand_value =
                codegen_expr(compiler, ctx, expr.unary.left_ref);

            if (!operand_value.is_lvalue) {
                ace::InstRef func_ref = *ctx->function_stack.last();

                Expr left_expr = expr.unary.left_ref.get(compiler);
                ace::Type *ir_type =
                    ctx->type_values[left_expr.expr_type_ref.id];

                value = {false, ctx->module->add_stack_slot(func_ref, ir_type)};
            } else {
                value = {false, operand_value.inst_ref};
            }

            break;
        }
        case UnaryOp_Dereference: {
            CodegenValue operand_value =
                codegen_expr(compiler, ctx, expr.unary.left_ref);
            ACE_ASSERT(operand_value.is_lvalue);

            value = {true, ctx->builder.insert_load(operand_value.inst_ref)};

            break;
        }
        case UnaryOp_Negate: {
            ACE_ASSERT(!"unimplemented");
            break;
        }
        case UnaryOp_Not: {
            ACE_ASSERT(!"unimplemented");
            break;
        }
        }

        break;
    }

    case ExprKind_Binary: {
        CodegenValue left_val =
            codegen_expr(compiler, ctx, expr.binary.left_ref);
        CodegenValue right_val =
            codegen_expr(compiler, ctx, expr.binary.right_ref);

        ace::InstRef left_inst = load_lvalue(ctx, left_val);
        ace::InstRef right_inst = load_lvalue(ctx, right_val);

        ace::BinaryOperation op = {};

        Type type = expr.expr_type_ref.get(compiler);

        switch (expr.binary.op) {
        case BinaryOp_Unknown:
        case BinaryOp_MAX: ACE_ASSERT(0); break;

        case BinaryOp_Add: {
            switch (type.kind) {
            case TypeKind_Int: op = ace::BinaryOperation_IAdd; break;
            case TypeKind_Float: ACE_ASSERT(!"unimplemented"); break;
            default: ACE_ASSERT(0);
            }

            break;
        }

        case BinaryOp_Sub: {
            switch (type.kind) {
            case TypeKind_Int: op = ace::BinaryOperation_ISub; break;
            case TypeKind_Float: ACE_ASSERT(!"unimplemented"); break;
            default: ACE_ASSERT(0);
            }

            break;
        }

        case BinaryOp_Mul: {
            switch (type.kind) {
            case TypeKind_Int: op = ace::BinaryOperation_IMul; break;
            case TypeKind_Float: ACE_ASSERT(!"unimplemented"); break;
            default: ACE_ASSERT(0);
            }

            break;
        }

        case BinaryOp_Div:
        case BinaryOp_Mod: {
            ACE_ASSERT(!"unimplemented binop codegen");
            break;
        }

        case BinaryOp_BitAnd:
        case BinaryOp_BitOr:
        case BinaryOp_BitXor: {
            ACE_ASSERT(!"unimplemented binop codegen");
            break;
        }

        case BinaryOp_Equal:
        case BinaryOp_NotEqual:
        case BinaryOp_Greater:
        case BinaryOp_GreaterEqual:
        case BinaryOp_Less:
        case BinaryOp_LessEqual: {
            ACE_ASSERT(!"unimplemented binop codegen");
            break;
        }

        case BinaryOp_LShift:
        case BinaryOp_RShift: {
            ACE_ASSERT(!"unimplemented binop codegen");
            break;
        }
        }

        value = {false, ctx->builder.insert_binop(op, left_inst, right_inst)};

        break;
    }
    }

    ctx->expr_values[expr_ref.id] = value;
    return value;
}

static void
codegen_stmt(Compiler *compiler, CodegenContext *ctx, StmtRef stmt_ref)
{
    ZoneScoped;

    Stmt stmt = stmt_ref.get(compiler);
    switch (stmt.kind) {
    case StmtKind_Unknown: ACE_ASSERT(0); break;

    case StmtKind_Expr: {
        codegen_expr(compiler, ctx, stmt.expr.expr_ref);
        break;
    }

    case StmtKind_Decl: {
        codegen_decl(compiler, ctx, stmt.decl.decl_ref);
        break;
    }

    case StmtKind_Assign: {
        CodegenValue receiver_value =
            codegen_expr(compiler, ctx, stmt.assign.assigned_expr_ref);
        CodegenValue value =
            codegen_expr(compiler, ctx, stmt.assign.value_expr_ref);

        ACE_ASSERT(receiver_value.is_lvalue);

        ctx->builder.insert_store(
            receiver_value.inst_ref, load_lvalue(ctx, value));
        break;
    }

    case StmtKind_Block: {
        for (StmtRef sub_stmt_ref : stmt.block.stmt_refs) {
            codegen_stmt(compiler, ctx, sub_stmt_ref);
        }
        break;
    }

    case StmtKind_Return: {
        ACE_ASSERT(!"unimplemented");
        break;
    }

    case StmtKind_If: {
        ace::InstRef cond_value = load_lvalue(
            ctx, codegen_expr(compiler, ctx, stmt.if_.cond_expr_ref));

        auto current_func = ctx->builder.current_func_ref;

        if (stmt.if_.false_stmt_ref.id == 0) {
            auto true_block = ctx->module->insert_block_at_end(current_func);
            auto merge_block = ctx->module->insert_block_at_end(current_func);

            ctx->builder.insert_branch(cond_value, true_block, merge_block);

            ctx->builder.position_at_end(true_block);
            codegen_stmt(compiler, ctx, stmt.if_.true_stmt_ref);
            ctx->builder.insert_jump(merge_block);

            ctx->builder.position_at_end(merge_block);
        } else {
            auto true_block = ctx->module->insert_block_at_end(current_func);
            auto false_block = ctx->module->insert_block_at_end(current_func);
            auto merge_block = ctx->module->insert_block_at_end(current_func);

            ctx->builder.insert_branch(cond_value, true_block, false_block);

            ctx->builder.position_at_end(true_block);
            codegen_stmt(compiler, ctx, stmt.if_.true_stmt_ref);
            ctx->builder.insert_jump(merge_block);

            ctx->builder.position_at_end(false_block);
            codegen_stmt(compiler, ctx, stmt.if_.false_stmt_ref);
            ctx->builder.insert_jump(merge_block);

            ctx->builder.position_at_end(merge_block);
        }

        break;
    }

    case StmtKind_While: {
        ace::InstRef cond_value = load_lvalue(
            ctx, codegen_expr(compiler, ctx, stmt.while_.cond_expr_ref));

        auto current_func = ctx->builder.current_func_ref;

        auto cond_block = ctx->module->insert_block_at_end(current_func);
        auto true_block = ctx->module->insert_block_at_end(current_func);
        auto merge_block = ctx->module->insert_block_at_end(current_func);

        ctx->builder.insert_jump(cond_block);

        ctx->builder.position_at_end(cond_block);
        ctx->builder.insert_branch(cond_value, true_block, merge_block);

        ctx->builder.position_at_end(true_block);
        codegen_stmt(compiler, ctx, stmt.while_.true_stmt_ref);
        ctx->builder.insert_jump(cond_block);

        ctx->builder.position_at_end(merge_block);

        break;
    }
    }
}

static void
codegen_decl(Compiler *compiler, CodegenContext *ctx, DeclRef decl_ref)
{
    ZoneScoped;

    ace::Module *module = ctx->module;
    Decl decl = decl_ref.get(compiler);
    CodegenValue value = {};

    switch (decl.kind) {
    case DeclKind_Unknown: ACE_ASSERT(0); break;

    case DeclKind_ConstDecl: {
        ACE_ASSERT(!"unimplemented");
        break;
    }

    case DeclKind_Function: {
        TypeRef func_type_ref = decl.decl_type_ref;
        Type func_type = func_type_ref.get(compiler);

        ace::Slice<ace::Type *> param_types =
            compiler->arena->alloc<ace::Type *>(func_type.func.param_types.len);
        for (size_t i = 0; i < func_type.func.param_types.len; ++i) {
            param_types[i] = ctx->type_values[func_type.func.param_types[i].id];
        }

        ace::Type *return_type =
            ctx->type_values[func_type.func.return_type.id];

        ace::Linkage linkage = ace::Linkage_Internal;
        if ((decl.func.flags & FunctionFlags_Exported) ||
            (decl.func.flags & FunctionFlags_Extern)) {
            linkage = ace::Linkage_External;
        }

        value = {
            false,
            module->add_function(
                decl.name,
                ace::CallingConvention_SystemV,
                linkage,
                false,
                param_types,
                return_type)};

        ctx->decl_values[decl_ref.id] = value;

        ctx->function_stack.push_back(value.inst_ref);

        for (size_t i = 0; i < decl.func.param_decl_refs.len; ++i) {
            DeclRef param_decl_ref = decl.func.param_decl_refs[i];

            ace::InstRef param_value = {};
            param_value = module->get_func_param(value.inst_ref, i);
            ctx->decl_values[param_decl_ref.id] = {false, param_value};
        }

        if (!(decl.func.flags & FunctionFlags_Extern)) {
            ctx->builder.set_function(value.inst_ref);

            auto block = module->insert_block_at_end(value.inst_ref);
            ctx->builder.position_at_end(block);

            for (StmtRef stmt_ref : decl.func.body_stmts) {
                codegen_stmt(compiler, ctx, stmt_ref);
            }

            ctx->builder.insert_return_void();
        }

        ctx->function_stack.pop();

        break;
    }

    case DeclKind_FunctionParameter: {
        // No need to implement this
        ACE_ASSERT(0);
        break;
    }

    case DeclKind_LocalVarDecl: {
        ace::InstRef func_ref = *ctx->function_stack.last();
        ace::Type *ir_type = ctx->type_values[decl.decl_type_ref.id];
        ACE_ASSERT(ir_type);

        value = {true, module->add_stack_slot(func_ref, ir_type)};

        ctx->decl_values[decl_ref.id] = value;

        if (decl.local_var_decl.value_expr.id) {
            CodegenValue assigned_value =
                codegen_expr(compiler, ctx, decl.local_var_decl.value_expr);

            ctx->builder.insert_store(
                value.inst_ref, load_lvalue(ctx, assigned_value));
        }

        break;
    }

    case DeclKind_GlobalVarDecl: {
        ace::Type *ir_type = ctx->type_values[decl.decl_type_ref.id];

        ace::Slice<uint8_t> global_data =
            compiler->arena->alloc<uint8_t>(ir_type->size_of(ctx->module));

        value = {
            true,
            module->add_global(
                ir_type, ace::GlobalFlags_Initialized, global_data)};

        ctx->decl_values[decl_ref.id] = value;

        if (decl.local_var_decl.value_expr.id) {
            ACE_ASSERT(!"unimplemented");
        }

        break;
    }
    }
}

void codegen_file(Compiler *compiler, FileRef file_ref)
{
    ZoneScoped;

    CodegenContext ctx = {};

    File file = compiler->files[file_ref.id];

    ctx.file_ref = file_ref;
    ctx.module = ace::Module::create(
        ace::TargetArch_X86_64, ace::Endianness_LittleEndian);
    ctx.builder = ace::Builder::create(ctx.module);

    ctx.type_values =
        ace::Array<ace::Type *>::create(ace::MallocAllocator::get_instance());
    ctx.type_values.resize(compiler->types.len);
    for (size_t i = 0; i < compiler->types.len; ++i) {
        ctx.type_values[i] =
            get_ir_type(compiler, ctx.module, compiler->types[i]);
    }

    ctx.decl_values =
        ace::Array<CodegenValue>::create(ace::MallocAllocator::get_instance());
    ctx.decl_values.resize(compiler->decls.len);
    for (size_t i = 0; i < compiler->decls.len; ++i) {
        ctx.decl_values[i] = {};
    }

    ctx.expr_values =
        ace::Array<CodegenValue>::create(ace::MallocAllocator::get_instance());
    ctx.expr_values.resize(compiler->exprs.len);
    for (size_t i = 0; i < compiler->exprs.len; ++i) {
        ctx.expr_values[i] = {};
    }

    ctx.function_stack = ace::Array<ace::InstRef>::create(ctx.module->arena);

    for (DeclRef decl_ref : file.top_level_decls) {
        codegen_decl(compiler, &ctx, decl_ref);
    }

#if !NDEBUG
    {
        auto allocator = ace::MallocAllocator::get_instance();
        ace::String str = ctx.module->print_alloc(allocator);
        printf("%.*s", (int)str.len, str.ptr);
        allocator->free(str);
    }
#endif

    ace::ObjectBuilder *obj_builder = ace::create_elf64_builder(ctx.module);
    ace::AsmBuilder *asm_builder =
        ace::create_x86_64_builder(ctx.module, obj_builder);

    asm_builder->generate();

    obj_builder->output_to_file("./main.o");

    asm_builder->destroy();
    obj_builder->destroy();

    ctx.type_values.destroy();
    ctx.decl_values.destroy();
    ctx.expr_values.destroy();

    ctx.module->destroy();
}
