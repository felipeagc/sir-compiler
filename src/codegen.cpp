#include "compiler.hpp"
#include <Tracy.hpp>

#include <ace_ir.hpp>
#include <ace_obj.hpp>

struct CodegenContext;

enum CodegenValueKind {
    CodegenValueKind_Unknown = 0,
    CodegenValueKind_Function,
    CodegenValueKind_Global,
    CodegenValueKind_StackSlot,
    CodegenValueKind_Constant,
    CodegenValueKind_Inst,
};

struct CodegenValue {
    CodegenValueKind kind;
    union {
        ace::FunctionRef func;
        ace::GlobalRef global;
        ace::StackSlotRef stack_slot;
        ace::ConstRef constant;
        ace::InstRef inst;
    };

    CodegenValue address_of(CodegenContext *ctx);
    CodegenValue load(CodegenContext *ctx, ace::Type *type) const;
    void store(CodegenContext *ctx, const CodegenValue &other);
};

struct CodegenContext {
    File *file;
    ace::Module *module;
    ace::Builder builder;
    ace::Array<ace::Type *> type_values;
    ace::Array<CodegenValue> expr_values;
    ace::Array<CodegenValue> decl_values;

    ace::Array<ace::FunctionRef> function_stack;
};

CodegenValue CodegenValue::address_of(CodegenContext *ctx)
{
    CodegenValue result = {};
    result.kind = CodegenValueKind_Inst;

    switch (this->kind) {
    case CodegenValueKind_Unknown:
    case CodegenValueKind_Inst:
    case CodegenValueKind_Constant: {
        ACE_ASSERT(0);
        break;
    }
    case CodegenValueKind_Function: {
        ACE_ASSERT(!"unimplemented");
        break;
    }
    case CodegenValueKind_StackSlot: {
        result.inst = ctx->builder.insert_stack_addr(this->stack_slot);
        break;
    }
    case CodegenValueKind_Global: {
        result.inst = ctx->builder.insert_global_addr(this->global);
        break;
    }
    }

    return result;
}

CodegenValue CodegenValue::load(CodegenContext *ctx, ace::Type *type) const
{
    CodegenValue result = *this;
    result.kind = CodegenValueKind_Inst;

    switch (this->kind) {
    case CodegenValueKind_Unknown:
    case CodegenValueKind_Inst:
    case CodegenValueKind_Function:
    case CodegenValueKind_Constant: {
        break;
    }
    case CodegenValueKind_StackSlot: {
        result.inst = ctx->builder.insert_stack_load(this->stack_slot);
        break;
    }
    case CodegenValueKind_Global: {
        result.inst = ctx->builder.insert_global_load(this->global, type);
        break;
    }
    }

    return result;
}

void CodegenValue::store(CodegenContext *ctx, const CodegenValue &other)
{
    switch (this->kind) {
    case CodegenValueKind_Unknown:
    case CodegenValueKind_Inst:
    case CodegenValueKind_Function:
    case CodegenValueKind_Constant: {
        ACE_ASSERT(0);
        break;
    }
    case CodegenValueKind_StackSlot: {
        ace::FunctionRef current_func_ref = ctx->builder.current_func_ref;
        ace::Function *func = &ctx->module->functions[current_func_ref.id];
        ace::StackSlot *stack_slot = &func->stack_slots[this->stack_slot.id];

        CodegenValue loaded_val = other.load(ctx, stack_slot->type);
        ACE_ASSERT(loaded_val.kind == CodegenValueKind_Inst);

        ctx->builder.insert_stack_store(this->stack_slot, loaded_val.inst);
        break;
    }
    case CodegenValueKind_Global: {
        ace::Global *global = &ctx->module->globals[this->global.id];

        CodegenValue loaded_val = other.load(ctx, global->type);
        ACE_ASSERT(loaded_val.kind == CodegenValueKind_Inst);

        ctx->builder.insert_global_store(this->global, loaded_val.inst);
        break;
    }
    }
}

static void
codegen_decl(Compiler *compiler, CodegenContext *ctx, DeclRef decl_ref);

static ace::Type *
get_ir_type(Compiler *compiler, CodegenContext *ctx, TypeRef type_ref)
{
    if (ctx->type_values[type_ref.id] != nullptr) {
        return ctx->type_values[type_ref.id];
    }

    Type type = type_ref.get(compiler);
    switch (type.kind) {
    case TypeKind_Unknown:
    case TypeKind_Function:
    case TypeKind_UntypedInt:
    case TypeKind_UntypedFloat:
    case TypeKind_Type: ACE_ASSERT(0);
    case TypeKind_Void: {
        return ctx->module->void_type;
    }
    case TypeKind_Bool: {
        return ctx->module->i8_type;
    }
    case TypeKind_Int: {
        switch (type.int_.bits) {
        case 8: return ctx->module->i8_type;
        case 16: return ctx->module->i16_type;
        case 32: return ctx->module->i32_type;
        case 64: return ctx->module->i64_type;
        }
        break;
    }
    case TypeKind_Float: {
        switch (type.float_.bits) {
        case 32: return ctx->module->f32_type;
        case 64: return ctx->module->f64_type;
        }
        break;
    }
    case TypeKind_Pointer: {
        return ctx->module->create_pointer_type();
    }
    case TypeKind_Slice: {
        ACE_ASSERT(!"unimplemented");
        break;
    }
    case TypeKind_Array: {
        ACE_ASSERT(!"unimplemented");
        break;
    }
    case TypeKind_Tuple: {
        ACE_ASSERT(!"unimplemented");
        break;
    }
    case TypeKind_Struct: {
        ACE_ASSERT(!"unimplemented");
        break;
    }
    }

    ACE_ASSERT(0);
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
        ACE_ASSERT(!"unimplemented");
        break;
    }

    case ExprKind_IntLiteral: {
        Type type = expr.expr_type_ref.get(compiler);

        switch (type.kind) {
        case TypeKind_Int: {
            ace::InstRef int_const =
                ctx->builder.insert_get_const(ctx->module->create_int_const(
                    get_ir_type(compiler, ctx, expr.expr_type_ref),
                    expr.int_literal.i64));

            value.kind = CodegenValueKind_Inst;
            value.inst = int_const;
            break;
        }
        case TypeKind_Float: {
            ace::InstRef float_const =
                ctx->builder.insert_get_const(ctx->module->create_float_const(
                    get_ir_type(compiler, ctx, expr.expr_type_ref),
                    (double)expr.int_literal.i64));

            value.kind = CodegenValueKind_Inst;
            value.inst = float_const;
            break;
        }
        default: ACE_ASSERT(0);
        }

        break;
    }

    case ExprKind_FloatLiteral: {
        ACE_ASSERT(expr.expr_type_ref.get(compiler).kind == TypeKind_Float);

        ace::InstRef float_const =
            ctx->builder.insert_get_const(ctx->module->create_float_const(
                get_ir_type(compiler, ctx, expr.expr_type_ref),
                expr.float_literal.f64));

        value.kind = CodegenValueKind_Inst;
        value.inst = float_const;

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

            auto str_global =
                ctx->module->add_global_string(expr.str_literal.str);

            value.kind = CodegenValueKind_Inst;
            value.inst = ctx->builder.insert_global_addr(str_global);

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
        case DeclKind_Function: {
            value = ctx->decl_values[expr.ident.decl_ref.id];
            ACE_ASSERT(value.kind > 0);
            break;
        }

        case DeclKind_FunctionParameter: {
            value = ctx->decl_values[expr.ident.decl_ref.id];
            ACE_ASSERT(value.kind == CodegenValueKind_Inst);
            break;
        }

        case DeclKind_LocalVarDecl: {
            value = ctx->decl_values[expr.ident.decl_ref.id];
            ACE_ASSERT(value.kind == CodegenValueKind_StackSlot);
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
        ace::Slice<ace::InstRef> params =
            compiler->arena->alloc<ace::InstRef>(expr.func_call.param_refs.len);

        for (size_t i = 0; i < expr.func_call.param_refs.len; ++i) {
            CodegenValue param_value =
                codegen_expr(compiler, ctx, expr.func_call.param_refs[i]);
            TypeRef param_type =
                expr.func_call.param_refs[i].get(compiler).expr_type_ref;
            ace::Type *param_ir_type = get_ir_type(compiler, ctx, param_type);

            param_value = param_value.load(ctx, param_ir_type);
            ACE_ASSERT(param_value.kind == CodegenValueKind_Inst);
            params[i] = param_value.inst;
        }

        CodegenValue func_value =
            codegen_expr(compiler, ctx, expr.func_call.func_expr_ref);

        switch (func_value.kind) {
        case CodegenValueKind_Function: {
            value.kind = CodegenValueKind_Inst;
            value.inst = ctx->builder.insert_func_call(func_value.func, params);
            break;
        }

        default: ACE_ASSERT(0); break;
        }

        break;
    }

    case ExprKind_Unary: {

        switch (expr.unary.op) {
        case UnaryOp_Unknown: ACE_ASSERT(0); break;
        case UnaryOp_AddressOf: {
            CodegenValue operand_value =
                codegen_expr(compiler, ctx, expr.unary.left_ref);

            value = operand_value.address_of(ctx);

            break;
        }
        case UnaryOp_Dereference: {
            ACE_ASSERT(!"unimplemented");
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
        ACE_ASSERT(!"unimplemented");
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

        TypeRef value_type =
            stmt.assign.value_expr_ref.get(compiler).expr_type_ref;
        ace::Type *value_ir_type = get_ir_type(compiler, ctx, value_type);

        receiver_value.store(ctx, value.load(ctx, value_ir_type));
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
        ACE_ASSERT(!"unimplemented");
        break;
    }

    case StmtKind_While: {
        ACE_ASSERT(!"unimplemented");
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
            param_types[i] =
                get_ir_type(compiler, ctx, func_type.func.param_types[i]);
        }

        ace::Type *return_type =
            get_ir_type(compiler, ctx, func_type.func.return_type);

        ace::Linkage linkage = ace::Linkage_Internal;
        if ((decl.func.flags & FunctionFlags_Exported) ||
            (decl.func.flags & FunctionFlags_Extern)) {
            linkage = ace::Linkage_External;
        }

        value.kind = CodegenValueKind_Function;
        value.func = module->add_function(
            decl.name,
            ace::CallingConvention_SystemV,
            linkage,
            false,
            param_types,
            return_type);

        ctx->decl_values[decl_ref.id] = value;

        ctx->function_stack.push_back(value.func);

        for (size_t i = 0; i < decl.func.param_decl_refs.len; ++i) {
            DeclRef param_decl_ref = decl.func.param_decl_refs[i];

            CodegenValue param_value = {};
            param_value.kind = CodegenValueKind_Inst;
            param_value.inst = module->get_func_param(value.func, i);
            ctx->decl_values[param_decl_ref.id] = param_value;
        }

        if (!(decl.func.flags & FunctionFlags_Extern)) {
            ctx->builder.set_function(value.func);

            auto block = module->insert_block_at_end(value.func);
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
        ace::FunctionRef func_ref = *ctx->function_stack.last();
        ace::Type *ir_type = get_ir_type(compiler, ctx, decl.decl_type_ref);

        value.kind = CodegenValueKind_StackSlot;
        value.stack_slot = module->add_stack_slot(func_ref, ir_type);

        ctx->decl_values[decl_ref.id] = value;

        if (decl.local_var_decl.value_expr.id) {
            CodegenValue assigned_value =
                codegen_expr(compiler, ctx, decl.local_var_decl.value_expr);

            ctx->builder.insert_stack_store(
                value.stack_slot, assigned_value.load(ctx, ir_type).inst);
        }

        break;
    }

    case DeclKind_GlobalVarDecl: {
        ACE_ASSERT(!"unimplemented");
        break;
    }
    }
}

void codegen_file(Compiler *compiler, File *file)
{
    ZoneScoped;

    CodegenContext ctx = {};

    ctx.file = file;
    ctx.module = ace::Module::create(
        ace::TargetArch_X86_64, ace::Endianness_LittleEndian);
    ctx.builder = ace::Builder::create(ctx.module);

    ctx.type_values =
        ace::Array<ace::Type *>::create(ace::MallocAllocator::get_instance());
    ctx.type_values.resize(compiler->types.len);
    for (size_t i = 0; i < compiler->types.len; ++i) {
        ctx.type_values[i] = nullptr;
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

    ctx.function_stack =
        ace::Array<ace::FunctionRef>::create(ctx.module->arena);

    for (DeclRef decl_ref : ctx.file->top_level_decls) {
        codegen_decl(compiler, &ctx, decl_ref);
    }

#if 1
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

    ctx.type_values.destroy();
    ctx.decl_values.destroy();
    ctx.expr_values.destroy();

    asm_builder->destroy();
    obj_builder->destroy();
    ctx.module->destroy();
}
