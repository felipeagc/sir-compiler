#include "compiler.hpp"

#include <ace_ir.hpp>
#include <ace_obj.hpp>

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
};

struct CodegenContext {
    File *file;
    ace::Module *module;
    ace::Builder builder;
    ace::Array<ace::Type *> type_values;
    ace::Array<CodegenValue> expr_values;
    ace::Array<CodegenValue> decl_values;
};

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
        ACE_ASSERT(!"unimplemented");
        break;
    }

    case ExprKind_FloatLiteral: {
        ACE_ASSERT(!"unimplemented");
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
        ACE_ASSERT(!"unimplemented");
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
    Stmt stmt = stmt_ref.get(compiler);
    switch (stmt.kind) {
    case StmtKind_Unknown: ACE_ASSERT(0); break;
    case StmtKind_Expr: {
        codegen_expr(compiler, ctx, stmt.expr.expr_ref);
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
    ace::Module *module = ctx->module;
    Decl decl = decl_ref.get(compiler);
    CodegenValue value = {};

    switch (decl.kind) {
    case DeclKind_Unknown: {
        break;
    }
    case DeclKind_ConstDecl: {
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

        if (!(decl.func.flags & FunctionFlags_Extern)) {
            ctx->builder.set_function(value.func);

            auto block = module->insert_block_at_end(value.func);
            ctx->builder.position_at_end(block);

            for (StmtRef stmt_ref : decl.func.body_stmts) {
                codegen_stmt(compiler, ctx, stmt_ref);
            }

            ctx->builder.insert_return_void();
        }

        break;
    }
    case DeclKind_FunctionParameter: {
        break;
    }
    case DeclKind_LocalVarDecl: {
        break;
    }
    case DeclKind_GlobalVarDecl: {
        break;
    }
    }
}

void codegen_file(Compiler *compiler, File *file)
{
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

    for (DeclRef decl_ref : ctx.file->top_level_decls) {
        codegen_decl(compiler, &ctx, decl_ref);
    }

#if 0
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
