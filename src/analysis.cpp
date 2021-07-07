#include "compiler.hpp"

struct AnalyzerState {
    File *file;
};

static void
analyze_stmt(Compiler *compiler, AnalyzerState *state, StmtRef stmt_ref);
static void
analyze_decl(Compiler *compiler, AnalyzerState *state, DeclRef decl_ref);

static void analyze_expr(
    Compiler *compiler,
    AnalyzerState *state,
    ExprRef expr_ref,
    TypeRef expected_type_ref = {0})
{
    (void)state;

    ACE_ASSERT(expr_ref.id > 0);
    Expr expr = compiler->exprs[expr_ref.id];

    switch (expr.kind) {
    case ExprKind_Unknown: {
        ACE_ASSERT(0);
        break;
    }
    case ExprKind_VoidType: {
        expr.expr_type_ref = compiler->type_type;
        expr.as_type_ref = compiler->void_type;
        break;
    }
    case ExprKind_BoolType: {
        expr.expr_type_ref = compiler->type_type;
        expr.as_type_ref = compiler->bool_type;
        break;
    }
    case ExprKind_IntType: {
        expr.expr_type_ref = compiler->type_type;
        if (expr.int_type.is_signed) {
            switch (expr.int_type.bits) {
            case 8: expr.as_type_ref = compiler->i8_type; break;
            case 16: expr.as_type_ref = compiler->i16_type; break;
            case 32: expr.as_type_ref = compiler->i32_type; break;
            case 64: expr.as_type_ref = compiler->i64_type; break;
            }
        } else {
            switch (expr.int_type.bits) {
            case 8: expr.as_type_ref = compiler->u8_type; break;
            case 16: expr.as_type_ref = compiler->u16_type; break;
            case 32: expr.as_type_ref = compiler->u32_type; break;
            case 64: expr.as_type_ref = compiler->u64_type; break;
            }
        }
        break;
    }
    case ExprKind_FloatType: {
        expr.expr_type_ref = compiler->type_type;
        switch (expr.float_type.bits) {
        case 32: expr.as_type_ref = compiler->f32_type; break;
        case 64: expr.as_type_ref = compiler->f64_type; break;
        }
        break;
    }
    case ExprKind_PointerType: {
        analyze_expr(
            compiler, state, expr.ptr_type.sub_expr_ref, compiler->type_type);
        TypeRef sub_type =
            compiler->exprs[expr.ptr_type.sub_expr_ref.id].expr_type_ref;
        if (sub_type.id) {
            expr.expr_type_ref = compiler->type_type;
            expr.as_type_ref = compiler->create_pointer_type(sub_type);
        }
        break;
    }
    case ExprKind_BoolLiteral: {
        expr.expr_type_ref = compiler->bool_type;
        break;
    }
    case ExprKind_IntLiteral: {
        if (expected_type_ref.id &&
            (compiler->types[expected_type_ref.id].kind == TypeKind_Int ||
             compiler->types[expected_type_ref.id].kind == TypeKind_Float)) {
            expr.expr_type_ref = expected_type_ref;
        } else {
            expr.expr_type_ref = compiler->untyped_int_type;
        }
        break;
    }
    case ExprKind_FloatLiteral: {
        if (expected_type_ref.id &&
            compiler->types[expected_type_ref.id].kind == TypeKind_Float) {
            expr.expr_type_ref = expected_type_ref;
        } else {
            expr.expr_type_ref = compiler->untyped_float_type;
        }
        break;
    }
    case ExprKind_StringLiteral: {
        compiler->add_error(expr.loc, "unimplemented string literal");
        break;
    }
    case ExprKind_NullLiteral: {
        if (expected_type_ref.id &&
            compiler->types[expected_type_ref.id].kind == TypeKind_Pointer) {
            expr.expr_type_ref = expected_type_ref;
        } else {
            expr.expr_type_ref =
                compiler->create_pointer_type(compiler->void_type);
        }
        break;
    }
    case ExprKind_VoidLiteral: {
        expr.expr_type_ref = compiler->void_type;
        break;
    }
    case ExprKind_Identifier: {
        compiler->add_error(expr.loc, "unimplemented identifier expr");
        break;
    }
    case ExprKind_Function: {
        compiler->add_error(expr.loc, "unimplemented function expr");
        break;
    }
    case ExprKind_FunctionCall: {
        compiler->add_error(expr.loc, "unimplemented function call");
        break;
    }
    case ExprKind_Unary: {
        compiler->add_error(expr.loc, "unimplemented unary expr");
        break;
    }
    case ExprKind_Binary: {
        compiler->add_error(expr.loc, "unimplemented binary expr");
        break;
    }
    }

    if (expected_type_ref.id != expr.expr_type_ref.id) {
        Type &expected_type = compiler->types[expected_type_ref.id];
        Type &expr_type = compiler->types[expr.expr_type_ref.id];

        ace::String expected_type_str = expected_type.to_string(compiler);
        ace::String expr_type_str = expr_type.to_string(compiler);

        compiler->add_error(
            expr.loc,
            "unmatched types, expecting '%.*s', but got '%.*s'",
            (int)expected_type_str.len,
            expected_type_str.ptr,
            (int)expr_type_str.len,
            expr_type_str.ptr);
    }

    compiler->exprs[expr_ref.id] = expr;
}

static void
analyze_stmt(Compiler *compiler, AnalyzerState *state, StmtRef stmt_ref)
{
    ACE_ASSERT(stmt_ref.id > 0);
    Stmt stmt = compiler->stmts[stmt_ref.id];

    switch (stmt.kind) {
    case StmtKind_Unknown: {
        ACE_ASSERT(0);
        break;
    }
    case StmtKind_Block: {
        compiler->add_error(stmt.loc, "unimplemented block stmt");
        break;
    }
    case StmtKind_Expr: {
        analyze_expr(compiler, state, stmt.expr.expr_ref);
        break;
    }
    case StmtKind_Return: {
        compiler->add_error(stmt.loc, "unimplemented return stmt");
        break;
    }
    case StmtKind_If: {
        compiler->add_error(stmt.loc, "unimplemented if stmt");
        break;
    }
    case StmtKind_While: {
        compiler->add_error(stmt.loc, "unimplemented while stmt");
        break;
    }
    }

    compiler->stmts[stmt_ref.id] = stmt;
}

static void
analyze_decl(Compiler *compiler, AnalyzerState *state, DeclRef decl_ref)
{
    ACE_ASSERT(decl_ref.id > 0);
    Decl decl = compiler->decls[decl_ref.id];

    switch (decl.kind) {
    case DeclKind_Unknown: {
        ACE_ASSERT(0);
        break;
    }
    case DeclKind_ConstDecl: {
        compiler->add_error(decl.loc, "const decl unimplemented");
        break;
    }
    case DeclKind_Function: {
        for (DeclRef param_decl_ref : decl.func.param_decl_refs) {
            analyze_decl(compiler, state, param_decl_ref);
        }

        for (StmtRef stmt_ref : decl.func.body_stmts) {
            analyze_stmt(compiler, state, stmt_ref);
        }

        break;
    }
    case DeclKind_FunctionParameter: {
        analyze_expr(
            compiler, state, decl.func_param.type_expr, compiler->type_type);
        break;
    }
    case DeclKind_LocalVarDecl: {
        compiler->add_error(decl.loc, "local var decl unimplemented");
        break;
    }
    case DeclKind_GlobalVarDecl: {
        compiler->add_error(decl.loc, "global var decl unimplemented");
        break;
    }
    }

    compiler->decls[decl_ref.id] = decl;
}

void analyze_file(Compiler *compiler, File *file)
{
    AnalyzerState state = {};
    state.file = file;

    // Register top level symbols
    for (DeclRef decl_ref : file->top_level_decls) {
        file->scope->add(compiler, decl_ref);
    }

    for (DeclRef decl_ref : state.file->top_level_decls) {
        analyze_decl(compiler, &state, decl_ref);
    }

    if (compiler->errors.len > 0) {
        compiler->halt_compilation();
    }
}
