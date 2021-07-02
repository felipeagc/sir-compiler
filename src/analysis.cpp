#include "compiler.hpp"

struct AnalyzerState {
    File *file;
};

void analyze_expr(
    Compiler *compiler,
    AnalyzerState *state,
    ExprRef expr_ref,
    TypeRef expected_type = {0})
{
    Expr expr = compiler->exprs[expr_ref.id];
    switch (expr.kind) {
    case ExprKind_UnitType: {
        compiler->add_error(expr.loc, "unimplemented");
        break;
    }
    case ExprKind_BoolType: {
        compiler->add_error(expr.loc, "unimplemented");
        break;
    }
    case ExprKind_IntType: {
        compiler->add_error(expr.loc, "unimplemented");
        break;
    }
    case ExprKind_FloatType: {
        compiler->add_error(expr.loc, "unimplemented");
        break;
    }
    case ExprKind_PointerType: {
        compiler->add_error(expr.loc, "unimplemented");
        break;
    }
    case ExprKind_BoolLiteral: {
        compiler->add_error(expr.loc, "unimplemented");
        break;
    }
    case ExprKind_IntLiteral: {
        compiler->add_error(expr.loc, "unimplemented");
        break;
    }
    case ExprKind_FloatLiteral: {
        compiler->add_error(expr.loc, "unimplemented");
        break;
    }
    case ExprKind_StringLiteral: {
        compiler->add_error(expr.loc, "unimplemented");
        break;
    }
    case ExprKind_NullLiteral: {
        compiler->add_error(expr.loc, "unimplemented");
        break;
    }
    case ExprKind_UnitLiteral: {
        compiler->add_error(expr.loc, "unimplemented");
        break;
    }
    case ExprKind_Identifier: {
        compiler->add_error(expr.loc, "unimplemented");
        break;
    }
    case ExprKind_Function: {
        compiler->add_error(expr.loc, "unimplemented");
        break;
    }
    case ExprKind_FunctionCall: {
        compiler->add_error(expr.loc, "unimplemented");
        break;
    }
    }

    compiler->exprs[expr_ref.id] = expr;
}

void analyze_decl(Compiler *compiler, AnalyzerState *state, DeclRef decl_ref)
{
    Decl decl = compiler->decls[decl_ref.id];

    switch (decl.kind) {
    case DeclKind_ConstDecl: {
        compiler->add_error(decl.loc, "const decl unimplemented");
        break;
    }
    case DeclKind_Function: {
        compiler->add_error(decl.loc, "function decl unimplemented");
        break;
    }
    case DeclKind_FunctionParameter: {
        compiler->add_error(decl.loc, "function param decl unimplemented");
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
