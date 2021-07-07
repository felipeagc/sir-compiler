#include "compiler.hpp"

struct AnalyzerState {
    File *file;
};

static void analyze_stmt(Compiler *compiler, AnalyzerState *state, StmtRef stmt_ref);
static void analyze_decl(Compiler *compiler, AnalyzerState *state, DeclRef decl_ref);

static void analyze_expr(
    Compiler *compiler,
    AnalyzerState *state,
    ExprRef expr_ref,
    TypeRef expected_type = {0})
{
    (void) state;
    (void) expected_type;

    ACE_ASSERT(expr_ref.id > 0);
    Expr expr = compiler->exprs[expr_ref.id];

    switch (expr.kind) {
    case ExprKind_Unknown: {
        ACE_ASSERT(0);
        break;
    }
    case ExprKind_VoidType: {
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
    case ExprKind_VoidLiteral: {
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
    case ExprKind_Unary: {
        compiler->add_error(expr.loc, "unimplemented");
        break;
    }
    case ExprKind_Binary: {
        compiler->add_error(expr.loc, "unimplemented");
        break;
    }
    }

    compiler->exprs[expr_ref.id] = expr;
}

static void analyze_stmt(Compiler *compiler, AnalyzerState *state, StmtRef stmt_ref)
{
    ACE_ASSERT(stmt_ref.id > 0);
    Stmt stmt = compiler->stmts[stmt_ref.id];
    
    switch (stmt.kind) {
    case StmtKind_Unknown: {
        ACE_ASSERT(0);
        break;
    }
    case StmtKind_Block: {
        compiler->add_error(stmt.loc, "unimplemented");
        break;
    }
    case StmtKind_Expr: {
        analyze_expr(compiler, state, stmt.expr.expr_ref);
        break;
    }
    case StmtKind_Return: {
        compiler->add_error(stmt.loc, "unimplemented");
        break;
    }
    case StmtKind_If: {
        compiler->add_error(stmt.loc, "unimplemented");
        break;
    }
    case StmtKind_While: {
        compiler->add_error(stmt.loc, "unimplemented");
        break;
    }
    }

    compiler->stmts[stmt_ref.id] = stmt;
}

static void analyze_decl(Compiler *compiler, AnalyzerState *state, DeclRef decl_ref)
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

        for (StmtRef stmt_ref: decl.func.body_stmts) {
            analyze_stmt(compiler, state, stmt_ref);
        }

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
