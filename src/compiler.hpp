#pragma once

#include <ace_base.hpp>
#include <setjmp.h>

struct Decl;

enum Keyword {
    Keyword_Def,
};

enum BuiltinFunction {
};

struct Scope {
    Scope *parent;
    ace::StringMap<Decl*> decls;
};

enum ExprKind {

};

struct Expr {
    ExprKind kind;
};

enum StmtKind {

};

struct Stmt {
    StmtKind kind;
};

enum DeclKind {
    DeclKind_Function,
};

struct Decl {
    DeclKind kind;
};

struct File {
    ace::String path;
    ace::String text;

    ace::Array<Decl*> top_level_decls;
};

struct Location {
    File *file;
    uint32_t offset;
    uint32_t len;
    uint32_t line;
    uint32_t col;
};

struct Error {
    Location loc;
    ace::String message;
};

struct Compiler {
    ace::ArenaAllocator *arena;
    ace::StringMap<Keyword> keyword_map;
    ace::StringMap<BuiltinFunction> builtin_function_map;
    ace::Array<Error> errors;

    static Compiler create();
    void destroy();

    ACE_PRINTF_FORMATTING(3, 4)
    void add_error(const Location &loc, const char *fmt, ...);
    void halt_compilation();
    void print_errors();

    void compile(ace::String path);
};
