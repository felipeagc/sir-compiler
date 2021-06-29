#include "compiler.hpp"

#include <stdio.h>

Compiler Compiler::create()
{
    ace::ArenaAllocator *arena = ace::ArenaAllocator::create(
        ace::MallocAllocator::get_instance(), 1 << 24);

    ace::StringMap<TokenKind> keyword_map =
        ace::StringMap<TokenKind>::create(arena, 32);
    ace::StringMap<BuiltinFunction> builtin_function_map =
        ace::StringMap<BuiltinFunction>::create(arena, 32);
    ace::Array<Error> errors = ace::Array<Error>::create(arena);
    errors.reserve(64);

    const size_t expected_type_count = 2048;
    const size_t expected_decl_count = 2048;
    const size_t expected_stmt_count = 16384;
    const size_t expected_expr_count = 65536;

    ace::StringMap<TypeRef> type_map = ace::StringMap<TypeRef>::create(
        ace::MallocAllocator::get_instance(), expected_type_count);
    ace::Array<Type> types =
        ace::Array<Type>::create(ace::MallocAllocator::get_instance());
    types.reserve(expected_type_count);
    types.push_back({}); // 0th type

    ace::Array<Decl> decls =
        ace::Array<Decl>::create(ace::MallocAllocator::get_instance());
    decls.reserve(expected_decl_count);
    decls.push_back({}); // 0th decl

    ace::Array<Stmt> stmts =
        ace::Array<Stmt>::create(ace::MallocAllocator::get_instance());
    stmts.reserve(expected_stmt_count);
    stmts.push_back({}); // 0th stmt

    ace::Array<Expr> exprs =
        ace::Array<Expr>::create(ace::MallocAllocator::get_instance());
    exprs.reserve(expected_expr_count);
    exprs.push_back({}); // 0th expr

    Compiler compiler = {
        .arena = arena,
        .keyword_map = keyword_map,
        .builtin_function_map = builtin_function_map,
        .errors = errors,

        .type_map = type_map,
        .types = types,
        .decls = decls,
        .stmts = stmts,
        .exprs = exprs,
    };

    return compiler;
}

void Compiler::destroy()
{
    this->exprs.destroy();
    this->stmts.destroy();
    this->decls.destroy();
    this->type_map.destroy();
    this->types.destroy();

    this->errors.destroy();
    this->builtin_function_map.destroy();
    this->keyword_map.destroy();
    this->arena->destroy();
}

void Compiler::add_error(const Location &loc, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    ace::String message = this->arena->vsprintf(fmt, args);
    va_end(args);

    Error err = {
        .loc = loc,
        .message = message,
    };
    this->errors.push_back(err);
}

void Compiler::halt_compilation()
{
    throw 0;
}

void Compiler::print_errors()
{
    for (Error &err : this->errors) {
        fprintf(
            stderr,
            "error: %.*s:%u:%u: %.*s\n",
            (int)err.loc.file->path.len,
            err.loc.file->path.ptr,
            err.loc.line,
            err.loc.col,
            (int)err.message.len,
            err.message.ptr);
    }
}

void Compiler::compile(ace::String path)
{
    try {
        FILE *f = fopen(this->arena->null_terminate(path), "rb");
        if (!f) {
            this->add_error(
                Location{},
                "could not open file: '%.*s'",
                (int)path.len,
                path.ptr);
            this->halt_compilation();
        }
        fseek(f, 0, SEEK_END);
        size_t file_size = ftell(f);
        fseek(f, 0, SEEK_SET);

        ace::Slice<char> file_content = this->arena->alloc<char>(file_size);
        size_t bytes_read = fread(file_content.ptr, 1, file_size, f);

        fclose(f);

        if (bytes_read != file_size) {
            this->add_error(
                Location{},
                "could not fully read file: '%.*s'",
                (int)path.len,
                path.ptr);
            this->halt_compilation();
        }

        File *file = this->arena->alloc<File>();
        *file = {
            .path = path,
            .text = ace::String{file_content.ptr, file_content.len},
            .top_level_decls = ace::Array<DeclRef>::create(this->arena),
        };

        parse_file(this, file);
    } catch (...) {
        if (this->errors.len == 0) {
            fprintf(
                stderr,
                "error: compilation halted, but compiler got no errors\n");
        }
        this->print_errors();
        exit(1);
    }
}
