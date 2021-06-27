#include "compiler.hpp"

#include <stdio.h>

using namespace ace;

Compiler Compiler::create()
{
    ArenaAllocator *arena =
        ArenaAllocator::create(MallocAllocator::get_instance(), 1 << 24);

    StringMap<Keyword> keyword_map = StringMap<Keyword>::create(arena, 32);
    StringMap<BuiltinFunction> builtin_function_map =
        StringMap<BuiltinFunction>::create(arena, 32);
    Array<Error> errors = Array<Error>::create(arena);
    errors.reserve(64);

    Compiler compiler = {
        .arena = arena,
        .keyword_map = keyword_map,
        .builtin_function_map = builtin_function_map,
        .errors = errors,
    };

    return compiler;
}

void Compiler::destroy()
{
    this->errors.destroy();
    this->builtin_function_map.destroy();
    this->keyword_map.destroy();
    this->arena->destroy();
}

void Compiler::add_error(const Location &loc, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    String message = this->arena->vsprintf(fmt, args);
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

    } catch (...) {
        if (this->errors.len == 0) {
            fprintf(stderr, "error: compilation halted, but compiler got no errors\n");
        }
        this->print_errors();
        exit(1);
    }
}
