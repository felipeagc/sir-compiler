#include "compiler.hpp"

#include <stdio.h>
#ifdef __linux__
#include <time.h>
#endif

struct Clock {
    int64_t start_seconds = 0;
    int64_t start_nanoseconds = 0;

    void start()
    {
#ifdef __linux__
        timespec start_time;
        clock_gettime(CLOCK_MONOTONIC, &start_time);
        this->start_seconds = start_time.tv_sec;
        this->start_nanoseconds = start_time.tv_nsec;
#else
#error Unsupported OS
#endif
    }

    double elapsed()
    {
#ifdef __linux__
        timespec end_time;
        clock_gettime(CLOCK_MONOTONIC, &end_time);
        int64_t ns_diff =
            ((int64_t)end_time.tv_sec - (int64_t)this->start_seconds) *
                (int64_t)1000000000 +
            ((int64_t)end_time.tv_nsec - (int64_t)this->start_nanoseconds);
        return (double)ns_diff / 1000000000.0;
#else
#error Unsupported OS
#endif
    }
};

bool ExprRef::is_lvalue(Compiler *compiler)
{
    bool is_lvalue = false;
    Expr expr = this->get(compiler);

    if (expr.kind == ExprKind_Identifier) {
        DeclKind decl_kind = expr.ident.decl_ref.get(compiler).kind;
        is_lvalue =
            (decl_kind == DeclKind_LocalVarDecl ||
             decl_kind == DeclKind_GlobalVarDecl);
    } else if (
        expr.kind == ExprKind_Unary && expr.unary.op == UnaryOp_Dereference) {
        is_lvalue = true;
    } else if (expr.kind == ExprKind_Subscript) {
        is_lvalue = true;
    } else if (expr.kind == ExprKind_Access) {
        return expr.access.left_ref.is_lvalue(compiler);
    }

    return is_lvalue;
}

Scope *Scope::create(Compiler *compiler, FileRef file_ref, Scope *parent)
{
    Scope *scope = compiler->arena->alloc<Scope>();
    *scope = {};

    scope->file_ref = file_ref;
    scope->parent = parent;
    scope->decl_refs = StringMap<DeclRef>::create(compiler->arena);

    return scope;
}

void Scope::add(Compiler *compiler, DeclRef decl_ref)
{
    String name = compiler->decl_names[decl_ref];
    if (!name.equal("_")) {
        DeclRef found_decl = this->lookup(name);
        if (found_decl.id != 0) {
            const Location &loc = compiler->decl_locs[decl_ref];
            compiler->add_error(
                loc,
                "duplicate declaration of: '%.*s'",
                (int)name.len,
                name.ptr);
        } else {
            this->decl_refs.set(name, decl_ref);
        }
    }
}

DeclRef Scope::lookup(const String &name)
{
    if (name.equal("_")) return {0};

    DeclRef found_decl_ref = {0};
    if (this->decl_refs.get(name, &found_decl_ref)) {
        return found_decl_ref;
    }

    if (this->parent) {
        return this->parent->lookup(name);
    }

    return {0};
}

Compiler Compiler::create()
{
    init_parser_tables();

    ArenaAllocator *arena =
        ArenaAllocator::create(MallocAllocator::get_instance());

    StringMap<TokenKind> keyword_map =
        StringMap<TokenKind>::create(MallocAllocator::get_instance());
    StringMap<BuiltinFunction> builtin_function_map =
        StringMap<BuiltinFunction>::create(MallocAllocator::get_instance());
    Array<Error> errors = Array<Error>::create(MallocAllocator::get_instance());

    StringBuilder sb = StringBuilder::create(MallocAllocator::get_instance());

    StringMap<bool> defines = StringMap<bool>::create(MallocAllocator::get_instance());
    defines.set("linux", true);

    Array<File> files = Array<File>::create(MallocAllocator::get_instance());
    files.push_back({}); // 0th file

    StringMap<TypeRef> type_map =
        StringMap<TypeRef>::create(MallocAllocator::get_instance());
    Array<Type> types = Array<Type>::create(MallocAllocator::get_instance());
    types.push_back({}); // 0th type

    Array<Decl> decls = Array<Decl>::create(MallocAllocator::get_instance());
    decls.push_back({}); // 0th decl

    Array<TypeRef> decl_types =
        Array<TypeRef>::create(MallocAllocator::get_instance());
    decl_types.push_back({}); // 0th decl

    Array<TypeRef> decl_as_types =
        Array<TypeRef>::create(MallocAllocator::get_instance());
    decl_as_types.push_back({}); // 0th decl

    Array<Stmt> stmts = Array<Stmt>::create(MallocAllocator::get_instance());
    stmts.push_back({}); // 0th decl

    Array<Expr> exprs = Array<Expr>::create(MallocAllocator::get_instance());
    exprs.push_back({}); // 0th expr

    Array<TypeRef> expr_types =
        Array<TypeRef>::create(MallocAllocator::get_instance());
    expr_types.push_back({}); // 0th expr

    Array<TypeRef> expr_as_types =
        Array<TypeRef>::create(MallocAllocator::get_instance());
    expr_as_types.push_back({}); // 0th expr

    Array<Location> expr_locs =
        Array<Location>::create(MallocAllocator::get_instance());
    expr_locs.push_back({}); // 0th expr

    Array<Location> stmt_locs =
        Array<Location>::create(MallocAllocator::get_instance());
    stmt_locs.push_back({}); // 0th expr

    Array<Location> decl_locs =
        Array<Location>::create(MallocAllocator::get_instance());
    decl_locs.push_back({}); // 0th expr

    Array<String> decl_names =
        Array<String>::create(MallocAllocator::get_instance());
    decl_names.push_back({}); // 0th expr

    keyword_map.set("extern", TokenKind_Extern);
    keyword_map.set("vararg", TokenKind_VarArg);
    keyword_map.set("export", TokenKind_Export);
    keyword_map.set("inline", TokenKind_Inline);
    keyword_map.set("distinct", TokenKind_Distinct);
    keyword_map.set("fn", TokenKind_Fn);
    keyword_map.set("type", TokenKind_Type);
    keyword_map.set("struct", TokenKind_Struct);
    keyword_map.set("global", TokenKind_Global);
    keyword_map.set("macro", TokenKind_Macro);
    keyword_map.set("null", TokenKind_Null);
    keyword_map.set("true", TokenKind_True);
    keyword_map.set("false", TokenKind_False);
    keyword_map.set("if", TokenKind_If);
    keyword_map.set("else", TokenKind_Else);
    keyword_map.set("while", TokenKind_While);
    keyword_map.set("break", TokenKind_Break);
    keyword_map.set("continue", TokenKind_Continue);
    keyword_map.set("return", TokenKind_Return);
    keyword_map.set("void", TokenKind_Void);
    keyword_map.set("bool", TokenKind_Bool);
    keyword_map.set("u8", TokenKind_U8);
    keyword_map.set("u16", TokenKind_U16);
    keyword_map.set("u32", TokenKind_U32);
    keyword_map.set("u64", TokenKind_U64);
    keyword_map.set("i8", TokenKind_I8);
    keyword_map.set("i16", TokenKind_I16);
    keyword_map.set("i32", TokenKind_I32);
    keyword_map.set("i64", TokenKind_I64);
    keyword_map.set("f32", TokenKind_F32);
    keyword_map.set("f64", TokenKind_F64);
    keyword_map.set("usize", TokenKind_USize);
    keyword_map.set("isize", TokenKind_ISize);
    keyword_map.set("and", TokenKind_And);
    keyword_map.set("or", TokenKind_Or);

    builtin_function_map.set("sizeof", BuiltinFunction_Sizeof);
    builtin_function_map.set("alignof", BuiltinFunction_Alignof);
    builtin_function_map.set("ptrcast", BuiltinFunction_PtrCast);
    builtin_function_map.set("defined", BuiltinFunction_Defined);

    Compiler compiler = {
        .arena = arena,
        .keyword_map = keyword_map,
        .builtin_function_map = builtin_function_map,
        .errors = errors,
        .sb = sb,

        .distinct_type_counter = 0,

        .defines = defines,
        .files = files,
        .type_map = type_map,
        .types = types,
        .decls = decls,
        .stmts = stmts,
        .exprs = exprs,

        .expr_locs = expr_locs,
        .decl_locs = decl_locs,
        .stmt_locs = stmt_locs,

        .decl_names = decl_names,

        .decl_types = decl_types,
        .decl_as_types = decl_as_types,

        .expr_types = expr_types,
        .expr_as_types = expr_as_types,

        .void_type = {0},
        .type_type = {0},
        .bool_type = {0},
        .untyped_int_type = {0},
        .untyped_float_type = {0},
        .u8_type = {0},
        .u16_type = {0},
        .u32_type = {0},
        .u64_type = {0},
        .i8_type = {0},
        .i16_type = {0},
        .i32_type = {0},
        .i64_type = {0},
        .f32_type = {0},
        .f64_type = {0},
        .usize_type = {0},
        .isize_type = {0},
    };

    {
        Type type = {};
        type.kind = TypeKind_Unknown;
        compiler.get_cached_type(type);
    }

    {
        Type type = {};
        type.kind = TypeKind_Void;
        compiler.void_type = compiler.get_cached_type(type);
    }

    {
        Type type = {};
        type.kind = TypeKind_Bool;
        compiler.bool_type = compiler.get_cached_type(type);
    }

    {
        Type type = {};
        type.kind = TypeKind_Type;
        compiler.type_type = compiler.get_cached_type(type);
    }

    {
        Type type = {};
        type.kind = TypeKind_UntypedInt;
        compiler.untyped_int_type = compiler.get_cached_type(type);
    }

    {
        Type type = {};
        type.kind = TypeKind_UntypedFloat;
        compiler.untyped_float_type = compiler.get_cached_type(type);
    }

    {
        Type type = {};
        type.kind = TypeKind_Int;
        type.int_.bits = 8;
        type.int_.is_signed = false;
        compiler.u8_type = compiler.get_cached_type(type);
    }

    {
        Type type = {};
        type.kind = TypeKind_Int;
        type.int_.bits = 16;
        type.int_.is_signed = false;
        compiler.u16_type = compiler.get_cached_type(type);
    }

    {
        Type type = {};
        type.kind = TypeKind_Int;
        type.int_.bits = 32;
        type.int_.is_signed = false;
        compiler.u32_type = compiler.get_cached_type(type);
    }

    {
        Type type = {};
        type.kind = TypeKind_Int;
        type.int_.bits = 64;
        type.int_.is_signed = false;
        compiler.u64_type = compiler.get_cached_type(type);
    }

    {
        Type type = {};
        type.kind = TypeKind_Int;
        type.int_.bits = 8;
        type.int_.is_signed = true;
        compiler.i8_type = compiler.get_cached_type(type);
    }

    {
        Type type = {};
        type.kind = TypeKind_Int;
        type.int_.bits = 16;
        type.int_.is_signed = true;
        compiler.i16_type = compiler.get_cached_type(type);
    }

    {
        Type type = {};
        type.kind = TypeKind_Int;
        type.int_.bits = 32;
        type.int_.is_signed = true;
        compiler.i32_type = compiler.get_cached_type(type);
    }

    {
        Type type = {};
        type.kind = TypeKind_Int;
        type.int_.bits = 64;
        type.int_.is_signed = true;
        compiler.i64_type = compiler.get_cached_type(type);
    }

    {
        Type type = {};
        type.kind = TypeKind_Float;
        type.float_.bits = 32;
        compiler.f32_type = compiler.get_cached_type(type);
    }

    {
        Type type = {};
        type.kind = TypeKind_Float;
        type.float_.bits = 64;
        compiler.f64_type = compiler.get_cached_type(type);
    }

    {
        // TODO: use different sizes depending on architecture
        Type type = {};
        type.kind = TypeKind_Int;
        type.int_.bits = 64;
        type.int_.is_signed = false;
        type.int_.is_size = true;
        compiler.usize_type = compiler.get_cached_type(type);
    }

    {
        // TODO: use different sizes depending on architecture
        Type type = {};
        type.kind = TypeKind_Int;
        type.int_.bits = 64;
        type.int_.is_signed = true;
        type.int_.is_size = true;
        compiler.isize_type = compiler.get_cached_type(type);
    }

    return compiler;
}

void Compiler::destroy()
{
    this->exprs.destroy();
    this->stmts.destroy();
    this->decls.destroy();
    this->expr_locs.destroy();
    this->stmt_locs.destroy();
    this->decl_locs.destroy();
    this->decl_types.destroy();
    this->decl_as_types.destroy();
    this->decl_names.destroy();
    this->expr_types.destroy();
    this->expr_as_types.destroy();
    this->type_map.destroy();
    this->types.destroy();
    this->files.destroy();

    this->sb.destroy();
    this->errors.destroy();
    this->builtin_function_map.destroy();
    this->keyword_map.destroy();
    this->arena->destroy();
}

size_t Compiler::get_error_checkpoint()
{
    return this->errors.len;
}

void Compiler::restore_error_checkpoint(size_t checkpoint)
{
    this->errors.len = checkpoint;
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
    LANG_ASSERT(this->errors.len > 0);

    for (Error &err : this->errors) {
        if (err.loc.file_ref.id) {
            File *file = &this->files[err.loc.file_ref.id];
            fprintf(
                stderr,
                "error: %.*s:%u:%u: %.*s\n",
                (int)file->path.len,
                file->path.ptr,
                err.loc.line,
                err.loc.col,
                (int)err.message.len,
                err.message.ptr);
        } else {
            fprintf(
                stderr,
                "error: (unknown file):%u:%u: %.*s\n",
                err.loc.line,
                err.loc.col,
                (int)err.message.len,
                err.message.ptr);
        }
    }
}

static void print_time_taken(const char *task_name, double time)
{
    printf("%s time: %.3lf seconds\n", task_name, time);
}

void Compiler::compile(String path)
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

        Slice<char> file_content = this->arena->alloc<char>(file_size + 1);
        size_t bytes_read = fread(file_content.ptr, 1, file_size, f);
        file_content[file_size] = '\0';

        fclose(f);

        if (bytes_read != file_size) {
            this->add_error(
                Location{},
                "could not fully read file: '%.*s'",
                (int)path.len,
                path.ptr);
            this->halt_compilation();
        }

        FileRef file_ref = this->add_file({});
        this->files[file_ref.id] = {
            .path = path,
            .text = String{file_content.ptr, file_content.len},
            .line_count = 0,
            .scope = Scope::create(this, file_ref),
            .top_level_decls = Array<DeclRef>::create(this->arena),
        };

        Clock total_clock = {};
        Clock phase_clock = {};

        total_clock.start();

        phase_clock.start();
        parse_file(this, file_ref);
        print_time_taken("Parser", phase_clock.elapsed());

        phase_clock.start();
        analyze_file(this, file_ref);
        print_time_taken("Analysis", phase_clock.elapsed());

        phase_clock.start();
        codegen_file(this, file_ref);
        print_time_taken("Codegen", phase_clock.elapsed());

        {
            File *file = &this->files[file_ref.id];

            double time = total_clock.elapsed();
            double total_line_count = (double)file->line_count;

            printf("Compilation time: %.3lf seconds\n", time);
            printf("Line count: %zu seconds\n", file->line_count);
            printf(
                "Lines per second: %.3lf lines/s\n", total_line_count / time);
        }
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

TypeRef Compiler::get_cached_type(Type &type)
{
    String type_string = type.to_string(this);
    TypeRef existing_type_ref = {};
    if (this->type_map.get(type_string, &existing_type_ref)) {
        return existing_type_ref;
    }

    TypeRef type_ref = {(uint32_t)this->types.len};
    this->types.push_back(type);
    this->type_map.set(type_string, type_ref);
    return type_ref;
}

TypeRef Compiler::create_pointer_type(TypeRef sub)
{
    Type type = {};
    type.kind = TypeKind_Pointer;
    type.pointer.sub_type = sub;
    return this->get_cached_type(type);
}

TypeRef Compiler::create_distinct_type(TypeRef sub)
{
    Type type = {};
    type.kind = TypeKind_Distinct;
    type.distinct.sub_type = sub;
    type.distinct.index = this->distinct_type_counter++;
    return this->get_cached_type(type);
}

TypeRef
Compiler::create_struct_type(Slice<TypeRef> fields, Slice<String> field_names)
{
    Type type = {};
    type.kind = TypeKind_Struct;
    type.struct_.field_types = fields;
    type.struct_.field_names = field_names;
    type.struct_.field_map = StringMap<uint32_t>::create(this->arena, 32);
    for (size_t i = 0; i < field_names.len; ++i) {
        type.struct_.field_map.set(field_names[i], i);
    }
    return this->get_cached_type(type);
}

TypeRef Compiler::create_tuple_type(Slice<TypeRef> fields)
{
    Type type = {};
    type.kind = TypeKind_Tuple;
    type.tuple.field_types = fields;
    return this->get_cached_type(type);
}

TypeRef Compiler::create_array_type(TypeRef sub, size_t size)
{
    Type type = {};
    type.kind = TypeKind_Array;
    type.array.sub_type = sub;
    type.array.size = size;
    return this->get_cached_type(type);
}

TypeRef Compiler::create_slice_type(TypeRef sub)
{
    Type type = {};
    type.kind = TypeKind_Slice;
    type.slice.sub_type = sub;
    return this->get_cached_type(type);
}

TypeRef Compiler::create_func_type(
    TypeRef return_type, Slice<TypeRef> param_types, bool vararg)
{
    Type type = {};
    type.kind = TypeKind_Function;
    type.func.return_type = return_type;
    type.func.param_types = param_types;
    type.func.vararg = vararg;
    return this->get_cached_type(type);
}

String Type::to_string(Compiler *compiler)
{
    if (this->str.len > 0) {
        return this->str;
    }

    switch (this->kind) {
    case TypeKind_MAX: LANG_ASSERT(0); break;
    case TypeKind_Unknown: {
        this->str = "@unknown";
        break;
    }
    case TypeKind_Void: {
        this->str = "@void";
        break;
    }
    case TypeKind_Type: {
        this->str = "@type";
        break;
    }
    case TypeKind_Bool: {
        this->str = "@bool";
        break;
    }
    case TypeKind_UntypedInt: {
        this->str = "@untyped_int";
        break;
    }
    case TypeKind_UntypedFloat: {
        this->str = "@untyped_float";
        break;
    }
    case TypeKind_Int: {
        if (this->int_.is_size) {
            if (this->int_.is_signed) {
                this->str =
                    compiler->arena->sprintf("@isize(%u)", this->int_.bits);
            } else {
                this->str =
                    compiler->arena->sprintf("@usize(%u)", this->int_.bits);
            }
        } else {
            if (this->int_.is_signed) {
                this->str =
                    compiler->arena->sprintf("@int(%u)", this->int_.bits);
            } else {
                this->str =
                    compiler->arena->sprintf("@uint(%u)", this->int_.bits);
            }
        }
        break;
    }
    case TypeKind_Float: {
        this->str = compiler->arena->sprintf("@float(%u)", this->float_.bits);
        break;
    }
    case TypeKind_Pointer: {
        String sub_str =
            compiler->types[this->pointer.sub_type.id].to_string(compiler);
        this->str = compiler->arena->sprintf(
            "@ptr(%.*s)", (int)sub_str.len, sub_str.ptr);
        break;
    }
    case TypeKind_Distinct: {
        String sub_str =
            compiler->types[this->distinct.sub_type.id].to_string(compiler);
        this->str = compiler->arena->sprintf(
            "@dist(%.*s, %u)",
            (int)sub_str.len,
            sub_str.ptr,
            this->distinct.index);
        break;
    }
    case TypeKind_Array: {
        String sub_str =
            compiler->types[this->array.sub_type.id].to_string(compiler);
        this->str = compiler->arena->sprintf(
            "@arr(%.*s, %lu)", (int)sub_str.len, sub_str.ptr, this->array.size);
        break;
    }
    case TypeKind_Slice: {
        String sub_str =
            compiler->types[this->slice.sub_type.id].to_string(compiler);
        this->str = compiler->arena->sprintf(
            "@slice(%.*s)", (int)sub_str.len, sub_str.ptr);
        break;
    }
    case TypeKind_Tuple: {
        StringBuilder sb =
            StringBuilder::create(MallocAllocator::get_instance());

        sb.append("@tuple(");

        for (size_t i = 0; i < this->tuple.field_types.len; ++i) {
            auto field_type_ref = this->tuple.field_types[i];
            Type *field_type = &compiler->types[field_type_ref.id];

            if (i > 0) sb.append(", ");

            String field_str = field_type->to_string(compiler);
            sb.append(field_str);
        }

        sb.append(")");

        this->str = sb.build_null_terminated(compiler->arena);

        sb.destroy();
        break;
    }
    case TypeKind_Struct: {
        StringBuilder sb =
            StringBuilder::create(MallocAllocator::get_instance());

        sb.append("@struct(");

        for (size_t i = 0; i < this->struct_.field_types.len; ++i) {
            auto field_type_ref = this->struct_.field_types[i];
            auto field_name = this->struct_.field_names[i];
            Type *field_type = &compiler->types[field_type_ref.id];

            if (i > 0) {
                sb.append(",");
            }

            sb.append(field_name);
            sb.append(":");

            String field_str = field_type->to_string(compiler);
            sb.append(field_str);
        }

        sb.append(")");

        this->str = sb.build_null_terminated(compiler->arena);

        sb.destroy();
        break;
    }
    case TypeKind_Function: {
        StringBuilder sb =
            StringBuilder::create(MallocAllocator::get_instance());

        if (!this->func.vararg) {
            sb.append("@func(");
        } else {
            sb.append("@func_vararg(");
        }

        Type *return_type = &compiler->types[this->func.return_type.id];
        sb.append(return_type->to_string(compiler));

        sb.append(",");

        sb.append("(");

        for (size_t i = 0; i < this->func.param_types.len; ++i) {
            auto field_type_ref = this->func.param_types[i];
            Type *field_type = &compiler->types[field_type_ref.id];

            if (i > 0) {
                sb.append(",");
            }

            String field_str = field_type->to_string(compiler);
            sb.append(field_str);
        }

        sb.append(")");
        sb.append(")");

        this->str = sb.build_null_terminated(compiler->arena);

        sb.destroy();
        break;
    }
    }

    return str;
}

uint32_t Type::size_of(Compiler *compiler)
{
    uint32_t size = 0;

    switch (this->kind) {
    case TypeKind_Unknown:
    case TypeKind_MAX:
    case TypeKind_Void:
    case TypeKind_UntypedInt:
    case TypeKind_UntypedFloat:
    case TypeKind_Function:
    case TypeKind_Type: {
        size = 0;
        break;
    }
    case TypeKind_Int: {
        size = this->int_.bits >> 3;
        break;
    }
    case TypeKind_Float: {
        size = this->float_.bits >> 3;
        break;
    }
    case TypeKind_Array: {
        uint32_t subtype_size =
            this->array.sub_type.get(compiler).size_of(compiler);
        uint32_t subtype_alignment =
            this->array.sub_type.get(compiler).align_of(compiler);
        uint32_t stride = LANG_ROUND_UP(subtype_alignment, subtype_size);
        size = stride * this->array.size;
        break;
    }
    case TypeKind_Slice: {
        size = 16;
        break;
    }
    case TypeKind_Bool: {
        size = 1;
        break;
    }
    case TypeKind_Pointer: {
        size = 8;
        break;
    }
    case TypeKind_Distinct: {
        size = this->distinct.sub_type.get(compiler).size_of(compiler);
        break;
    }
    case TypeKind_Struct: {
        size = 0;
        for (TypeRef field_type_ref : this->struct_.field_types) {
            Type field_type = field_type_ref.get(compiler);
            uint32_t field_align = field_type.align_of(compiler);
            size = LANG_ROUND_UP(field_align, size); // Add padding

            uint32_t field_size = field_type.size_of(compiler);
            size += field_size;
        }
        break;
    }
    case TypeKind_Tuple: {
        size = 0;
        for (TypeRef field_type_ref : this->tuple.field_types) {
            Type field_type = field_type_ref.get(compiler);
            uint32_t field_align = field_type.align_of(compiler);
            size = LANG_ROUND_UP(field_align, size); // Add padding

            uint32_t field_size = field_type.size_of(compiler);
            size += field_size;
        }
        break;
    }
    }

    uint32_t self_alignment = this->align_of(compiler);
    size = LANG_ROUND_UP(self_alignment, size); // Round size up for alignment

    return size;
}

uint32_t Type::align_of(Compiler *compiler)
{
    uint32_t alignment = 0;

    switch (this->kind) {
    case TypeKind_Unknown:
    case TypeKind_MAX:
    case TypeKind_Void:
    case TypeKind_UntypedInt:
    case TypeKind_UntypedFloat:
    case TypeKind_Function:
    case TypeKind_Type: {
        alignment = 0;
        break;
    }
    case TypeKind_Int: {
        alignment = this->int_.bits >> 3;
        break;
    }
    case TypeKind_Float: {
        alignment = this->float_.bits >> 3;
        break;
    }
    case TypeKind_Array: {
        uint32_t subtype_alignment =
            this->array.sub_type.get(compiler).align_of(compiler);
        alignment = subtype_alignment;
        break;
    }
    case TypeKind_Slice: {
        alignment = 8;
        break;
    }
    case TypeKind_Bool: {
        alignment = 1;
        break;
    }
    case TypeKind_Pointer: {
        alignment = 8;
        break;
    }
    case TypeKind_Distinct: {
        alignment = this->distinct.sub_type.get(compiler).align_of(compiler);
        break;
    }
    case TypeKind_Struct: {
        alignment = 0;

        for (TypeRef field_type_ref : this->struct_.field_types) {
            Type field_type = field_type_ref.get(compiler);
            uint32_t field_align = field_type.align_of(compiler);
            if (field_align > alignment) {
                alignment = field_align;
            }
        }

        break;
    }
    case TypeKind_Tuple: {
        alignment = 0;

        for (TypeRef field_type_ref : this->tuple.field_types) {
            Type field_type = field_type_ref.get(compiler);
            uint32_t field_align = field_type.align_of(compiler);
            if (field_align > alignment) {
                alignment = field_align;
            }
        }

        break;
    }
    }

    return alignment;
}
