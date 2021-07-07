#include "compiler.hpp"

#include <stdio.h>

Scope *Scope::create(Compiler *compiler, File *file, Scope *parent)
{
    Scope *scope = compiler->arena->alloc<Scope>();
    *scope = {};

    scope->file = file;
    scope->parent = parent;
    scope->decl_refs = ace::StringMap<DeclRef>::create(compiler->arena, 32);

    return scope;
}

void Scope::add(Compiler *compiler, DeclRef decl_ref)
{
    ace::String name = compiler->decls[decl_ref.id].name;
    if (name != "_") {
        if (this->lookup(name).id != 0) {
            const Location &loc = compiler->decls[decl_ref.id].loc;
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

DeclRef Scope::lookup(const ace::String &name)
{
    if (name == "_") return {0};

    DeclRef out_ref = {};
    if (this->decl_refs.get(name, &out_ref)) {
        return out_ref;
    }

    if (this->parent) {
        return this->parent->lookup(name);
    }

    return {0};
}

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

    ace::Array<Decl> decls =
        ace::Array<Decl>::create(ace::MallocAllocator::get_instance());
    decls.reserve(expected_decl_count);
    decls.push_back({}); // 0th decl

    ace::Array<Stmt> stmts =
        ace::Array<Stmt>::create(ace::MallocAllocator::get_instance());
    stmts.reserve(expected_stmt_count);
    stmts.push_back({}); // 0th decl

    ace::Array<Expr> exprs =
        ace::Array<Expr>::create(ace::MallocAllocator::get_instance());
    exprs.reserve(expected_expr_count);
    exprs.push_back({}); // 0th expr

    keyword_map.set("extern", TokenKind_Extern);
    keyword_map.set("export", TokenKind_Export);
    keyword_map.set("inline", TokenKind_Inline);
    keyword_map.set("def", TokenKind_Def);
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
    ACE_ASSERT(this->errors.len > 0);

    for (Error &err : this->errors) {
        if (err.loc.file) {
            fprintf(
                stderr,
                "error: %.*s:%u:%u: %.*s\n",
                (int)err.loc.file->path.len,
                err.loc.file->path.ptr,
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
            .scope = Scope::create(this, file),
            .top_level_decls = ace::Array<DeclRef>::create(this->arena),
        };

        parse_file(this, file);
        analyze_file(this, file);
        codegen_file(this, file);
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
    ace::String type_string = type.to_string(this);
    TypeRef existing_type_ref = {0};
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

TypeRef Compiler::create_struct_type(
    ace::Slice<TypeRef> fields, ace::Slice<ace::String> field_names)
{
    Type type = {};
    type.kind = TypeKind_Struct;
    type.struct_.field_types = fields;
    type.struct_.field_names = field_names;
    return this->get_cached_type(type);
}

TypeRef Compiler::create_tuple_type(ace::Slice<TypeRef> fields)
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

TypeRef
Compiler::create_func_type(TypeRef return_type, ace::Slice<TypeRef> param_types)
{
    Type type = {};
    type.kind = TypeKind_Function;
    type.func.return_type = return_type;
    type.func.param_types = param_types;
    return this->get_cached_type(type);
}

ace::String Type::to_string(Compiler *compiler)
{
    if (this->str.len > 0) {
        return this->str;
    }

    switch (this->kind) {
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
        if (this->int_.is_signed) {
            this->str = compiler->arena->sprintf("@int(%u)", this->int_.bits);
        } else {
            this->str = compiler->arena->sprintf("@uint(%u)", this->int_.bits);
        }
        break;
    }
    case TypeKind_Float: {
        this->str = compiler->arena->sprintf("@float(%u)", this->float_.bits);
        break;
    }
    case TypeKind_Pointer: {
        ace::String sub_str =
            compiler->types[this->pointer.sub_type.id].to_string(compiler);
        this->str = compiler->arena->sprintf(
            "@ptr(%.*s)", (int)sub_str.len, sub_str.ptr);
        break;
    }
    case TypeKind_Array: {
        ace::String sub_str =
            compiler->types[this->array.sub_type.id].to_string(compiler);
        this->str = compiler->arena->sprintf(
            "@arr(%.*s, %lu)", (int)sub_str.len, sub_str.ptr, this->array.size);
        break;
    }
    case TypeKind_Slice: {
        ace::String sub_str =
            compiler->types[this->slice.sub_type.id].to_string(compiler);
        this->str = compiler->arena->sprintf(
            "@slice(%.*s)", (int)sub_str.len, sub_str.ptr);
        break;
    }
    case TypeKind_Tuple: {
        ace::StringBuilder sb =
            ace::StringBuilder::create(ace::MallocAllocator::get_instance());

        sb.append("@tuple(");

        for (size_t i = 0; i < this->tuple.field_types.len; ++i) {
            auto field_type_ref = this->tuple.field_types[i];
            Type *field_type = &compiler->types[field_type_ref.id];
            if (i > 0) {
                sb.append(", ");
            }
            ace::String field_str = field_type->to_string(compiler);
            sb.append(field_str);
        }

        sb.append(")");

        this->str = sb.build(compiler->arena);

        sb.destroy();
        break;
    }
    case TypeKind_Struct: {
        ace::StringBuilder sb =
            ace::StringBuilder::create(ace::MallocAllocator::get_instance());

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

            ace::String field_str = field_type->to_string(compiler);
            sb.append(field_str);
        }

        sb.append(")");

        this->str = sb.build(compiler->arena);

        sb.destroy();
        break;
    }
    case TypeKind_Function: {
        ace::StringBuilder sb =
            ace::StringBuilder::create(ace::MallocAllocator::get_instance());

        sb.append("@func(");

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

            ace::String field_str = field_type->to_string(compiler);
            sb.append(field_str);
        }

        sb.append(")");
        sb.append(")");

        this->str = sb.build(compiler->arena);

        sb.destroy();
        break;
    }
    }

    return str;
}

uint32_t Type::align_of(Compiler *compiler)
{
    (void)compiler;
    return 0;
}

uint32_t Type::size_of(Compiler *compiler)
{
    (void)compiler;
    return 0;
}
