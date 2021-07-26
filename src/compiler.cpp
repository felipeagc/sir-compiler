#include "compiler.hpp"

#include <stdio.h>
#include <time.h>

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

bool TypeRef::is_runtime(Compiler *compiler)
{
    bool is_runtime = true;

    Type type = this->get(compiler);
    switch (type.kind) {
    case TypeKind_Unknown:
    case TypeKind_UntypedFloat:
    case TypeKind_UntypedInt:
    case TypeKind_Function:
    case TypeKind_Type: {
        is_runtime = false;
        break;
    }
    default: break;
    }

    return is_runtime;
}

Scope *Scope::create(Compiler *compiler, FileRef file_ref, Scope *parent)
{
    Scope *scope = compiler->arena->alloc<Scope>();
    *scope = {};

    scope->file_ref = file_ref;
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

    ace::StringBuilder sb =
        ace::StringBuilder::create(ace::MallocAllocator::get_instance());

    ace::Array<File> files =
        ace::Array<File>::create(ace::MallocAllocator::get_instance());
    files.reserve(64);
    files.push_back({}); // 0th file

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
    keyword_map.set("vararg", TokenKind_VarArg);
    keyword_map.set("export", TokenKind_Export);
    keyword_map.set("inline", TokenKind_Inline);
    keyword_map.set("def", TokenKind_Def);
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

    builtin_function_map.set("sizeof", BuiltinFunction_Sizeof);
    builtin_function_map.set("alignof", BuiltinFunction_Alignof);
    builtin_function_map.set("ptrcast", BuiltinFunction_PtrCast);

    Compiler compiler = {
        .arena = arena,
        .keyword_map = keyword_map,
        .builtin_function_map = builtin_function_map,
        .errors = errors,
        .sb = sb,

        .files = files,
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

        FileRef file_ref = this->add_file({});
        this->files[file_ref.id] = {
            .path = path,
            .text = ace::String{file_content.ptr, file_content.len},
            .line_count = 0,
            .scope = Scope::create(this, file_ref),
            .top_level_decls = ace::Array<DeclRef>::create(this->arena),
        };

        timespec start_time, end_time;
        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start_time);

        parse_file(this, file_ref);
        analyze_file(this, file_ref);
        codegen_file(this, file_ref);

        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end_time);

        {
            File *file = &this->files[file_ref.id];

            uint64_t secs = end_time.tv_sec - start_time.tv_sec;
            uint64_t nsecs = end_time.tv_nsec - start_time.tv_nsec;
            double time = (double)secs + ((double)nsecs / (double)1e9);
            double total_line_count = (double)file->line_count;

            printf("Compilation time: %.3lf seconds\n", time);
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
    type.struct_.field_map = ace::StringMap<uint32_t>::create(this->arena, 32);
    for (size_t i = 0; i < field_names.len; ++i) {
        type.struct_.field_map.set(field_names[i], (uint32_t)i);
    }
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

TypeRef Compiler::create_func_type(
    TypeRef return_type, ace::Slice<TypeRef> param_types, bool vararg)
{
    Type type = {};
    type.kind = TypeKind_Function;
    type.func.return_type = return_type;
    type.func.param_types = param_types;
    type.func.vararg = vararg;
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

        this->str = sb.build_null_terminated(compiler->arena);

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

        this->str = sb.build_null_terminated(compiler->arena);

        sb.destroy();
        break;
    }
    case TypeKind_Function: {
        ace::StringBuilder sb =
            ace::StringBuilder::create(ace::MallocAllocator::get_instance());

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

            ace::String field_str = field_type->to_string(compiler);
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
        uint32_t stride = ACE_ROUND_UP(subtype_alignment, subtype_size);
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
    case TypeKind_Struct: {
        size = 0;
        for (TypeRef field_type_ref : this->struct_.field_types) {
            Type field_type = field_type_ref.get(compiler);
            uint32_t field_align = field_type.align_of(compiler);
            size = ACE_ROUND_UP(field_align, size); // Add padding

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
            size = ACE_ROUND_UP(field_align, size); // Add padding

            uint32_t field_size = field_type.size_of(compiler);
            size += field_size;
        }
        break;
    }
    }

    uint32_t self_alignment = this->align_of(compiler);
    size = ACE_ROUND_UP(self_alignment, size); // Round size up for alignment

    return size;
}

uint32_t Type::align_of(Compiler *compiler)
{
    uint32_t alignment = 0;

    switch (this->kind) {
    case TypeKind_Unknown:
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
