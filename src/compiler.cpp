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
    scope->decl_refs = SIRStringMapCreate(compiler->arena, 0);

    return scope;
}

void Scope::add(Compiler *compiler, DeclRef decl_ref)
{
    SIRString name = compiler->decls[decl_ref.id].name;
    if (!SIRStringEqual(name, "_")) {
        DeclRef found_decl = this->lookup(name);
        if (found_decl.id != 0) {
            const Location &loc = compiler->decls[decl_ref.id].loc;
            compiler->add_error(
                loc,
                "duplicate declaration of: '%.*s'",
                (int)name.len,
                name.ptr);
        } else {
            SIRStringMapSet(&this->decl_refs, name, decl_ref.id);
        }
    }
}

DeclRef Scope::lookup(const SIRString &name)
{
    if (SIRStringEqual(name, "_")) return {0};

    uintptr_t out_ref_id = 0;
    if (SIRStringMapGet(&this->decl_refs, name, &out_ref_id)) {
        return (DeclRef){(uint32_t)out_ref_id};
    }

    if (this->parent) {
        return this->parent->lookup(name);
    }

    return {0};
}

Compiler Compiler::create()
{
    init_parser_tables();

    SIRArenaAllocator *arena =
        SIRArenaAllocator::create(SIRMallocAllocator::get_instance());

    SIRStringMap keyword_map =
        SIRStringMapCreate(SIRMallocAllocator::get_instance(), 0);
    SIRStringMap builtin_function_map =
        SIRStringMapCreate(SIRMallocAllocator::get_instance(), 0);
    SIRArray<Error> errors = SIRArray<Error>::create(arena);

    SIRStringBuilder sb =
        SIRStringBuilder::create(SIRMallocAllocator::get_instance());

    SIRArray<File> files =
        SIRArray<File>::create(SIRMallocAllocator::get_instance());
    files.push_back({}); // 0th file

    SIRStringMap type_map =
        SIRStringMapCreate(SIRMallocAllocator::get_instance(), 0);
    SIRArray<Type> types =
        SIRArray<Type>::create(SIRMallocAllocator::get_instance());

    SIRArray<Decl> decls =
        SIRArray<Decl>::create(SIRMallocAllocator::get_instance());
    decls.push_back({}); // 0th decl

    SIRArray<Stmt> stmts =
        SIRArray<Stmt>::create(SIRMallocAllocator::get_instance());
    stmts.push_back({}); // 0th decl

    SIRArray<Expr> exprs =
        SIRArray<Expr>::create(SIRMallocAllocator::get_instance());
    exprs.push_back({}); // 0th expr

    SIRStringMapSet(&keyword_map, "extern", TokenKind_Extern);
    SIRStringMapSet(&keyword_map, "extern", TokenKind_Extern);
    SIRStringMapSet(&keyword_map, "vararg", TokenKind_VarArg);
    SIRStringMapSet(&keyword_map, "export", TokenKind_Export);
    SIRStringMapSet(&keyword_map, "inline", TokenKind_Inline);
    SIRStringMapSet(&keyword_map, "def", TokenKind_Def);
    SIRStringMapSet(&keyword_map, "type", TokenKind_Type);
    SIRStringMapSet(&keyword_map, "struct", TokenKind_Struct);
    SIRStringMapSet(&keyword_map, "global", TokenKind_Global);
    SIRStringMapSet(&keyword_map, "macro", TokenKind_Macro);
    SIRStringMapSet(&keyword_map, "null", TokenKind_Null);
    SIRStringMapSet(&keyword_map, "true", TokenKind_True);
    SIRStringMapSet(&keyword_map, "false", TokenKind_False);
    SIRStringMapSet(&keyword_map, "if", TokenKind_If);
    SIRStringMapSet(&keyword_map, "else", TokenKind_Else);
    SIRStringMapSet(&keyword_map, "while", TokenKind_While);
    SIRStringMapSet(&keyword_map, "break", TokenKind_Break);
    SIRStringMapSet(&keyword_map, "continue", TokenKind_Continue);
    SIRStringMapSet(&keyword_map, "return", TokenKind_Return);
    SIRStringMapSet(&keyword_map, "void", TokenKind_Void);
    SIRStringMapSet(&keyword_map, "bool", TokenKind_Bool);
    SIRStringMapSet(&keyword_map, "u8", TokenKind_U8);
    SIRStringMapSet(&keyword_map, "u16", TokenKind_U16);
    SIRStringMapSet(&keyword_map, "u32", TokenKind_U32);
    SIRStringMapSet(&keyword_map, "u64", TokenKind_U64);
    SIRStringMapSet(&keyword_map, "i8", TokenKind_I8);
    SIRStringMapSet(&keyword_map, "i16", TokenKind_I16);
    SIRStringMapSet(&keyword_map, "i32", TokenKind_I32);
    SIRStringMapSet(&keyword_map, "i64", TokenKind_I64);
    SIRStringMapSet(&keyword_map, "f32", TokenKind_F32);
    SIRStringMapSet(&keyword_map, "f64", TokenKind_F64);
    SIRStringMapSet(&keyword_map, "and", TokenKind_And);
    SIRStringMapSet(&keyword_map, "or", TokenKind_Or);

    SIRStringMapSet(&builtin_function_map, "sizeof", BuiltinFunction_Sizeof);
    SIRStringMapSet(&builtin_function_map, "alignof", BuiltinFunction_Alignof);
    SIRStringMapSet(&builtin_function_map, "ptrcast", BuiltinFunction_PtrCast);

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
    SIRStringMapDestroy(&this->type_map);
    this->types.destroy();
    this->files.destroy();

    this->sb.destroy();
    this->errors.destroy();
    SIRStringMapDestroy(&this->builtin_function_map);
    SIRStringMapDestroy(&this->keyword_map);
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
    SIRString message = this->arena->vsprintf(fmt, args);
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
    SIR_ASSERT(this->errors.len > 0);

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

static void
print_time_taken(const char *task_name, timespec start_time, timespec end_time)
{
    uint64_t secs = end_time.tv_sec - start_time.tv_sec;
    uint64_t nsecs = end_time.tv_nsec - start_time.tv_nsec;
    double time = (double)secs + ((double)nsecs / (double)1e9);

    printf("%s time: %.3lf seconds\n", task_name, time);
}

void Compiler::compile(SIRString path)
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

        SIRSlice<char> file_content = this->arena->alloc<char>(file_size + 1);
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
            .text = SIRString{file_content.ptr, file_content.len},
            .line_count = 0,
            .scope = Scope::create(this, file_ref),
            .top_level_decls = SIRArray<DeclRef>::create(this->arena),
        };

        timespec total_start_time, total_end_time;
        timespec start_time, end_time;

        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &total_start_time);

        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start_time);
        parse_file(this, file_ref);
        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end_time);
        print_time_taken("Parser", start_time, end_time);

        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start_time);
        analyze_file(this, file_ref);
        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end_time);
        print_time_taken("Analysis", start_time, end_time);

        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start_time);
        codegen_file(this, file_ref);
        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end_time);
        print_time_taken("Codegen", start_time, end_time);

        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &total_end_time);

        {
            File *file = &this->files[file_ref.id];

            uint64_t secs = total_end_time.tv_sec - total_start_time.tv_sec;
            uint64_t nsecs = total_end_time.tv_nsec - total_start_time.tv_nsec;
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
    SIRString type_string = type.to_string(this);
    uintptr_t existing_type_ref_id = 0;
    if (SIRStringMapGet(&this->type_map, type_string, &existing_type_ref_id)) {
        return (TypeRef){(uint32_t)existing_type_ref_id};
    }

    TypeRef type_ref = {(uint32_t)this->types.len};
    this->types.push_back(type);
    SIRStringMapSet(&this->type_map, type_string, type_ref.id);
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
    SIRSlice<TypeRef> fields, SIRSlice<SIRString> field_names)
{
    Type type = {};
    type.kind = TypeKind_Struct;
    type.struct_.field_types = fields;
    type.struct_.field_names = field_names;
    type.struct_.field_map = SIRStringMapCreate(this->arena, 32);
    for (size_t i = 0; i < field_names.len; ++i) {
        SIRStringMapSet(&type.struct_.field_map, field_names[i], i);
    }
    return this->get_cached_type(type);
}

TypeRef Compiler::create_tuple_type(SIRSlice<TypeRef> fields)
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
    TypeRef return_type, SIRSlice<TypeRef> param_types, bool vararg)
{
    Type type = {};
    type.kind = TypeKind_Function;
    type.func.return_type = return_type;
    type.func.param_types = param_types;
    type.func.vararg = vararg;
    return this->get_cached_type(type);
}

SIRString Type::to_string(Compiler *compiler)
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
        SIRString sub_str =
            compiler->types[this->pointer.sub_type.id].to_string(compiler);
        this->str = compiler->arena->sprintf(
            "@ptr(%.*s)", (int)sub_str.len, sub_str.ptr);
        break;
    }
    case TypeKind_Array: {
        SIRString sub_str =
            compiler->types[this->array.sub_type.id].to_string(compiler);
        this->str = compiler->arena->sprintf(
            "@arr(%.*s, %lu)", (int)sub_str.len, sub_str.ptr, this->array.size);
        break;
    }
    case TypeKind_Slice: {
        SIRString sub_str =
            compiler->types[this->slice.sub_type.id].to_string(compiler);
        this->str = compiler->arena->sprintf(
            "@slice(%.*s)", (int)sub_str.len, sub_str.ptr);
        break;
    }
    case TypeKind_Tuple: {
        SIRStringBuilder sb =
            SIRStringBuilder::create(SIRMallocAllocator::get_instance());

        sb.append("@tuple(");

        for (size_t i = 0; i < this->tuple.field_types.len; ++i) {
            auto field_type_ref = this->tuple.field_types[i];
            Type *field_type = &compiler->types[field_type_ref.id];
            if (i > 0) {
                sb.append(", ");
            }
            SIRString field_str = field_type->to_string(compiler);
            sb.append(field_str);
        }

        sb.append(")");

        this->str = sb.build_null_terminated(compiler->arena);

        sb.destroy();
        break;
    }
    case TypeKind_Struct: {
        SIRStringBuilder sb =
            SIRStringBuilder::create(SIRMallocAllocator::get_instance());

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

            SIRString field_str = field_type->to_string(compiler);
            sb.append(field_str);
        }

        sb.append(")");

        this->str = sb.build_null_terminated(compiler->arena);

        sb.destroy();
        break;
    }
    case TypeKind_Function: {
        SIRStringBuilder sb =
            SIRStringBuilder::create(SIRMallocAllocator::get_instance());

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

            SIRString field_str = field_type->to_string(compiler);
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
        uint32_t stride = SIR_ROUND_UP(subtype_alignment, subtype_size);
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
            size = SIR_ROUND_UP(field_align, size); // Add padding

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
            size = SIR_ROUND_UP(field_align, size); // Add padding

            uint32_t field_size = field_type.size_of(compiler);
            size += field_size;
        }
        break;
    }
    }

    uint32_t self_alignment = this->align_of(compiler);
    size = SIR_ROUND_UP(self_alignment, size); // Round size up for alignment

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
