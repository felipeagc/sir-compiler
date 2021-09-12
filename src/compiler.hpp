#pragma once

#include "base.hpp"
#include <sir.h>
#include <sir_interp.h>

struct Compiler;
struct File;
struct CodegenContext;

struct Expr;
struct Stmt;
struct Decl;

struct Type;

struct FileRef {
    uint32_t id;
};

struct DeclRef {
    uint32_t id;

    operator uint32_t()
    {
        return this->id;
    }
    Decl get(Compiler *compiler) const;
};

struct StmtRef {
    uint32_t id;

    operator uint32_t()
    {
        return this->id;
    }
    Stmt get(Compiler *compiler) const;
};

struct ExprRef {
    uint32_t id;

    operator uint32_t()
    {
        return this->id;
    }
    Expr get(Compiler *compiler) const;
    bool is_lvalue(Compiler *compiler);
};

struct TypeRef {
    uint32_t id;

    operator uint32_t()
    {
        return this->id;
    }
    Type get(Compiler *compiler) const;
    TypeRef inner(Compiler *compiler) const;
};

struct Scope {
    FileRef file_ref;
    Scope *parent;
    StringMap<DeclRef> decl_refs;

    static Scope *
    create(Compiler *compiler, FileRef file_ref, Scope *parent = nullptr);
    void add(Compiler *compiler, DeclRef decl_ref);
    DeclRef lookup(const String &name);
};

struct File {
    String path;
    String text;
    size_t line_count;

    Scope *scope;
    Array<DeclRef> top_level_decls;
};

struct Location {
    FileRef file_ref;
    uint32_t offset;
    uint32_t len;
    uint32_t line;
    uint32_t col;
};

struct Error {
    Location loc;
    String message;
};

enum TokenKind {
    TokenKind_Unknown = 0,
    TokenKind_Error,

    TokenKind_LParen,
    TokenKind_RParen,
    TokenKind_LBracket,
    TokenKind_RBracket,
    TokenKind_LCurly,
    TokenKind_RCurly,

    TokenKind_Colon,
    TokenKind_Comma,
    TokenKind_Dot,
    TokenKind_Semicolon,
    TokenKind_Question,

    TokenKind_Equal,

    TokenKind_Add,
    TokenKind_Sub,
    TokenKind_Mul,
    TokenKind_Div,
    TokenKind_Mod,
    TokenKind_Arrow,

    TokenKind_Not,
    TokenKind_BitAnd,
    TokenKind_BitOr,
    TokenKind_BitXor,
    TokenKind_BitNot,

    TokenKind_EqualEqual,
    TokenKind_NotEqual,
    TokenKind_Less,
    TokenKind_LessEqual,
    TokenKind_Greater,
    TokenKind_GreaterEqual,

    TokenKind_LShift,
    TokenKind_RShift,

    TokenKind_AddEqual,
    TokenKind_SubEqual,
    TokenKind_MulEqual,
    TokenKind_DivEqual,
    TokenKind_ModEqual,

    TokenKind_BitAndEqual,
    TokenKind_BitOrEqual,
    TokenKind_BitXorEqual,
    TokenKind_BitNotEqual,
    TokenKind_LShiftEqual,
    TokenKind_RShiftEqual,

    TokenKind_Global,
    TokenKind_Const,
    TokenKind_Var,
    TokenKind_Val,
    TokenKind_Extern,
    TokenKind_Export,
    TokenKind_Inline,
    TokenKind_Distinct,
    TokenKind_VarArg,
    TokenKind_Fn,
    TokenKind_Macro,
    TokenKind_Type,
    TokenKind_Struct,
    TokenKind_Union,
    TokenKind_If,
    TokenKind_Else,
    TokenKind_While,
    TokenKind_ComptimeIf,
    TokenKind_Break,
    TokenKind_Continue,
    TokenKind_Return,
    TokenKind_And,
    TokenKind_Or,
    TokenKind_Void,
    TokenKind_Bool,
    TokenKind_True,
    TokenKind_False,
    TokenKind_Null,
    TokenKind_Undefined,
    TokenKind_U8,
    TokenKind_U16,
    TokenKind_U32,
    TokenKind_U64,
    TokenKind_I8,
    TokenKind_I16,
    TokenKind_I32,
    TokenKind_I64,
    TokenKind_F32,
    TokenKind_F64,
    TokenKind_USize,
    TokenKind_ISize,
    TokenKind_Identifier,
    TokenKind_BuiltinIdentifier,
    TokenKind_StringLiteral,
    TokenKind_CharLiteral,
    TokenKind_IntLiteral,
    TokenKind_FloatLiteral,

    TokenKind_EOF,
};

struct Token {
    TokenKind kind;
    Location loc;
    union {
        String str;
        uint64_t u64;
        double f64;
    };
};

enum BuiltinFunction {
    BuiltinFunction_Unknown = 0,
    BuiltinFunction_Sizeof,
    BuiltinFunction_Alignof,
    BuiltinFunction_BitCast,
    BuiltinFunction_Defined,
};

enum TypeKind {
    TypeKind_Unknown = 0,

    TypeKind_Void,
    TypeKind_Type,
    TypeKind_Bool,
    TypeKind_UntypedInt,
    TypeKind_UntypedFloat,
    TypeKind_Distinct,
    TypeKind_Int,
    TypeKind_Float,
    TypeKind_Struct,
    TypeKind_Tuple,
    TypeKind_Pointer,
    TypeKind_Array,
    TypeKind_Slice,
    TypeKind_Function,

    TypeKind_MAX,
};

struct StructType {
    Slice<TypeRef> field_types;
    Slice<String> field_names;
    StringMap<uint32_t> field_map;
    String display_name;
};

struct Type {
    TypeKind kind;
    String str;
    union {
        struct {
            uint32_t bits;
            bool is_signed;
            bool is_size;
        } int_;
        struct {
            uint32_t bits;
        } float_;
        StructType *struct_;
        struct {
            Slice<TypeRef> field_types;
        } tuple;
        struct {
            TypeRef sub_type;
        } pointer;
        struct {
            TypeRef sub_type;
            String display_name;
        } distinct;
        struct {
            TypeRef sub_type;
            uint64_t size;
        } array;
        struct {
            TypeRef sub_type;
        } slice;
        struct {
            TypeRef return_type;
            Slice<TypeRef> param_types;
            bool vararg;
        } func;
    };

    String to_internal_string(Compiler *compiler);
    String to_pretty_string(Compiler *compiler);
    uint32_t align_of(Compiler *compiler);
    uint32_t size_of(Compiler *compiler);

    inline bool is_numeric()
    {
        switch (this->kind) {
        case TypeKind_UntypedInt:
        case TypeKind_Int:
        case TypeKind_UntypedFloat:
        case TypeKind_Float: return true;
        default: return false;
        }
        return false;
    }

    inline bool is_int()
    {
        switch (this->kind) {
        case TypeKind_UntypedInt:
        case TypeKind_Int: return true;
        default: return false;
        }
        return false;
    }

    inline bool is_runtime_int()
    {
        return this->kind == TypeKind_Int;
    }

    inline bool is_runtime_numeric()
    {
        switch (this->kind) {
        case TypeKind_Int:
        case TypeKind_Float: return true;
        default: return false;
        }
        return false;
    }

    inline bool is_runtime_float()
    {
        return this->kind == TypeKind_Float;
    }

    inline bool is_runtime()
    {
        switch (this->kind) {
        case TypeKind_Unknown:
        case TypeKind_UntypedFloat:
        case TypeKind_UntypedInt:
        case TypeKind_Function:
        case TypeKind_Type: return false;
        default: return true;
        }
        return true;
    }
};

struct InterpValue {
    TypeRef type_ref;
    union {
        bool boolean;
        uint64_t u64;
        int64_t i64;
        double f64;
    };
};

enum FunctionFlags {
    FunctionFlags_Inline = 1 << 0,
    FunctionFlags_Extern = 1 << 1,
    FunctionFlags_Exported = 1 << 2,
    FunctionFlags_VarArg = 1 << 3,
};

enum UnaryOp {
    UnaryOp_Unknown = 0,

    UnaryOp_AddressOf,
    UnaryOp_Dereference,
    UnaryOp_Not,
    UnaryOp_BitNot,
    UnaryOp_Negate,
};

enum BinaryOp {
    BinaryOp_Unknown = 0,

    BinaryOp_Add,
    BinaryOp_Sub,
    BinaryOp_Mul,
    BinaryOp_Div,
    BinaryOp_Mod,
    BinaryOp_BitAnd,
    BinaryOp_BitOr,
    BinaryOp_BitXor,
    BinaryOp_LShift,
    BinaryOp_RShift,
    BinaryOp_Equal,
    BinaryOp_NotEqual,
    BinaryOp_Less,
    BinaryOp_LessEqual,
    BinaryOp_Greater,
    BinaryOp_GreaterEqual,
    BinaryOp_And,
    BinaryOp_Or,
    BinaryOp_MAX,
};

enum ExprKind : uint8_t {
    ExprKind_Unknown = 0,
    ExprKind_Identifier,
    ExprKind_StringLiteral,
    ExprKind_IntLiteral,
    ExprKind_FloatLiteral,
    ExprKind_BoolLiteral,
    ExprKind_VoidLiteral,
    ExprKind_Function,
    ExprKind_FunctionCall,
    ExprKind_NullLiteral,
    ExprKind_UndefinedLiteral,
    ExprKind_PointerType,
    ExprKind_DistinctType,
    ExprKind_VoidType,
    ExprKind_BoolType,
    ExprKind_IntType,
    ExprKind_USizeType,
    ExprKind_ISizeType,
    ExprKind_FloatType,
    ExprKind_SliceType,
    ExprKind_ArrayType,
    ExprKind_StructType,
    ExprKind_Subscript,
    ExprKind_Access,
    ExprKind_BuiltinCall,
    ExprKind_Unary,
    ExprKind_Binary,
};

struct Expr {
    union {
        struct {
            String str;
            DeclRef decl_ref;
        } ident;
        struct {
            String str;
        } str_literal;
        struct {
            uint64_t u64;
        } int_literal;
        struct {
            double f64;
        } float_literal;
        struct {
            bool bool_;
        } bool_literal;
        struct {
            ExprRef func_expr_ref;
            Array<ExprRef> param_refs;
        } func_call;
        struct {
            BuiltinFunction builtin;
            Array<ExprRef> param_refs;
        } builtin_call;
        struct {
            ExprRef sub_expr_ref;
        } ptr_type;
        struct {
            ExprRef sub_expr_ref;
        } distinct_type;
        struct {
            uint32_t bits;
            bool is_signed;
        } int_type;
        struct {
            uint32_t bits;
        } float_type;
        struct {
            ExprRef subtype_expr_ref;
        } slice_type;
        struct {
            ExprRef subtype_expr_ref;
            ExprRef size_expr_ref;
        } array_type;
        struct {
            Array<String> field_names;
            Array<ExprRef> field_type_expr_refs;
        } struct_type;
        struct {
            ExprRef left_ref;
            ExprRef right_ref;
        } subscript;
        struct {
            ExprRef left_ref;
            ExprRef accessed_ident_ref;
        } access;
        struct {
            ExprRef left_ref;
            ExprRef right_ref;
            BinaryOp op;
        } binary;
        struct {
            ExprRef left_ref;
            UnaryOp op;
        } unary;
    };
    ExprKind kind;
};

enum StmtKind : uint8_t {
    StmtKind_Unknown = 0,
    StmtKind_Block,
    StmtKind_Expr,
    StmtKind_Decl,
    StmtKind_ComptimeIf,
    StmtKind_If,
    StmtKind_While,
    StmtKind_Return,
    StmtKind_Assign,
};

struct Stmt {
    union {
        struct {
            Array<StmtRef> stmt_refs;
        } block;
        struct {
            ExprRef expr_ref;
        } expr;
        struct {
            DeclRef decl_ref;
        } decl;
        struct {
            ExprRef returned_expr_ref;
        } return_;
        struct {
            ExprRef cond_expr_ref;
            StmtRef true_stmt_ref;
            StmtRef false_stmt_ref;
            bool cond_value;
            bool has_evaluated;
        } comptime_if;
        struct {
            ExprRef cond_expr_ref;
            StmtRef true_stmt_ref;
            StmtRef false_stmt_ref;
        } if_;
        struct {
            ExprRef cond_expr_ref;
            StmtRef true_stmt_ref;
        } while_;
        struct {
            ExprRef assigned_expr_ref;
            ExprRef value_expr_ref;
        } assign;
    };
    StmtKind kind;
};

enum DeclKind : uint8_t {
    DeclKind_Unknown = 0,
    DeclKind_Type,
    DeclKind_Function,
    DeclKind_FunctionParameter,
    DeclKind_LocalVarDecl,
    DeclKind_ImmutableLocalVarDecl,
    DeclKind_GlobalVarDecl,
    DeclKind_ConstDecl,
    DeclKind_ComptimeIf,
};

struct FuncDecl {
    Scope *scope;
    uint32_t flags;
    Array<ExprRef> return_type_expr_refs;
    Array<DeclRef> param_decl_refs;
    Array<StmtRef> body_stmts;
};

struct Decl {
    union {
        FuncDecl *func;
        struct {
            ExprRef type_expr;
        } func_param;
        struct {
            ExprRef type_expr;
            ExprRef value_expr;
        } var_decl;
        struct {
            ExprRef type_expr;
        } type_decl;
        struct {
            ExprRef cond_expr_ref;
            Array<DeclRef> true_decls;
            Array<DeclRef> false_decls;
            bool cond_value;
            bool has_evaluated;
        } comptime_if;
    };
    DeclKind kind;
};

struct Compiler {
    ArenaAllocator *arena;
    StringMap<TokenKind> keyword_map;
    StringMap<BuiltinFunction> builtin_function_map;
    Array<Error> errors;
    StringBuilder sb;

    StringMap<bool> defines;
    Array<File> files;
    StringMap<TypeRef> type_map;
    StringMap<TypeRef> named_type_map;
    Array<Type> types;
    Array<Decl> decls;
    Array<Stmt> stmts;
    Array<Expr> exprs;
    Array<Location> expr_locs;
    Array<Location> decl_locs;
    Array<Location> stmt_locs;
    Array<String> decl_names;
    Array<TypeRef> decl_types;
    Array<TypeRef> decl_as_types;
    Array<TypeRef> expr_types;
    Array<TypeRef> expr_as_types;

    TypeRef void_type;
    TypeRef type_type;
    TypeRef bool_type;
    TypeRef untyped_int_type;
    TypeRef untyped_float_type;
    TypeRef u8_type;
    TypeRef u16_type;
    TypeRef u32_type;
    TypeRef u64_type;
    TypeRef i8_type;
    TypeRef i16_type;
    TypeRef i32_type;
    TypeRef i64_type;
    TypeRef f32_type;
    TypeRef f64_type;
    TypeRef usize_type;
    TypeRef isize_type;

    static Compiler create();
    void destroy();

    TypeRef get_cached_type(Type &type);
    TypeRef create_pointer_type(TypeRef sub);
    TypeRef create_distinct_type(const String &name);
    void set_distinct_type_alias(TypeRef distinct_type, TypeRef sub);
    TypeRef
    create_struct_type(Slice<TypeRef> fields, Slice<String> field_names);
    TypeRef create_named_struct_type(const String &name);
    void set_named_struct_body(
        TypeRef struct_type, Slice<TypeRef> fields, Slice<String> field_names);
    TypeRef create_tuple_type(Slice<TypeRef> fields);
    TypeRef create_array_type(TypeRef sub, size_t size);
    TypeRef create_slice_type(TypeRef sub);
    TypeRef create_func_type(
        TypeRef return_type, Slice<TypeRef> param_types, bool vararg);

    size_t get_error_checkpoint();
    void restore_error_checkpoint(size_t checkpoint);
    LANG_PRINTF_FORMATTING(3, 4)
    void add_error(const Location &loc, const char *fmt, ...);
    void halt_compilation();
    void print_errors();

    LANG_INLINE
    FileRef add_file(const File &file)
    {
        FileRef ref = {(uint32_t)this->files.len};
        this->files.push_back(file);
        return ref;
    }

    LANG_INLINE
    ExprRef add_expr(const Location &loc, const Expr &expr)
    {
        LANG_ASSERT(expr.kind != ExprKind_Unknown);
        ExprRef ref = {(uint32_t)this->exprs.len};
        this->exprs.push_back(expr);
        this->expr_locs.push_back(loc);
        this->expr_types.push_back({0});
        this->expr_as_types.push_back({0});
        return ref;
    }

    LANG_INLINE
    StmtRef add_stmt(const Location &loc, const Stmt &stmt)
    {
        LANG_ASSERT(stmt.kind != StmtKind_Unknown);
        StmtRef ref = {(uint32_t)this->stmts.len};
        this->stmts.push_back(stmt);
        this->stmt_locs.push_back(loc);
        return ref;
    }

    LANG_INLINE
    DeclRef add_decl(const String &name, const Location &loc, const Decl &decl)
    {
        LANG_ASSERT(decl.kind != DeclKind_Unknown);
        DeclRef ref = {(uint32_t)this->decls.len};
        this->decls.push_back(decl);
        this->decl_types.push_back({0});
        this->decl_as_types.push_back({0});
        this->decl_names.push_back(name);
        this->decl_locs.push_back(loc);
        return ref;
    }

    LANG_INLINE
    Expr *get_expr(ExprRef ref)
    {
        return &this->exprs[ref.id];
    }

    LANG_INLINE
    Stmt *get_stmt(StmtRef ref)
    {
        return &this->stmts[ref.id];
    }

    LANG_INLINE
    Decl *get_decl(DeclRef ref)
    {
        return &this->decls[ref.id];
    }

    void compile(String path);
};

void init_parser_tables();

CodegenContext *CodegenContextCreate();
void CodegenContextDestroy(CodegenContext *ctx);

void parse_file(Compiler *compiler, FileRef file_ref);
void analyze_file(Compiler *compiler, FileRef file_ref);
void codegen_file(Compiler *compiler, CodegenContext *ctx, FileRef file_ref);
void *codegen_interp_expr(
    Compiler *compiler,
    CodegenContext *ctx,
    ExprRef expr_ref,
    SIRInterpResult *err_code,
    size_t *out_size);

inline Decl DeclRef::get(Compiler *compiler) const
{
    return compiler->decls[this->id];
}

inline Stmt StmtRef::get(Compiler *compiler) const
{
    return compiler->stmts[this->id];
}

inline Expr ExprRef::get(Compiler *compiler) const
{
    return compiler->exprs[this->id];
}

inline Type TypeRef::get(Compiler *compiler) const
{
    return compiler->types[this->id];
}

inline TypeRef TypeRef::inner(Compiler *compiler) const
{
    const Type &this_type = this->get(compiler);
    switch (this_type.kind) {
    case TypeKind_Distinct: return this_type.distinct.sub_type.inner(compiler);
    default: return *this;
    }
    return *this;
}
