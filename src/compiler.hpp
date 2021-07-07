#pragma once

#include <ace_base.hpp>

struct Compiler;
struct File;

struct Expr;
struct Stmt;
struct Decl;

struct Type;

struct DeclRef {
    uint32_t id;

    Decl get(Compiler *compiler);
};

struct StmtRef {
    uint32_t id;

    Stmt get(Compiler *compiler);
};

struct ExprRef {
    uint32_t id;

    Expr get(Compiler *compiler);
};

struct TypeRef {
    uint32_t id;

    Type get(Compiler *compiler);
};

struct Scope {
    File *file;
    Scope *parent;
    ace::StringMap<DeclRef> decl_refs;

    static Scope *
    create(Compiler *compiler, File *file, Scope *parent = nullptr);
    void add(Compiler *compiler, DeclRef decl_ref);
    DeclRef lookup(const ace::String &name);
};

struct File {
    ace::String path;
    ace::String text;

    Scope *scope;
    ace::Array<DeclRef> top_level_decls;
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
    TokenKind_ColonColon,
    TokenKind_Comma,
    TokenKind_Underscore,
    TokenKind_Dot,
    TokenKind_Semicolon,
    TokenKind_Question,

    TokenKind_Equal,

    TokenKind_And,
    TokenKind_Or,

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
    TokenKind_Extern,
    TokenKind_Inline,
    TokenKind_Def,
    TokenKind_Macro,
    TokenKind_Struct,
    TokenKind_Union,
    TokenKind_If,
    TokenKind_Else,
    TokenKind_While,
    TokenKind_Break,
    TokenKind_Continue,
    TokenKind_Return,
    TokenKind_Void,
    TokenKind_Bool,
    TokenKind_True,
    TokenKind_False,
    TokenKind_Null,
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
        ace::String str;
        int64_t int_;
        double float_;
    };
};

enum BuiltinFunction {

};

enum TypeKind {
    TypeKind_Unknown = 0,
    TypeKind_Void,
    TypeKind_Type,
    TypeKind_Bool,
    TypeKind_UntypedInt,
    TypeKind_UntypedFloat,
    TypeKind_Int,
    TypeKind_Float,
    TypeKind_Struct,
    TypeKind_Tuple,
    TypeKind_Pointer,
    TypeKind_Array,
    TypeKind_Slice,
    TypeKind_Function,
};

struct Type {
    TypeKind kind;
    ace::String str;
    union {
        struct {
            uint32_t bits;
            bool is_signed;
        } int_;
        struct {
            uint32_t bits;
        } float_;
        struct {
            ace::Slice<TypeRef> field_types;
            ace::Slice<ace::String> field_names;
        } struct_;
        struct {
            ace::Slice<TypeRef> field_types;
        } tuple;
        struct {
            TypeRef sub_type;
        } pointer;
        struct {
            TypeRef sub_type;
            uint64_t size;
        } array;
        struct {
            TypeRef sub_type;
        } slice;
        struct {
            TypeRef return_type;
            ace::Slice<TypeRef> param_types;
        } func;
    };

    ace::String to_string(Compiler *compiler);
    uint32_t align_of(Compiler *compiler);
    uint32_t size_of(Compiler *compiler);
};

struct InterpValue {
    TypeRef type_ref;
    union {
        uint8_t u8;
        uint16_t u16;
        uint32_t u32;
        uint64_t u64;
        int8_t i8;
        int16_t i16;
        int32_t i32;
        int64_t i64;
        float f32;
        double f64;
    };
};

enum FunctionFlags {
    FunctionFlags_Inline = 1 << 0,
    FunctionFlags_Extern = 1 << 1,
};

enum UnaryOp {
    UnaryOp_Unknown,

    UnaryOp_Dereference,
    UnaryOp_Not,
    UnaryOp_Negate,
};

enum BinaryOp {
    BinaryOp_Unknown,
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
    BinaryOp_MAX,
};

enum ExprKind {
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
    ExprKind_PointerType,
    ExprKind_VoidType,
    ExprKind_BoolType,
    ExprKind_IntType,
    ExprKind_FloatType,
    ExprKind_SliceType,
    ExprKind_ArrayType,
    ExprKind_Unary,
    ExprKind_Binary,
};

struct Expr {
    ExprKind kind;
    TypeRef expr_type_ref;
    TypeRef as_type_ref;
    Location loc;
    union {
        struct {
            ace::String str;
        } ident;
        struct {
            ace::String str;
        } str_literal;
        struct {
            int64_t i64;
        } int_literal;
        struct {
            double f64;
        } float_literal;
        struct {
            bool bool_;
        } bool_literal;
        struct {
            uint32_t flags;
            ace::Array<ExprRef> return_type_expr_refs;
            ace::Array<DeclRef> param_decl_refs;
            ace::Array<StmtRef> body_stmts;
        } func;
        struct {
            ExprRef func_expr_ref;
            ace::Array<ExprRef> param_refs;
        } func_call;
        struct {
            ExprRef sub_expr_ref;
        } ptr_type;
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
            ExprRef left_ref;
            ExprRef right_ref;
            BinaryOp op;
        } binary;
        struct {
            ExprRef left_ref;
            UnaryOp op;
        } unary;
    };
};

enum StmtKind {
    StmtKind_Unknown = 0,
    StmtKind_Block,
    StmtKind_Expr,
    StmtKind_If,
    StmtKind_While,
    StmtKind_Return,
};

struct Stmt {
    StmtKind kind;
    Location loc;
    union {
        struct {
            ace::Array<StmtRef> stmt_refs;
        } block;
        struct {
            ExprRef expr_ref;
        } expr;
        struct {
            ExprRef returned_expr_ref;
        } return_;
        struct {
            ExprRef cond_expr_ref;
            StmtRef true_stmt_ref;
            StmtRef false_stmt_ref;
        } if_;
    };
};

enum DeclKind {
    DeclKind_Unknown = 0,
    DeclKind_Function,
    DeclKind_FunctionParameter,
    DeclKind_LocalVarDecl,
    DeclKind_GlobalVarDecl,
    DeclKind_ConstDecl,
};

struct Decl {
    DeclKind kind;
    TypeRef decl_type_ref;
    TypeRef as_type_ref;
    Location loc;
    ace::String name;
    union {
        struct {
            Scope *scope;
            uint32_t flags;
            ace::Array<ExprRef> return_type_expr_refs;
            ace::Array<DeclRef> param_decl_refs;
            ace::Array<StmtRef> body_stmts;
        } func;
        struct {
            ExprRef type_expr;
        } func_param;
        struct {
            ExprRef type_expr;
            ExprRef value_expr;
        } local_var_decl;
        struct {
            ExprRef type_expr;
            ExprRef value_expr;
        } global_var_decl;
        struct {
            ExprRef type_expr;
            ExprRef value_expr;
        } const_decl;
    };
};

struct Compiler {
    ace::ArenaAllocator *arena;
    ace::StringMap<TokenKind> keyword_map;
    ace::StringMap<BuiltinFunction> builtin_function_map;
    ace::Array<Error> errors;

    ace::StringMap<TypeRef> type_map;
    ace::Array<Type> types;
    ace::Array<Decl> decls;
    ace::Array<Stmt> stmts;
    ace::Array<Expr> exprs;

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

    static Compiler create();
    void destroy();

    TypeRef get_cached_type(Type &type);
    TypeRef create_pointer_type(TypeRef sub);
    TypeRef create_struct_type(
        ace::Slice<TypeRef> fields, ace::Slice<ace::String> field_names);
    TypeRef create_tuple_type(ace::Slice<TypeRef> fields);
    TypeRef create_array_type(TypeRef sub, size_t size);
    TypeRef create_slice_type(TypeRef sub);
    TypeRef
    create_func_type(TypeRef return_type, ace::Slice<TypeRef> param_types);

    ACE_PRINTF_FORMATTING(3, 4)
    void add_error(const Location &loc, const char *fmt, ...);
    void halt_compilation();
    void print_errors();

    ACE_INLINE
    ExprRef add_expr(const Expr &expr)
    {
        ACE_ASSERT(expr.kind != ExprKind_Unknown);
        ExprRef ref = {(uint32_t)this->exprs.len};
        this->exprs.push_back(expr);
        return ref;
    }

    ACE_INLINE
    StmtRef add_stmt(const Stmt &stmt)
    {
        ACE_ASSERT(stmt.kind != StmtKind_Unknown);
        StmtRef ref = {(uint32_t)this->stmts.len};
        this->stmts.push_back(stmt);
        return ref;
    }

    ACE_INLINE
    DeclRef add_decl(const Decl &decl)
    {
        ACE_ASSERT(decl.kind != DeclKind_Unknown);
        DeclRef ref = {(uint32_t)this->decls.len};
        this->decls.push_back(decl);
        return ref;
    }

    ACE_INLINE
    Expr *get_expr(ExprRef ref)
    {
        return &this->exprs[ref.id];
    }

    ACE_INLINE
    Stmt *get_stmt(StmtRef ref)
    {
        return &this->stmts[ref.id];
    }

    ACE_INLINE
    Decl *get_decl(DeclRef ref)
    {
        return &this->decls[ref.id];
    }

    void compile(ace::String path);
};

void parse_file(Compiler *compiler, File *file);
void analyze_file(Compiler *compiler, File *file);

inline Decl DeclRef::get(Compiler *compiler)
{
    return compiler->decls[this->id];
}

inline Stmt StmtRef::get(Compiler *compiler)
{
    return compiler->stmts[this->id];
}

inline Expr ExprRef::get(Compiler *compiler)
{
    return compiler->exprs[this->id];
}

inline Type TypeRef::get(Compiler *compiler)
{
    return compiler->types[this->id];
}
