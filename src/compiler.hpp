#pragma once

#include <ace_base.hpp>

struct DeclRef {
    uint32_t id;
};

struct StmtRef {
    uint32_t id;
};

struct ExprRef {
    uint32_t id;
};

struct TypeRef {
    uint32_t id;
};

struct File {
    ace::String path;
    ace::String text;

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
    TokenKind_Func,
    TokenKind_Macro,
    TokenKind_Struct,
    TokenKind_Union,
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

struct Scope {
    Scope *parent;
    ace::StringMap<DeclRef> decls;
};

enum TypeKind {
    TypeKind_Void,
    TypeKind_Bool,
    TypeKind_UntypedInt,
    TypeKind_Int,
    TypeKind_Float,
    TypeKind_Struct,
    TypeKind_Tuple,
    TypeKind_Array,
    TypeKind_Slice,
};

struct Type {
    TypeKind kind;
    union {
        struct {
            uint32_t bits;
            bool is_signed;
        } int_;
        struct {
            uint32_t bits;
        } float_;
        struct {
            ace::Array<TypeRef> field_types;
            ace::Array<ace::String> field_names;
        } struct_;
        struct {

        } array;
        struct {

        } slice;
    };
};

enum FunctionFlags {
    FunctionFlags_Inline = 1 << 0,
    FunctionFlags_Extern = 1 << 1,
};

enum ExprKind {
    ExprKind_Identifier,
    ExprKind_StringLiteral,
    ExprKind_IntLiteral,
    ExprKind_FloatLiteral,
    ExprKind_BoolLiteral,
    ExprKind_Function,
    ExprKind_FunctionCall,
    ExprKind_NullPointer,
    ExprKind_PointerType,
    ExprKind_VoidType,
    ExprKind_BoolType,
    ExprKind_IntType,
    ExprKind_FloatType,
};

struct Expr {
    ExprKind kind;
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
    };
};

enum StmtKind {
    StmtKind_Expr,
};

struct Stmt {
    StmtKind kind;
    Location loc;
    union {
        ExprRef expr_ref;
    };
};

enum DeclKind {
    DeclKind_Function,
    DeclKind_FunctionParameter,
    DeclKind_LocalVarDecl,
    DeclKind_GlobalVarDecl,
    DeclKind_ConstDecl,
};

struct Decl {
    DeclKind kind;
    Location loc;
    ace::String name;
    union {
        struct {
            uint32_t flags;
            ace::Array<ExprRef> return_type_expr_refs;
            ace::Array<DeclRef> param_decl_refs;
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

    static Compiler create();
    void destroy();

    ACE_PRINTF_FORMATTING(3, 4)
    void add_error(const Location &loc, const char *fmt, ...);
    void halt_compilation();
    void print_errors();

    void compile(ace::String path);
};

void parse_file(Compiler *compiler, File *file);
void analyze_file(Compiler *compiler, File *file);
