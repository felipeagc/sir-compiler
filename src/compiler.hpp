#pragma once

#include <ace_base.hpp>

struct Compiler;
struct File;

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

struct Scope {
    File *file;
    Scope *parent;
    ace::StringMap<DeclRef> decl_refs;

    static Scope *
    create(Compiler *compiler, File *file, Scope *parent = nullptr);
    void add(Compiler *compiler, DeclRef decl_ref);
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
            ace::Array<TypeRef> field_types;
            ace::Array<ace::String> field_names;
        } struct_;
        struct {
            ace::Array<TypeRef> field_types;
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
    };

    ace::String to_string(Compiler *compiler);
    uint32_t align_of(Compiler *compiler);
    uint32_t size_of(Compiler *compiler);
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
    ExprKind_Unary,
    ExprKind_Binary,
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
    Location loc;
    ace::String name;
    union {
        struct {
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

    static Compiler create();
    void destroy();

    TypeRef get_cached_type(Type *type);

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
