#include "compiler.hpp"

static inline String token_kind_to_string(TokenKind kind)
{
    switch (kind) {
    case TokenKind_Unknown: return "<unknown>";
    case TokenKind_Error: return "<error>";

    case TokenKind_LParen: return "(";
    case TokenKind_RParen: return ")";
    case TokenKind_LBracket: return "[";
    case TokenKind_RBracket: return "]";
    case TokenKind_LCurly: return "{";
    case TokenKind_RCurly: return "}";

    case TokenKind_Colon: return ":";
    case TokenKind_Comma: return ",";
    case TokenKind_Dot: return ".";
    case TokenKind_Semicolon: return ";";
    case TokenKind_Question: return "?";

    case TokenKind_Equal: return "=";

    case TokenKind_And: return "and";
    case TokenKind_Or: return "or";

    case TokenKind_Sub: return "-";
    case TokenKind_Add: return "+";
    case TokenKind_Mul: return "*";
    case TokenKind_Div: return "/";
    case TokenKind_Mod: return "%";
    case TokenKind_Arrow: return "=>";

    case TokenKind_Not: return "!";
    case TokenKind_BitAnd: return "&";
    case TokenKind_BitOr: return "|";
    case TokenKind_BitXor: return "^";
    case TokenKind_BitNot: return "~";

    case TokenKind_EqualEqual: return "==";
    case TokenKind_NotEqual: return "!=";
    case TokenKind_Less: return "<";
    case TokenKind_LessEqual: return "<=";
    case TokenKind_Greater: return ">";
    case TokenKind_GreaterEqual: return ">=";

    case TokenKind_LShift: return "<<";
    case TokenKind_RShift: return ">>";

    case TokenKind_AddEqual: return "+=";
    case TokenKind_SubEqual: return "-=";
    case TokenKind_MulEqual: return "*=";
    case TokenKind_DivEqual: return "/=";
    case TokenKind_ModEqual: return "%=";

    case TokenKind_BitAndEqual: return "&=";
    case TokenKind_BitOrEqual: return "|=";
    case TokenKind_BitXorEqual: return "^=";
    case TokenKind_BitNotEqual: return "~=";
    case TokenKind_LShiftEqual: return "<<=";
    case TokenKind_RShiftEqual: return ">>=";

    case TokenKind_Const: return "const";
    case TokenKind_Extern: return "extern";
    case TokenKind_Export: return "export";
    case TokenKind_VarArg: return "vararg";
    case TokenKind_Global: return "global";
    case TokenKind_Var: return "var";
    case TokenKind_Val: return "val";
    case TokenKind_Inline: return "inline";
    case TokenKind_Distinct: return "distinct";
    case TokenKind_Macro: return "macro";
    case TokenKind_Fn: return "fn";
    case TokenKind_Type: return "type";
    case TokenKind_Struct: return "struct";
    case TokenKind_Union: return "union";
    case TokenKind_ComptimeIf: return "#if";
    case TokenKind_If: return "if";
    case TokenKind_Else: return "else";
    case TokenKind_While: return "while";
    case TokenKind_Break: return "break";
    case TokenKind_Continue: return "continue";
    case TokenKind_Return: return "return";
    case TokenKind_Void: return "void";
    case TokenKind_Bool: return "bool";
    case TokenKind_True: return "true";
    case TokenKind_False: return "false";
    case TokenKind_Null: return "null";
    case TokenKind_Undefined: return "undefined";
    case TokenKind_U8: return "u8";
    case TokenKind_U16: return "u16";
    case TokenKind_U32: return "u32";
    case TokenKind_U64: return "u64";
    case TokenKind_I8: return "i8";
    case TokenKind_I16: return "i16";
    case TokenKind_I32: return "i32";
    case TokenKind_I64: return "i64";
    case TokenKind_F32: return "f32";
    case TokenKind_F64: return "f64";
    case TokenKind_ISize: return "isize";
    case TokenKind_USize: return "usize";
    case TokenKind_Identifier: return "<identifier>";
    case TokenKind_BuiltinIdentifier: return "<builtin identifier>";
    case TokenKind_StringLiteral: return "<string literal>";
    case TokenKind_CharLiteral: return "<character literal>";
    case TokenKind_IntLiteral: return "<integer literal>";
    case TokenKind_FloatLiteral: return "<float literal>";

    case TokenKind_EOF: return "<eof>";
    }

    return "<unknown>";
}

static String token_to_string(Compiler *compiler, const Token &token)
{
    switch (token.kind) {
    case TokenKind_Error:
        return compiler->arena->sprintf(
            "error: \"%.*s\"", (int)token.str.len, token.str.ptr);
    case TokenKind_StringLiteral:
        return compiler->arena->sprintf(
            "string literal: \"%.*s\"", (int)token.str.len, token.str.ptr);
    case TokenKind_Identifier:
        return compiler->arena->sprintf(
            "identifier: \"%.*s\"", (int)token.str.len, token.str.ptr);
    default: return token_kind_to_string(token.kind);
    }

    LANG_ASSERT(0);
    return "";
}

LANG_INLINE static bool is_whitespace(char c)
{
    return c == ' ' || c == '\t' || c == '\r' || c == '\n';
}

LANG_INLINE static bool is_alpha(char c)
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_');
}

LANG_INLINE static bool is_alpha_num(char c)
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_') ||
           (c >= '0' && c <= '9');
}

LANG_INLINE static bool is_hex(char c)
{
    return (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') ||
           (c >= '0' && c <= '9');
}

LANG_INLINE static bool is_num(char c)
{
    return (c >= '0' && c <= '9');
}

enum EqClass : uint8_t {
    EqClass_Other = 0,
    EqClass_Whitespace,
    EqClass_LineFeed,       // \n
    EqClass_CarriageReturn, // \r
    EqClass_LetterX,        // x (lowercase)
    EqClass_LetterHex,      // a..f A..F
    EqClass_Letter,         // g..z g..Z (excluding x)
    EqClass_DigitZero,      // 0
    EqClass_Digit,          // 1..9
    EqClass_Dot,            // .
    EqClass_Comma,          // ,
    EqClass_Colon,          // :
    EqClass_Semicolon,      // ;
    EqClass_LParen,         // (
    EqClass_RParen,         // )
    EqClass_LBracket,       // [
    EqClass_RBracket,       // ]
    EqClass_LCurly,         // {
    EqClass_RCurly,         // }
    EqClass_Ampersand,      // &
    EqClass_Pipe,           // |
    EqClass_Tilde,          // ~
    EqClass_Caret,          // ^
    EqClass_Exclam,         // !
    EqClass_Equal,          // =
    EqClass_Less,           // <
    EqClass_Greater,        // >
    EqClass_Plus,           // +
    EqClass_Minus,          // -
    EqClass_Asterisk,       // *
    EqClass_Underscore,     // _
    EqClass_SingleQuote,    // '
    EqClass_DoubleQuote,    // "
    EqClass_Slash,          // /
    EqClass_BackSlash,      // "\"
    EqClass_Percent,        // %
    EqClass_Question,       // ?
    EqClass_At,             // @
    EqClass_Pound,          // #
    EqClass_EOF,            // \0
    EqClass_COUNT,
};

enum State : uint8_t {
    State_Error = 0,

    State_Whitespace,
    State_Identifier,
    State_ComptimeIdentifier,
    State_BuiltinIdentifier,
    State_StringLiteral,
    State_IntLiteral,
    State_HexIntLiteral,
    State_FloatLiteral,
    State_LParen,
    State_RParen,
    State_LBracket,
    State_RBracket,
    State_LCurly,
    State_RCurly,
    State_Dot,
    State_Comma,
    State_Colon,
    State_Semicolon,
    State_Question,
    State_Not,
    State_BitAnd,
    State_BitOr,
    State_BitNot,
    State_BitXor,
    State_BitAndEqual,
    State_BitOrEqual,
    State_BitNotEqual,
    State_BitXorEqual,
    State_Mul,
    State_Add,
    State_Sub,
    State_Div,
    State_Mod,
    State_MulEqual,
    State_AddEqual,
    State_SubEqual,
    State_DivEqual,
    State_ModEqual,
    State_EqualEqual,
    State_NotEqual,
    State_Less,
    State_LessEqual,
    State_Greater,
    State_GreaterEqual,
    State_LShift,
    State_RShift,
    State_LShiftEqual,
    State_RShiftEqual,
    State_Arrow,
    State_Assign,

    State_EOF,

    State_Final,

    State_Start,

    State_UWhitespace,
    State_UCommentBody,

    State_UIdentifier,
    State_UComptimeIdentifier,
    State_UBuiltinIdentifier,
    State_UIntLiteral,
    State_UHexIntLiteral,
    State_UZeroIntLiteral,
    State_UFloatLiteral,
    State_UOpenStringLiteral,
    State_UOpenStringLiteralSlashEscape,
    State_UClosedStringLiteral,
    State_ULParen,
    State_URParen,
    State_ULBracket,
    State_URBracket,
    State_ULCurly,
    State_URCurly,
    State_UComma,
    State_UColon,
    State_USemicolon,
    State_UQuestion,
    State_UDot,
    State_UNot,
    State_UBitAnd,
    State_UBitOr,
    State_UBitXor,
    State_UBitNot,
    State_UBitAndEqual,
    State_UBitOrEqual,
    State_UBitXorEqual,
    State_UBitNotEqual,
    State_UMul,
    State_UAdd,
    State_USub,
    State_UDivOrComment,
    State_UMod,
    State_UMulEqual,
    State_UAddEqual,
    State_USubEqual,
    State_UDivEqual,
    State_UModEqual,
    State_UEqualEqual,
    State_UNotEqual,
    State_ULess,
    State_ULessEqual,
    State_UGreater,
    State_UGreaterEqual,
    State_ULShift,
    State_URShift,
    State_ULShiftEqual,
    State_URShiftEqual,
    State_UAssign,
    State_UArrow,

    State_COUNT,
};

static EqClass EQ_CLASSES[255];
static State TRANSITION[State_COUNT][EqClass_COUNT];
static uint32_t EQ_CLASS_COLCOUNT_AND_MASK[EqClass_COUNT];
static uint32_t EQ_CLASS_COLCOUNT_OR_MASK[EqClass_COUNT];
static TokenKind STATE_TOKENS[State_COUNT];

static void end_transition(State from_state, State final_state)
{
    for (size_t i = 0; i < EqClass_COUNT; ++i) {
        TRANSITION[from_state][i] = final_state;
    }
}

struct TokenizerState {
    FileRef file_ref;
    const char *text;
    size_t text_len;
    uint32_t pos;
    uint32_t line;
    uint32_t col;

    static TokenizerState
    create(FileRef file_ref, const char *text, size_t text_len)
    {
        TokenizerState state = {};
        state.file_ref = file_ref;
        state.text = text;
        state.text_len = text_len;
        state.pos = 0;
        state.line = 1;
        state.col = 1;
        return state;
    }

    Token consume_token(Compiler *compiler, TokenKind token_kind)
    {
        Token token = {};
        *this = this->next_token(compiler, &token);
        if (token.kind != token_kind) {
            String token_string = token_to_string(compiler, token);
            if (token.kind == TokenKind_Error) {
                compiler->add_error(
                    token.loc,
                    "unexpected token: '%.*s'",
                    (int)token_string.len,
                    token_string.ptr);
                compiler->halt_compilation();
            }

            String expected_token_string = token_kind_to_string(token_kind);

            compiler->add_error(
                token.loc,
                "unexpected token: '%.*s', expecting '%.*s'",
                (int)token_string.len,
                token_string.ptr,
                (int)expected_token_string.len,
                expected_token_string.ptr);
            compiler->halt_compilation();
        }

        return token;
    }

    LANG_INLINE TokenizerState
    next_token(Compiler *compiler, Token *token) const
    {
        ZoneScoped;

        TokenizerState state = *this;
        Allocator *allocator = compiler->arena;

    start:

        *token = {};
        token->loc.file_ref = state.file_ref;
        token->loc.offset = state.pos;
        token->loc.col = state.col;
        token->loc.line = state.line;

        State mstate = State_Start;
        EqClass eq_class;

        do {
            char c = state.text[state.pos++];
            eq_class = EQ_CLASSES[(int)c];
            mstate = TRANSITION[mstate][eq_class];

            state.line += (eq_class == EqClass_LineFeed);
            state.col++;
            state.col &= EQ_CLASS_COLCOUNT_AND_MASK[eq_class];
            state.col |= EQ_CLASS_COLCOUNT_OR_MASK[eq_class];
        } while (mstate > State_Final);

        state.pos--;
        state.col--;
        state.line -= (eq_class == EqClass_LineFeed);

        if (mstate == State_Whitespace) {
            goto start;
        }

        token->loc.len = state.pos - token->loc.offset;

        switch (mstate) {
        case State_Identifier: {
            token->kind = TokenKind_Identifier;
            String ident_str =
                String{&state.text[token->loc.offset], token->loc.len};
            if (!compiler->keyword_map.get(ident_str, &token->kind)) {
                token->str = ident_str;
            }
            break;
        }
        case State_ComptimeIdentifier: {
            String ident_str =
                String{&state.text[token->loc.offset], token->loc.len};
            if (!compiler->keyword_map.get(ident_str, &token->kind)) {
                token->kind = TokenKind_Error;
            }
            break;
        }
        case State_BuiltinIdentifier: {
            token->kind = TokenKind_BuiltinIdentifier;
            String ident_str =
                String{&state.text[token->loc.offset + 1], token->loc.len - 1};
            token->str = ident_str;
            break;
        }
        case State_StringLiteral: {
            token->kind = TokenKind_StringLiteral;
            String ident_str =
                String{&state.text[token->loc.offset + 1], token->loc.len - 2};

            compiler->sb.reset();
            for (size_t i = 0; i < ident_str.len; ++i) {
                if (ident_str.ptr[i] == '\\') {
                    if (i + 1 >= ident_str.len) {
                        token->kind = TokenKind_Error;
                        token->str = "invalid string literal";
                        goto end;
                    }
                    ++i;

                    char actual_char = ident_str.ptr[i];
                    switch (ident_str.ptr[i]) {
                    case 'n': actual_char = '\n'; break;
                    case 'r': actual_char = '\r'; break;
                    case 't': actual_char = '\t'; break;
                    case '0': actual_char = '\0'; break;
                    case '\"': actual_char = '\"'; break;
                    case '\'': actual_char = '\''; break;
                    case '\\': actual_char = '\\'; break;
                    default: {
                        compiler->sb.append('\\');
                        break;
                    }
                    }

                    compiler->sb.append(actual_char);
                    continue;
                }

                compiler->sb.append(ident_str.ptr[i]);
            }

            token->str = compiler->sb.build_null_terminated(compiler->arena);
            break;
        }
        case State_FloatLiteral: {
            token->kind = TokenKind_FloatLiteral;
            String ident_str =
                String{&state.text[token->loc.offset], token->loc.len};
            const char *strz = allocator->null_terminate(ident_str);
            token->f64 = strtod(strz, NULL);
            break;
        }
        case State_IntLiteral: {
            token->kind = TokenKind_IntLiteral;
            String ident_str =
                String{&state.text[token->loc.offset], token->loc.len};
            const char *strz = allocator->null_terminate(ident_str);
            token->u64 = strtoull(strz, NULL, 10);
            break;
        }
        case State_HexIntLiteral: {
            token->kind = TokenKind_IntLiteral;
            LANG_ASSERT(token->loc.len > 2);
            String ident_str =
                String{&state.text[token->loc.offset + 2], token->loc.len};
            const char *strz = allocator->null_terminate(ident_str);
            token->u64 = strtoull(strz, NULL, 16);
            break;
        }
        case State_Error: {
            token->str = String{&state.text[token->loc.offset], token->loc.len};
            break;
        }
        default: token->kind = STATE_TOKENS[mstate]; break;
        }

    end:
        return state;
    }
};

struct ParserState {
    Array<Token> tokens;
    size_t pos;

    LANG_INLINE Token peek_token() const
    {
        return this->tokens[this->pos];
    }

    LANG_INLINE Token next_token()
    {
        return this->tokens[this->pos++];
    }

    Token consume_token(Compiler *compiler, TokenKind token_kind)
    {
        Token token = this->peek_token();
        if (token.kind != token_kind) {
            String token_string = token_to_string(compiler, token);
            if (token.kind == TokenKind_Error) {
                compiler->add_error(
                    token.loc,
                    "unexpected token: '%.*s'",
                    (int)token_string.len,
                    token_string.ptr);
                compiler->halt_compilation();
            }

            String expected_token_string = token_kind_to_string(token_kind);

            compiler->add_error(
                token.loc,
                "unexpected token: '%.*s', expecting '%.*s'",
                (int)token_string.len,
                token_string.ptr,
                (int)expected_token_string.len,
                expected_token_string.ptr);
            compiler->halt_compilation();
        }

        return this->tokens[this->pos++];
    }
};

static Expr
parse_expr(Compiler *compiler, ParserState *state, Location *expr_loc);
static Stmt
parse_stmt(Compiler *compiler, ParserState *state, Location *expr_loc);

static Expr
parse_primary_expr(Compiler *compiler, ParserState *state, Location *expr_loc)
{
    ZoneScoped;

    Expr expr = {};

    Token next_token = state->peek_token();
    switch (next_token.kind) {
    case TokenKind_LParen: {
        state->next_token();
        expr = parse_expr(compiler, state, expr_loc);
        state->consume_token(compiler, TokenKind_RParen);
        break;
    }
    case TokenKind_Identifier: {
        Token ident_token = state->next_token();
        expr.kind = ExprKind_Identifier;
        *expr_loc = ident_token.loc;
        expr.ident.str = ident_token.str;
        break;
    }
    case TokenKind_StringLiteral: {
        Token str_token = state->next_token();
        expr.kind = ExprKind_StringLiteral;
        *expr_loc = str_token.loc;
        expr.str_literal.str = str_token.str;
        break;
    }
    case TokenKind_IntLiteral: {
        Token int_token = state->next_token();
        expr.kind = ExprKind_IntLiteral;
        *expr_loc = int_token.loc;
        expr.int_literal.u64 = int_token.u64;
        break;
    }
    case TokenKind_FloatLiteral: {
        Token float_token = state->next_token();
        expr.kind = ExprKind_FloatLiteral;
        *expr_loc = float_token.loc;
        expr.float_literal.f64 = float_token.f64;
        break;
    }
    case TokenKind_True: {
        state->next_token();

        expr.kind = ExprKind_BoolLiteral;
        *expr_loc = next_token.loc;
        expr.bool_literal.bool_ = true;
        break;
    }
    case TokenKind_False: {
        state->next_token();

        expr.kind = ExprKind_BoolLiteral;
        *expr_loc = next_token.loc;
        expr.bool_literal.bool_ = false;
        break;
    }
    case TokenKind_Null: {
        state->next_token();

        expr.kind = ExprKind_NullLiteral;
        *expr_loc = next_token.loc;
        break;
    }
    case TokenKind_Undefined: {
        state->next_token();

        expr.kind = ExprKind_UndefinedLiteral;
        *expr_loc = next_token.loc;
        break;
    }
    case TokenKind_Mul: {
        Token asterisk_token = state->next_token();

        Location sub_loc = {};
        Expr sub_expr = parse_expr(compiler, state, &sub_loc);
        ExprRef sub_expr_ref = compiler->add_expr(sub_loc, sub_expr);

        expr.kind = ExprKind_PointerType;
        *expr_loc = asterisk_token.loc;
        expr.ptr_type.sub_expr_ref = sub_expr_ref;
        break;
    }
    case TokenKind_LBracket: {
        Token lbracket_token = state->next_token();

        Token next_token = state->peek_token();
        if (next_token.kind != TokenKind_RBracket) {
            Location size_expr_loc = {};
            Expr size_expr = parse_expr(compiler, state, &size_expr_loc);
            state->consume_token(compiler, TokenKind_RBracket);

            Location subtype_expr_loc = {};
            Expr subtype_expr = parse_expr(compiler, state, &subtype_expr_loc);

            expr.kind = ExprKind_ArrayType;
            *expr_loc = lbracket_token.loc;
            expr.array_type.size_expr_ref =
                compiler->add_expr(size_expr_loc, size_expr);
            expr.array_type.subtype_expr_ref =
                compiler->add_expr(subtype_expr_loc, subtype_expr);
        } else {
            state->consume_token(compiler, TokenKind_RBracket);

            Location subtype_expr_loc = {};
            Expr subtype_expr = parse_expr(compiler, state, &subtype_expr_loc);

            expr.kind = ExprKind_SliceType;
            *expr_loc = lbracket_token.loc;
            expr.slice_type.subtype_expr_ref =
                compiler->add_expr(subtype_expr_loc, subtype_expr);
        }
        break;
    }
    case TokenKind_Void: {
        state->next_token();

        expr.kind = ExprKind_VoidType;
        *expr_loc = next_token.loc;
        break;
    }
    case TokenKind_Bool: {
        state->next_token();

        expr.kind = ExprKind_BoolType;
        *expr_loc = next_token.loc;
        break;
    }
    case TokenKind_U8:
    case TokenKind_U16:
    case TokenKind_U32:
    case TokenKind_U64: {
        state->next_token();

        expr.kind = ExprKind_IntType;
        *expr_loc = next_token.loc;
        expr.int_type.is_signed = false;

        switch (next_token.kind) {
        case TokenKind_U8: expr.int_type.bits = 8; break;
        case TokenKind_U16: expr.int_type.bits = 16; break;
        case TokenKind_U32: expr.int_type.bits = 32; break;
        case TokenKind_U64: expr.int_type.bits = 64; break;
        default: LANG_ASSERT(0);
        }

        break;
    }
    case TokenKind_I8:
    case TokenKind_I16:
    case TokenKind_I32:
    case TokenKind_I64: {
        state->next_token();

        expr.kind = ExprKind_IntType;
        *expr_loc = next_token.loc;
        expr.int_type.is_signed = true;

        switch (next_token.kind) {
        case TokenKind_I8: expr.int_type.bits = 8; break;
        case TokenKind_I16: expr.int_type.bits = 16; break;
        case TokenKind_I32: expr.int_type.bits = 32; break;
        case TokenKind_I64: expr.int_type.bits = 64; break;
        default: LANG_ASSERT(0);
        }

        break;
    }
    case TokenKind_F32:
    case TokenKind_F64: {
        state->next_token();

        expr.kind = ExprKind_FloatType;
        *expr_loc = next_token.loc;

        switch (next_token.kind) {
        case TokenKind_F32: expr.float_type.bits = 32; break;
        case TokenKind_F64: expr.float_type.bits = 64; break;
        default: LANG_ASSERT(0);
        }

        break;
    }
    case TokenKind_USize: {
        state->next_token();

        expr.kind = ExprKind_USizeType;
        *expr_loc = next_token.loc;

        break;
    }
    case TokenKind_ISize: {
        state->next_token();

        expr.kind = ExprKind_ISizeType;
        *expr_loc = next_token.loc;

        break;
    }
    case TokenKind_Distinct: {
        Token distinct_token = state->next_token();

        Location sub_loc = {};
        Expr sub_expr = parse_expr(compiler, state, &sub_loc);
        ExprRef sub_expr_ref = compiler->add_expr(sub_loc, sub_expr);

        expr.kind = ExprKind_DistinctType;
        *expr_loc = distinct_token.loc;
        expr.distinct_type.sub_expr_ref = sub_expr_ref;

        break;
    }
    case TokenKind_BuiltinIdentifier: {
        Token ident_token =
            state->consume_token(compiler, TokenKind_BuiltinIdentifier);

        state->consume_token(compiler, TokenKind_LParen);

        BuiltinFunction builtin_func_id = BuiltinFunction_Unknown;
        if (!compiler->builtin_function_map.get(
                ident_token.str, &builtin_func_id)) {
            compiler->add_error(
                next_token.loc,
                "invalid builtin function: '@%.*s'",
                (int)ident_token.str.len,
                ident_token.str.ptr);
        }

        BuiltinFunction builtin_func = (BuiltinFunction)builtin_func_id;

        expr.kind = ExprKind_BuiltinCall;
        *expr_loc = ident_token.loc;
        expr.builtin_call.builtin = builtin_func;
        expr.builtin_call.param_refs = Array<ExprRef>::create(compiler->arena);

        next_token = state->peek_token();
        while (next_token.kind != TokenKind_RParen) {
            Location param_expr_loc = {};
            Expr param_expr = parse_expr(compiler, state, &param_expr_loc);
            ExprRef param_expr_ref =
                compiler->add_expr(param_expr_loc, param_expr);

            expr.builtin_call.param_refs.push_back(param_expr_ref);

            next_token = state->peek_token();
            if (next_token.kind != TokenKind_RParen) {
                state->consume_token(compiler, TokenKind_Comma);
            }

            next_token = state->peek_token();
        }

        state->consume_token(compiler, TokenKind_RParen);
        break;
    }
    case TokenKind_Struct: {
        Token struct_token = state->next_token();

        expr.kind = ExprKind_StructType;
        *expr_loc = struct_token.loc;
        expr.struct_type.field_names = Array<String>::create(compiler->arena);
        expr.struct_type.field_type_expr_refs =
            Array<ExprRef>::create(compiler->arena);

        state->consume_token(compiler, TokenKind_LCurly);

        next_token = state->peek_token();
        while (next_token.kind != TokenKind_RCurly) {
            Token field_ident_token =
                state->consume_token(compiler, TokenKind_Identifier);

            state->consume_token(compiler, TokenKind_Colon);

            Location field_type_expr_loc = {};
            Expr field_type_expr =
                parse_expr(compiler, state, &field_type_expr_loc);

            expr.struct_type.field_names.push_back(field_ident_token.str);
            expr.struct_type.field_type_expr_refs.push_back(
                compiler->add_expr(field_type_expr_loc, field_type_expr));

            next_token = state->peek_token();
            if (next_token.kind != TokenKind_RCurly) {
                state->consume_token(compiler, TokenKind_Comma);
            }

            next_token = state->peek_token();
        }

        state->consume_token(compiler, TokenKind_RCurly);
        break;
    }
    default: {
        String token_string = token_to_string(compiler, next_token);
        compiler->add_error(
            next_token.loc,
            "unexpected token: '%.*s', expecting primary expression",
            (int)token_string.len,
            token_string.ptr);
        compiler->halt_compilation();
        break;
    }
    }

    return expr;
}

#if 0
static Expr parse_func_expr(Compiler *compiler, TokenizerState *state)
{
    ZoneScoped;

    Expr expr = {};

    Token next_token = {};
    state->next_token(compiler, &next_token);

    expr.loc = next_token.loc;
    expr.func = {};

    expr.func.param_decl_refs = Array<DeclRef>::create(compiler->arena);
    expr.func.return_type_expr_refs = Array<ExprRef>::create(compiler->arena);

    switch (next_token.kind) {
    case TokenKind_Inline: {
        state->consume_token(compiler, TokenKind_Inline);
        expr.func.flags |= FunctionFlags_Inline;
        break;
    }
    default: {
        String token_string = token_to_string(compiler, next_token);
        compiler->add_error(
            next_token.loc,
            "unexpected token: '%.*s', expecting function literal",
            (int)token_string.len,
            token_string.ptr);
        compiler->halt_compilation();
        break;
    }
    }

    state->consume_token(compiler, TokenKind_LParen);

    // Parse params:

    state->next_token(compiler, &next_token);
    while (next_token.kind == TokenKind_Identifier) {
        Token ident_token =
            state->consume_token(compiler, TokenKind_Identifier);
        state->consume_token(compiler, TokenKind_Colon);

        Expr type_expr = parse_expr(compiler, state);
        ExprRef type_expr_ref = compiler->add_expr(type_expr);

        Decl param_decl = {};
        param_decl.kind = DeclKind_FunctionParameter;
        param_decl.loc = ident_token.loc;
        param_decl.name = ident_token.str;
        param_decl.func_param.type_expr = type_expr_ref;

        DeclRef param_decl_ref = compiler->add_decl(param_decl);

        expr.func.param_decl_refs.push_back(param_decl_ref);

        state->next_token(compiler, &next_token);
        if (next_token.kind == TokenKind_Comma) {
            *state = state->next_token(compiler, &next_token);
        } else {
            break;
        }

        state->next_token(compiler, &next_token);
    }

    state->consume_token(compiler, TokenKind_RParen);

    state->next_token(compiler, &next_token);
    if (next_token.kind == TokenKind_Colon) {
        state->consume_token(compiler, TokenKind_Colon);

        while (1) {
            Expr return_type_expr = parse_expr(compiler, state);
            ExprRef return_type_expr_ref = compiler->add_expr(return_type_expr);

            expr.func.return_type_expr_refs.push_back(return_type_expr_ref);

            state->next_token(compiler, &next_token);
            if (next_token.kind == TokenKind_Comma) {
                state->consume_token(compiler, TokenKind_Comma);
                continue;
            } else {
                break;
            }
        }
    }

    state->consume_token(compiler, TokenKind_Arrow);

    state->consume_token(compiler, TokenKind_LCurly);

    state->next_token(compiler, &next_token);
    while (next_token.kind != TokenKind_RCurly) {
        Stmt stmt = parse_stmt(compiler, state);
        StmtRef stmt_ref = compiler->add_stmt(stmt);

        expr.func.body_stmts.push_back(stmt_ref);

        state->next_token(compiler, &next_token);
    }

    state->consume_token(compiler, TokenKind_RCurly);

    return expr;
}
#endif

static Expr
parse_func_call_expr(Compiler *compiler, ParserState *state, Location *expr_loc)
{
    ZoneScoped;

    Expr expr = parse_primary_expr(compiler, state, expr_loc);

    Token next_token = state->peek_token();
    while (1) {
        switch (next_token.kind) {
        case TokenKind_LBracket: {
            state->next_token();

            Location indexed_expr_loc = *expr_loc;
            Expr indexed_expr = expr;

            Location index_expr_loc = {};
            Expr index_expr = parse_expr(compiler, state, &index_expr_loc);

            expr = {};
            expr.kind = ExprKind_Subscript;
            expr.subscript.left_ref =
                compiler->add_expr(indexed_expr_loc, indexed_expr);
            expr.subscript.right_ref =
                compiler->add_expr(index_expr_loc, index_expr);

            state->consume_token(compiler, TokenKind_RBracket);
            break;
        }
        case TokenKind_LParen: {
            state->next_token();

            Expr func_expr = expr;
            Location func_expr_loc = *expr_loc;
            ExprRef func_expr_ref =
                compiler->add_expr(func_expr_loc, func_expr);

            expr = {};
            expr.kind = ExprKind_FunctionCall;
            expr.func_call.func_expr_ref = func_expr_ref;
            expr.func_call.param_refs = Array<ExprRef>::create(compiler->arena);

            next_token = state->peek_token();
            while (next_token.kind != TokenKind_RParen) {
                Location param_expr_loc = {};
                Expr param_expr = parse_expr(compiler, state, &param_expr_loc);
                ExprRef param_expr_ref =
                    compiler->add_expr(param_expr_loc, param_expr);

                expr.func_call.param_refs.push_back(param_expr_ref);

                next_token = state->peek_token();
                if (next_token.kind != TokenKind_RParen) {
                    state->consume_token(compiler, TokenKind_Comma);
                }

                next_token = state->peek_token();
            }

            state->consume_token(compiler, TokenKind_RParen);
            break;
        }
        case TokenKind_Dot: {
            Token dot_tok = state->next_token();

            next_token = state->peek_token();
            if (next_token.kind == TokenKind_Mul) {
                state->consume_token(compiler, TokenKind_Mul);

                Location sub_expr_loc = *expr_loc;
                Expr sub_expr = expr;
                ExprRef sub_expr_ref =
                    compiler->add_expr(sub_expr_loc, sub_expr);

                expr = {};
                *expr_loc = dot_tok.loc;
                expr.kind = ExprKind_Unary;
                expr.unary.op = UnaryOp_Dereference;
                expr.unary.left_ref = sub_expr_ref;
            } else {
                Location left_expr_loc = *expr_loc;
                Expr left_expr = expr;
                Token ident_tok =
                    state->consume_token(compiler, TokenKind_Identifier);

                Location accessed_ident_expr_loc = ident_tok.loc;
                Expr accessed_ident_expr = {};
                accessed_ident_expr.kind = ExprKind_Identifier;
                accessed_ident_expr.ident.str = ident_tok.str;

                ExprRef left_expr_ref =
                    compiler->add_expr(left_expr_loc, left_expr);
                ExprRef right_expr_ref = compiler->add_expr(
                    accessed_ident_expr_loc, accessed_ident_expr);

                expr = {};
                *expr_loc = dot_tok.loc;
                expr.kind = ExprKind_Access;
                expr.access.left_ref = left_expr_ref;
                expr.access.accessed_ident_ref = right_expr_ref;
            }

            break;
        }
        default: goto end;
        }

        next_token = state->peek_token();
    }

end:

    return expr;
}

static Expr
parse_unary_expr(Compiler *compiler, ParserState *state, Location *expr_loc)
{
    ZoneScoped;

    Token next_token = state->peek_token();
    switch (next_token.kind) {
    case TokenKind_Sub:
    case TokenKind_Not:
    case TokenKind_BitAnd:
    case TokenKind_BitNot: {
        state->next_token();

        UnaryOp op = {};

        switch (next_token.kind) {
        case TokenKind_Sub: op = UnaryOp_Negate; break;
        case TokenKind_Not: op = UnaryOp_Not; break;
        case TokenKind_BitAnd: op = UnaryOp_AddressOf; break;
        case TokenKind_BitNot: op = UnaryOp_BitNot; break;
        default: LANG_ASSERT(0); break;
        }

        Location right_expr_loc = {};
        Expr right_expr = parse_unary_expr(compiler, state, &right_expr_loc);

        Expr expr = {};
        expr.kind = ExprKind_Unary;
        *expr_loc = next_token.loc;
        expr.unary.left_ref = compiler->add_expr(right_expr_loc, right_expr);
        expr.unary.op = op;

        return expr;
    }

    default: break;
    }

    return parse_func_call_expr(compiler, state, expr_loc);
}

enum BinaryOpSymbolKind {
    BinaryOpSymbol_Expr,
    BinaryOpSymbol_Operator,
};

struct BinaryOpSymbol {
    BinaryOpSymbolKind kind;
    union {
        struct {
            Location loc;
            Expr expr;
        } expr;
        BinaryOp op;
    };
};

static Expr
parse_binary_expr(Compiler *compiler, ParserState *state, Location *expr_loc)
{
    ZoneScoped;

    Expr expr = parse_unary_expr(compiler, state, expr_loc);

    Token next_token = state->peek_token();
    switch (next_token.kind) {
    case TokenKind_Add:
    case TokenKind_Sub:
    case TokenKind_Mul:
    case TokenKind_Div:
    case TokenKind_Mod:

    case TokenKind_BitOr:
    case TokenKind_BitXor:
    case TokenKind_BitAnd:

    case TokenKind_LShift:
    case TokenKind_RShift:

    case TokenKind_EqualEqual:
    case TokenKind_NotEqual:
    case TokenKind_Greater:
    case TokenKind_GreaterEqual:
    case TokenKind_Less:
    case TokenKind_LessEqual:

    case TokenKind_And:
    case TokenKind_Or: break;
    default: return expr;
    }

    Array<BinaryOp> op_stack =
        Array<BinaryOp>::create(MallocAllocator::get_instance());
    Array<BinaryOpSymbol> symbol_queue =
        Array<BinaryOpSymbol>::create(MallocAllocator::get_instance());

    static uint8_t precedences[BinaryOp_MAX] = {
        0,  // BinaryOp_Unknown,
        4,  // BinaryOp_Add
        4,  // BinaryOp_Sub
        3,  // BinaryOp_Mul
        3,  // BinaryOp_Div
        3,  // BinaryOp_Mod
        8,  // BinaryOp_BitAnd
        10, // BinaryOp_BitOr
        9,  // BinaryOp_BitXor
        5,  // BinaryOp_LShift
        5,  // BinaryOp_RShift
        7,  // BinaryOp_Equal
        7,  // BinaryOp_NotEqual
        6,  // BinaryOp_Less
        6,  // BinaryOp_LessEqual
        6,  // BinaryOp_Greater
        6,  // BinaryOp_GreaterEqual
        11, // BinaryOp_And
        12, // BinaryOp_or
    };

    {
        BinaryOpSymbol expr_symbol = {};
        expr_symbol.kind = BinaryOpSymbol_Expr;
        expr_symbol.expr.expr = expr;
        expr_symbol.expr.loc = *expr_loc;
        symbol_queue.push_back(expr_symbol);
    }

    while (true) {
        BinaryOp op = BinaryOp_Unknown;
        switch (next_token.kind) {
        case TokenKind_Add: op = BinaryOp_Add; break;
        case TokenKind_Sub: op = BinaryOp_Sub; break;
        case TokenKind_Mul: op = BinaryOp_Mul; break;
        case TokenKind_Div: op = BinaryOp_Div; break;
        case TokenKind_Mod: op = BinaryOp_Mod; break;
        case TokenKind_BitAnd: op = BinaryOp_BitAnd; break;
        case TokenKind_BitOr: op = BinaryOp_BitOr; break;
        case TokenKind_BitXor: op = BinaryOp_BitXor; break;
        case TokenKind_LShift: op = BinaryOp_LShift; break;
        case TokenKind_RShift: op = BinaryOp_RShift; break;
        case TokenKind_EqualEqual: op = BinaryOp_Equal; break;
        case TokenKind_NotEqual: op = BinaryOp_NotEqual; break;
        case TokenKind_Less: op = BinaryOp_Less; break;
        case TokenKind_LessEqual: op = BinaryOp_LessEqual; break;
        case TokenKind_Greater: op = BinaryOp_Greater; break;
        case TokenKind_GreaterEqual: op = BinaryOp_GreaterEqual; break;
        case TokenKind_And: op = BinaryOp_And; break;
        case TokenKind_Or: op = BinaryOp_Or; break;
        default: break;
        }

        if (op == BinaryOp_Unknown) break;

        state->consume_token(compiler, next_token.kind);

        while (op_stack.len > 0 &&
               precedences[op_stack[op_stack.len - 1]] < precedences[op]) {
            BinaryOp popped_op = op_stack[op_stack.len - 1];
            op_stack.pop();

            BinaryOpSymbol op_symbol = {};
            op_symbol.kind = BinaryOpSymbol_Operator;
            op_symbol.op = popped_op;
            symbol_queue.push_back(op_symbol);
        }

        op_stack.push_back(op);

        Location right_expr_loc = {};
        Expr right_expr = parse_unary_expr(compiler, state, &right_expr_loc);

        {
            BinaryOpSymbol expr_symbol = {};
            expr_symbol.kind = BinaryOpSymbol_Expr;
            expr_symbol.expr.expr = right_expr;
            expr_symbol.expr.loc = right_expr_loc;
            symbol_queue.push_back(expr_symbol);
        }

        next_token = state->peek_token();
    }

    while (op_stack.len > 0) {
        BinaryOp popped_op = op_stack[op_stack.len - 1];
        op_stack.pop();

        BinaryOpSymbol op_symbol = {};
        op_symbol.kind = BinaryOpSymbol_Operator;
        op_symbol.op = popped_op;
        symbol_queue.push_back(op_symbol);
    }

    Array<Expr> expr_stack =
        Array<Expr>::create(MallocAllocator::get_instance());
    Array<Location> expr_loc_stack =
        Array<Location>::create(MallocAllocator::get_instance());

    for (size_t i = 0; i < symbol_queue.len; ++i) {
        BinaryOpSymbol symbol = symbol_queue[i];
        if (symbol.kind == BinaryOpSymbol_Operator) {
            LANG_ASSERT(expr_stack.len >= 2);
            Expr right_expr = expr_stack[expr_stack.len - 1];
            Expr left_expr = expr_stack[expr_stack.len - 2];
            Location right_expr_loc = expr_loc_stack[expr_loc_stack.len - 1];
            Location left_expr_loc = expr_loc_stack[expr_loc_stack.len - 2];
            expr_stack.pop();
            expr_stack.pop();
            expr_loc_stack.pop();
            expr_loc_stack.pop();

            Location bin_expr_loc = left_expr_loc; // TODO: replace bin_expr_loc
                                                   // with operator location
            Expr bin_expr = {};
            bin_expr.kind = ExprKind_Binary;
            bin_expr.binary.op = symbol.op;
            bin_expr.binary.left_ref =
                compiler->add_expr(left_expr_loc, left_expr);
            bin_expr.binary.right_ref =
                compiler->add_expr(right_expr_loc, right_expr);

            expr_stack.push_back(bin_expr);
            expr_loc_stack.push_back(bin_expr_loc);
        } else {
            expr_stack.push_back(symbol.expr.expr);
            expr_loc_stack.push_back(symbol.expr.loc);
        }
    }

    LANG_ASSERT(expr_stack.len == 1);

    Expr result_expr = expr_stack[0];

    op_stack.destroy();
    symbol_queue.destroy();
    expr_stack.destroy();
    expr_loc_stack.destroy();

    return result_expr;
}

static Expr
parse_expr(Compiler *compiler, ParserState *state, Location *expr_loc)
{
    ZoneScoped;

    return parse_binary_expr(compiler, state, expr_loc);
}

static Stmt
parse_stmt(Compiler *compiler, ParserState *state, Location *stmt_loc)
{
    ZoneScoped;

    Stmt stmt = {};

    Token next_token = state->peek_token();

    switch (next_token.kind) {
    case TokenKind_ComptimeIf: {
        Token comptime_if_token = state->next_token();
        *stmt_loc = comptime_if_token.loc;

        stmt.kind = StmtKind_ComptimeIf;
        stmt.comptime_if = {};

        state->consume_token(compiler, TokenKind_LParen);

        Location cond_expr_loc = {};
        Expr cond_expr = parse_expr(compiler, state, &cond_expr_loc);

        stmt.comptime_if.cond_expr_ref =
            compiler->add_expr(cond_expr_loc, cond_expr);

        state->consume_token(compiler, TokenKind_RParen);

        Location true_stmt_loc = {};
        Stmt true_stmt = parse_stmt(compiler, state, &true_stmt_loc);

        stmt.comptime_if.true_stmt_ref =
            compiler->add_stmt(true_stmt_loc, true_stmt);

        next_token = state->peek_token();
        if (next_token.kind == TokenKind_Else) {
            state->consume_token(compiler, TokenKind_Else);

            Location false_stmt_loc = {};
            Stmt false_stmt = parse_stmt(compiler, state, &false_stmt_loc);
            stmt.comptime_if.false_stmt_ref =
                compiler->add_stmt(false_stmt_loc, false_stmt);
        }
        break;
    }
    case TokenKind_If: {
        Token if_token = state->next_token();
        *stmt_loc = if_token.loc;

        stmt.kind = StmtKind_If;
        stmt.if_ = {};

        state->consume_token(compiler, TokenKind_LParen);

        Location cond_expr_loc = {};
        Expr cond_expr = parse_expr(compiler, state, &cond_expr_loc);

        stmt.if_.cond_expr_ref = compiler->add_expr(cond_expr_loc, cond_expr);

        state->consume_token(compiler, TokenKind_RParen);

        Location true_stmt_loc = {};
        Stmt true_stmt = parse_stmt(compiler, state, &true_stmt_loc);

        stmt.if_.true_stmt_ref = compiler->add_stmt(true_stmt_loc, true_stmt);

        next_token = state->peek_token();
        if (next_token.kind == TokenKind_Else) {
            state->consume_token(compiler, TokenKind_Else);

            Location false_stmt_loc = {};
            Stmt false_stmt = parse_stmt(compiler, state, &false_stmt_loc);
            stmt.if_.false_stmt_ref =
                compiler->add_stmt(false_stmt_loc, false_stmt);
        }
        break;
    }
    case TokenKind_While: {
        Token while_token = state->consume_token(compiler, TokenKind_While);
        stmt.kind = StmtKind_While;
        *stmt_loc = while_token.loc;
        stmt.while_ = {};

        state->consume_token(compiler, TokenKind_LParen);

        Location cond_expr_loc = {};
        Expr cond_expr = parse_expr(compiler, state, &cond_expr_loc);

        stmt.while_.cond_expr_ref =
            compiler->add_expr(cond_expr_loc, cond_expr);

        state->consume_token(compiler, TokenKind_RParen);

        Location true_stmt_loc = {};
        Stmt true_stmt = parse_stmt(compiler, state, &true_stmt_loc);

        stmt.while_.true_stmt_ref =
            compiler->add_stmt(true_stmt_loc, true_stmt);
        break;
    }
    case TokenKind_Return: {
        Token return_token = state->consume_token(compiler, TokenKind_Return);
        stmt.kind = StmtKind_Return;
        *stmt_loc = return_token.loc;
        stmt.return_ = {};

        next_token = state->peek_token();
        if (next_token.kind != TokenKind_Semicolon) {
            Location returned_expr_loc = {};
            Expr returned_expr =
                parse_expr(compiler, state, &returned_expr_loc);
            stmt.return_.returned_expr_ref =
                compiler->add_expr(returned_expr_loc, returned_expr);
        }

        state->consume_token(compiler, TokenKind_Semicolon);

        break;
    }
    case TokenKind_LCurly: {
        Token lcurly_token = state->consume_token(compiler, TokenKind_LCurly);

        stmt.kind = StmtKind_Block;
        *stmt_loc = lcurly_token.loc;
        stmt.block.stmt_refs = Array<StmtRef>::create(compiler->arena);

        next_token = state->peek_token();
        while (next_token.kind != TokenKind_RCurly) {
            Location sub_stmt_loc = {};
            Stmt sub_stmt = parse_stmt(compiler, state, &sub_stmt_loc);
            StmtRef sub_stmt_ref = compiler->add_stmt(sub_stmt_loc, sub_stmt);

            stmt.block.stmt_refs.push_back(sub_stmt_ref);

            next_token = state->peek_token();
        }

        state->consume_token(compiler, TokenKind_RCurly);
        break;
    }
    case TokenKind_Var:
    case TokenKind_Val:
    case TokenKind_Const:
    case TokenKind_Global: {
        Token var_kind_tok = state->next_token();

        Token ident_tok = state->consume_token(compiler, TokenKind_Identifier);

        ExprRef type_expr_ref = {0};
        ExprRef value_expr_ref = {0};

        next_token = state->peek_token();
        if (next_token.kind == TokenKind_Colon) {
            state->next_token();

            Location type_expr_loc = {};
            Expr type_expr = parse_expr(compiler, state, &type_expr_loc);
            type_expr_ref = compiler->add_expr(type_expr_loc, type_expr);
        }

        state->consume_token(compiler, TokenKind_Equal);

        Location value_expr_loc = {};
        Expr value_expr = parse_expr(compiler, state, &value_expr_loc);
        value_expr_ref = compiler->add_expr(value_expr_loc, value_expr);

        LANG_ASSERT(type_expr_ref.id > 0 || value_expr_ref.id > 0);

        Location var_decl_loc = ident_tok.loc;
        String var_decl_name = ident_tok.str;
        Decl var_decl = {};

        switch (var_kind_tok.kind) {
        case TokenKind_Var: var_decl.kind = DeclKind_LocalVarDecl; break;
        case TokenKind_Val:
            var_decl.kind = DeclKind_ImmutableLocalVarDecl;
            break;
        case TokenKind_Global: var_decl.kind = DeclKind_GlobalVarDecl; break;
        case TokenKind_Const: var_decl.kind = DeclKind_ConstDecl; break;
        default: LANG_ASSERT(0); break;
        }

        var_decl.var_decl.type_expr = type_expr_ref;
        var_decl.var_decl.value_expr = value_expr_ref;

        state->consume_token(compiler, TokenKind_Semicolon);

        stmt.kind = StmtKind_Decl;
        *stmt_loc = var_decl_loc;
        stmt.decl.decl_ref =
            compiler->add_decl(var_decl_name, var_decl_loc, var_decl);

        break;
    }
    case TokenKind_Type: {
        state->next_token();

        Token ident_tok = state->consume_token(compiler, TokenKind_Identifier);

        Location type_expr_loc = {};
        Expr type_expr = parse_expr(compiler, state, &type_expr_loc);
        ExprRef type_expr_ref = compiler->add_expr(type_expr_loc, type_expr);

        state->consume_token(compiler, TokenKind_Semicolon);

        Location type_decl_loc = ident_tok.loc;
        String type_decl_name = ident_tok.str;
        Decl type_decl = {};
        type_decl.kind = DeclKind_Type;
        type_decl.type_decl.type_expr = type_expr_ref;

        stmt.kind = StmtKind_Decl;
        *stmt_loc = type_decl_loc;
        stmt.decl.decl_ref =
            compiler->add_decl(type_decl_name, type_decl_loc, type_decl);

        break;
    }
    default: {
        Location expr_loc = {};
        Expr expr = parse_expr(compiler, state, &expr_loc);
        ExprRef expr_ref = compiler->add_expr(expr_loc, expr);

        next_token = state->peek_token();
        if (next_token.kind == TokenKind_Equal) {
            state->next_token();

            *stmt_loc = expr_loc;
            stmt.kind = StmtKind_Assign;
            stmt.assign.assigned_expr_ref = expr_ref;

            Location value_expr_loc = {};
            Expr value_expr = parse_expr(compiler, state, &value_expr_loc);

            stmt.assign.value_expr_ref =
                compiler->add_expr(value_expr_loc, value_expr);
        } else {
            stmt.kind = StmtKind_Expr;
            *stmt_loc = expr_loc;
            stmt.expr.expr_ref = expr_ref;
        }

        state->consume_token(compiler, TokenKind_Semicolon);

        break;
    }
    }

    LANG_ASSERT(stmt.kind != StmtKind_Unknown);
    return stmt;
}

static void parse_top_level_decl(
    Compiler *compiler, ParserState *state, Array<DeclRef> *top_level_decls)
{
    ZoneScoped;

    Token next_token = state->peek_token();
    switch (next_token.kind) {
    case TokenKind_ComptimeIf: {
        Token comptime_if_token = state->next_token();

        Decl comptime_if_decl = {};
        Location comptime_if_decl_loc = comptime_if_token.loc;
        comptime_if_decl.kind = DeclKind_ComptimeIf;
        comptime_if_decl.comptime_if = {};
        comptime_if_decl.comptime_if.true_decls =
            Array<DeclRef>::create(compiler->arena);
        comptime_if_decl.comptime_if.false_decls =
            Array<DeclRef>::create(compiler->arena);

        state->consume_token(compiler, TokenKind_LParen);

        Location cond_expr_loc = {};
        Expr cond_expr = parse_expr(compiler, state, &cond_expr_loc);

        comptime_if_decl.comptime_if.cond_expr_ref =
            compiler->add_expr(cond_expr_loc, cond_expr);

        state->consume_token(compiler, TokenKind_RParen);

        state->consume_token(compiler, TokenKind_LCurly);

        while (state->peek_token().kind != TokenKind_RCurly) {
            parse_top_level_decl(
                compiler, state, &comptime_if_decl.comptime_if.true_decls);
        }

        state->consume_token(compiler, TokenKind_RCurly);

        next_token = state->peek_token();
        if (next_token.kind == TokenKind_Else) {
            state->consume_token(compiler, TokenKind_Else);

            state->consume_token(compiler, TokenKind_LCurly);

            while (state->peek_token().kind != TokenKind_RCurly) {
                parse_top_level_decl(
                    compiler, state, &comptime_if_decl.comptime_if.false_decls);
            }

            state->consume_token(compiler, TokenKind_RCurly);
        }

        DeclRef comptime_if_decl_ref =
            compiler->add_decl("", comptime_if_decl_loc, comptime_if_decl);
        top_level_decls->push_back(comptime_if_decl_ref);
        break;
    }
    case TokenKind_Fn: {
        Token func_token = state->next_token();

        Location func_decl_loc = func_token.loc;
        Decl func_decl = {};
        func_decl.kind = DeclKind_Function;
        func_decl.func = compiler->arena->alloc_init<FuncDecl>();

        func_decl.func->param_decl_refs =
            Array<DeclRef>::create(compiler->arena);
        func_decl.func->return_type_expr_refs =
            Array<ExprRef>::create(compiler->arena);
        func_decl.func->body_stmts = Array<StmtRef>::create(compiler->arena);

        next_token = state->peek_token();
        switch (next_token.kind) {
        case TokenKind_Extern: {
            state->consume_token(compiler, TokenKind_Extern);
            func_decl.func->flags |= FunctionFlags_Extern;
            break;
        }
        case TokenKind_Export: {
            state->consume_token(compiler, TokenKind_Export);
            func_decl.func->flags |= FunctionFlags_Exported;
            break;
        }
        case TokenKind_Inline: {
            state->consume_token(compiler, TokenKind_Inline);
            func_decl.func->flags |= FunctionFlags_Inline;
            break;
        }
        default: break;
        }

        next_token = state->peek_token();
        if (next_token.kind == TokenKind_VarArg) {
            state->consume_token(compiler, TokenKind_VarArg);
            func_decl.func->flags |= FunctionFlags_VarArg;
        }

        Token ident_token =
            state->consume_token(compiler, TokenKind_Identifier);
        String func_decl_name = ident_token.str;

        state->consume_token(compiler, TokenKind_LParen);

        // Parse params:

        next_token = state->peek_token();
        while (next_token.kind == TokenKind_Identifier) {
            Token ident_token =
                state->consume_token(compiler, TokenKind_Identifier);
            state->consume_token(compiler, TokenKind_Colon);

            Location type_expr_loc = {};
            Expr type_expr = parse_expr(compiler, state, &type_expr_loc);
            ExprRef type_expr_ref =
                compiler->add_expr(type_expr_loc, type_expr);

            Location param_decl_loc = ident_token.loc;
            String param_decl_name = ident_token.str;
            Decl param_decl = {};
            param_decl.kind = DeclKind_FunctionParameter;
            param_decl.func_param.type_expr = type_expr_ref;

            DeclRef param_decl_ref =
                compiler->add_decl(param_decl_name, param_decl_loc, param_decl);

            func_decl.func->param_decl_refs.push_back(param_decl_ref);

            next_token = state->peek_token();
            if (next_token.kind == TokenKind_Comma) {
                next_token = state->next_token();
            } else {
                break;
            }

            next_token = state->peek_token();
        }

        state->consume_token(compiler, TokenKind_RParen);

        next_token = state->peek_token();
        if (next_token.kind == TokenKind_Colon) {
            state->consume_token(compiler, TokenKind_Colon);

            while (1) {
                Location return_type_expr_loc = {};
                Expr return_type_expr =
                    parse_expr(compiler, state, &return_type_expr_loc);
                ExprRef return_type_expr_ref =
                    compiler->add_expr(return_type_expr_loc, return_type_expr);

                func_decl.func->return_type_expr_refs.push_back(
                    return_type_expr_ref);

                next_token = state->peek_token();
                if (next_token.kind == TokenKind_Comma) {
                    state->consume_token(compiler, TokenKind_Comma);
                    continue;
                } else {
                    break;
                }
            }
        }

        if (func_decl.func->flags & FunctionFlags_Extern) {
            state->consume_token(compiler, TokenKind_Semicolon);
        } else {
            state->consume_token(compiler, TokenKind_LCurly);

            next_token = state->peek_token();
            while (next_token.kind != TokenKind_RCurly) {
                Location stmt_loc = {};
                Stmt stmt = parse_stmt(compiler, state, &stmt_loc);
                StmtRef stmt_ref = compiler->add_stmt(stmt_loc, stmt);

                func_decl.func->body_stmts.push_back(stmt_ref);

                next_token = state->peek_token();
            }

            state->consume_token(compiler, TokenKind_RCurly);
        }

        DeclRef func_decl_ref =
            compiler->add_decl(func_decl_name, func_decl_loc, func_decl);
        top_level_decls->push_back(func_decl_ref);

        break;
    }
    case TokenKind_Global: {
        state->next_token();

        Token ident_tok = state->consume_token(compiler, TokenKind_Identifier);

        ExprRef type_expr_ref = {0};
        ExprRef value_expr_ref = {0};

        next_token = state->peek_token();
        if (next_token.kind == TokenKind_Colon) {
            state->next_token();

            Location type_expr_loc = {};
            Expr type_expr = parse_expr(compiler, state, &type_expr_loc);
            type_expr_ref = compiler->add_expr(type_expr_loc, type_expr);
        }

        state->consume_token(compiler, TokenKind_Equal);

        Location value_expr_loc = {};
        Expr value_expr = parse_expr(compiler, state, &value_expr_loc);
        value_expr_ref = compiler->add_expr(value_expr_loc, value_expr);

        LANG_ASSERT(type_expr_ref.id > 0 || value_expr_ref.id > 0);

        Location var_decl_loc = ident_tok.loc;
        String var_decl_name = ident_tok.str;
        Decl var_decl = {};
        var_decl.kind = DeclKind_GlobalVarDecl;
        var_decl.var_decl.type_expr = type_expr_ref;
        var_decl.var_decl.value_expr = value_expr_ref;

        state->consume_token(compiler, TokenKind_Semicolon);

        DeclRef var_decl_ref =
            compiler->add_decl(var_decl_name, var_decl_loc, var_decl);
        top_level_decls->push_back(var_decl_ref);

        break;
    }
    case TokenKind_Type: {
        state->consume_token(compiler, TokenKind_Type);

        Token ident_tok = state->consume_token(compiler, TokenKind_Identifier);

        Location type_expr_loc = {};
        Expr type_expr = parse_expr(compiler, state, &type_expr_loc);
        ExprRef type_expr_ref = compiler->add_expr(type_expr_loc, type_expr);

        state->consume_token(compiler, TokenKind_Semicolon);

        Location type_decl_loc = ident_tok.loc;
        String type_decl_name = ident_tok.str;
        Decl type_decl = {};
        type_decl.kind = DeclKind_Type;
        type_decl.type_decl.type_expr = type_expr_ref;

        DeclRef type_decl_ref =
            compiler->add_decl(type_decl_name, type_decl_loc, type_decl);
        top_level_decls->push_back(type_decl_ref);
        break;
    }
    default: {
        String token_string = token_to_string(compiler, next_token);
        compiler->add_error(
            next_token.loc,
            "unexpected token: '%.*s', expecting top level declaration",
            (int)token_string.len,
            token_string.ptr);
        compiler->halt_compilation();
        break;
    }
    }
}

void init_parser_tables()
{
    for (size_t i = 'a'; i <= 'g'; ++i) {
        EQ_CLASSES[i] = EqClass_LetterHex;
    }
    for (size_t i = 'A'; i <= 'G'; ++i) {
        EQ_CLASSES[i] = EqClass_LetterHex;
    }

    for (size_t i = 'g'; i <= 'z'; ++i) {
        EQ_CLASSES[i] = EqClass_Letter;
    }
    for (size_t i = 'G'; i <= 'Z'; ++i) {
        EQ_CLASSES[i] = EqClass_Letter;
    }

    EQ_CLASSES[(int)'x'] = EqClass_LetterX;

    for (size_t i = '0'; i <= '9'; ++i) {
        EQ_CLASSES[i] = EqClass_Digit;
    }
    EQ_CLASSES[(int)'0'] = EqClass_DigitZero;

    EQ_CLASSES[(int)'.'] = EqClass_Dot;
    EQ_CLASSES[(int)','] = EqClass_Comma;
    EQ_CLASSES[(int)':'] = EqClass_Colon;
    EQ_CLASSES[(int)';'] = EqClass_Semicolon;
    EQ_CLASSES[(int)'('] = EqClass_LParen;
    EQ_CLASSES[(int)')'] = EqClass_RParen;
    EQ_CLASSES[(int)'['] = EqClass_LBracket;
    EQ_CLASSES[(int)']'] = EqClass_RBracket;
    EQ_CLASSES[(int)'{'] = EqClass_LCurly;
    EQ_CLASSES[(int)'}'] = EqClass_RCurly;
    EQ_CLASSES[(int)'&'] = EqClass_Ampersand;
    EQ_CLASSES[(int)'|'] = EqClass_Pipe;
    EQ_CLASSES[(int)'~'] = EqClass_Tilde;
    EQ_CLASSES[(int)'^'] = EqClass_Caret;
    EQ_CLASSES[(int)'!'] = EqClass_Exclam;
    EQ_CLASSES[(int)'='] = EqClass_Equal;
    EQ_CLASSES[(int)'<'] = EqClass_Less;
    EQ_CLASSES[(int)'>'] = EqClass_Greater;
    EQ_CLASSES[(int)'+'] = EqClass_Plus;
    EQ_CLASSES[(int)'-'] = EqClass_Minus;
    EQ_CLASSES[(int)'*'] = EqClass_Asterisk;
    EQ_CLASSES[(int)'_'] = EqClass_Underscore;
    EQ_CLASSES[(int)'\''] = EqClass_SingleQuote;
    EQ_CLASSES[(int)'\"'] = EqClass_DoubleQuote;
    EQ_CLASSES[(int)'/'] = EqClass_Slash;
    EQ_CLASSES[(int)'\\'] = EqClass_BackSlash;
    EQ_CLASSES[(int)'%'] = EqClass_Percent;
    EQ_CLASSES[(int)'@'] = EqClass_At;
    EQ_CLASSES[(int)'#'] = EqClass_Pound;
    EQ_CLASSES[(int)'?'] = EqClass_Question;
    EQ_CLASSES[(int)'\0'] = EqClass_EOF;
    EQ_CLASSES[(int)' '] = EqClass_Whitespace;
    EQ_CLASSES[(int)'\t'] = EqClass_Whitespace;
    EQ_CLASSES[(int)'\n'] = EqClass_LineFeed;
    EQ_CLASSES[(int)'\r'] = EqClass_CarriageReturn;

    for (size_t i = 0; i < EqClass_COUNT; ++i) {
        EQ_CLASS_COLCOUNT_AND_MASK[i] = 0xffffffff;
    }
    EQ_CLASS_COLCOUNT_AND_MASK[EqClass_LineFeed] = 0;
    EQ_CLASS_COLCOUNT_OR_MASK[EqClass_LineFeed] = 1;

    end_transition(State_UWhitespace, State_Whitespace);
    TRANSITION[State_Start][EqClass_Whitespace] = State_UWhitespace;
    TRANSITION[State_Start][EqClass_LineFeed] = State_UWhitespace;
    TRANSITION[State_Start][EqClass_CarriageReturn] = State_UWhitespace;

    TRANSITION[State_UWhitespace][EqClass_Whitespace] = State_UWhitespace;
    TRANSITION[State_UWhitespace][EqClass_LineFeed] = State_UWhitespace;
    TRANSITION[State_UWhitespace][EqClass_CarriageReturn] = State_UWhitespace;

    for (size_t i = 0; i < EqClass_COUNT; ++i) {
        TRANSITION[State_UCommentBody][i] = State_UCommentBody;
    }
    TRANSITION[State_UCommentBody][EqClass_LineFeed] = State_UWhitespace;
    TRANSITION[State_UCommentBody][EqClass_CarriageReturn] = State_UWhitespace;

    end_transition(State_UIdentifier, State_Identifier);
    end_transition(State_UComptimeIdentifier, State_ComptimeIdentifier);
    end_transition(State_UBuiltinIdentifier, State_BuiltinIdentifier);
    end_transition(State_UIntLiteral, State_IntLiteral);
    end_transition(State_UZeroIntLiteral, State_IntLiteral);
    end_transition(State_UHexIntLiteral, State_HexIntLiteral);
    end_transition(State_UFloatLiteral, State_FloatLiteral);
    end_transition(State_UClosedStringLiteral, State_StringLiteral);
    end_transition(State_ULParen, State_LParen);
    end_transition(State_URParen, State_RParen);
    end_transition(State_ULBracket, State_LBracket);
    end_transition(State_URBracket, State_RBracket);
    end_transition(State_ULCurly, State_LCurly);
    end_transition(State_URCurly, State_RCurly);
    end_transition(State_UComma, State_Comma);
    end_transition(State_UColon, State_Colon);
    end_transition(State_USemicolon, State_Semicolon);
    end_transition(State_UQuestion, State_Question);
    end_transition(State_UDot, State_Dot);
    end_transition(State_UNot, State_Not);
    end_transition(State_UEqualEqual, State_EqualEqual);
    end_transition(State_UNotEqual, State_NotEqual);
    end_transition(State_ULess, State_Less);
    end_transition(State_ULessEqual, State_LessEqual);
    end_transition(State_UGreater, State_Greater);
    end_transition(State_UGreaterEqual, State_GreaterEqual);
    end_transition(State_UAssign, State_Assign);
    end_transition(State_UArrow, State_Arrow);
    end_transition(State_UBitAnd, State_BitAnd);
    end_transition(State_UBitOr, State_BitOr);
    end_transition(State_UBitNot, State_BitNot);
    end_transition(State_UBitXor, State_BitXor);
    end_transition(State_UBitAndEqual, State_BitAndEqual);
    end_transition(State_UBitOrEqual, State_BitOrEqual);
    end_transition(State_UBitNotEqual, State_BitNotEqual);
    end_transition(State_UBitXorEqual, State_BitXorEqual);
    end_transition(State_UAdd, State_Add);
    end_transition(State_USub, State_Sub);
    end_transition(State_UMul, State_Mul);
    end_transition(State_UDivOrComment, State_Div);
    end_transition(State_UMod, State_Mod);
    end_transition(State_UAddEqual, State_AddEqual);
    end_transition(State_USubEqual, State_SubEqual);
    end_transition(State_UMulEqual, State_MulEqual);
    end_transition(State_UDivEqual, State_DivEqual);
    end_transition(State_UModEqual, State_ModEqual);
    end_transition(State_ULShift, State_LShift);
    end_transition(State_URShift, State_RShift);
    end_transition(State_ULShiftEqual, State_LShiftEqual);
    end_transition(State_URShiftEqual, State_RShiftEqual);

    TRANSITION[State_Start][EqClass_EOF] = State_EOF;

    // Simple tokens
    TRANSITION[State_Start][EqClass_LParen] = State_ULParen;
    TRANSITION[State_Start][EqClass_RParen] = State_URParen;
    TRANSITION[State_Start][EqClass_LBracket] = State_ULBracket;
    TRANSITION[State_Start][EqClass_RBracket] = State_URBracket;
    TRANSITION[State_Start][EqClass_LCurly] = State_ULCurly;
    TRANSITION[State_Start][EqClass_RCurly] = State_URCurly;
    TRANSITION[State_Start][EqClass_Colon] = State_UColon;
    TRANSITION[State_Start][EqClass_Comma] = State_UComma;
    TRANSITION[State_Start][EqClass_Semicolon] = State_USemicolon;
    TRANSITION[State_Start][EqClass_Question] = State_UQuestion;

    TRANSITION[State_Start][EqClass_Exclam] = State_UNot;
    TRANSITION[State_Start][EqClass_Less] = State_ULess;
    TRANSITION[State_Start][EqClass_Greater] = State_UGreater;
    TRANSITION[State_Start][EqClass_Equal] = State_UAssign;
    TRANSITION[State_Start][EqClass_Ampersand] = State_UBitAnd;
    TRANSITION[State_Start][EqClass_Pipe] = State_UBitOr;
    TRANSITION[State_Start][EqClass_Tilde] = State_UBitNot;
    TRANSITION[State_Start][EqClass_Caret] = State_UBitXor;
    TRANSITION[State_Start][EqClass_Asterisk] = State_UMul;
    TRANSITION[State_Start][EqClass_Plus] = State_UAdd;
    TRANSITION[State_Start][EqClass_Minus] = State_USub;
    TRANSITION[State_Start][EqClass_Slash] = State_UDivOrComment;
    TRANSITION[State_Start][EqClass_Percent] = State_UMod;

    TRANSITION[State_UNot][EqClass_Equal] = State_UNotEqual;
    TRANSITION[State_ULess][EqClass_Equal] = State_ULessEqual;
    TRANSITION[State_ULess][EqClass_Less] = State_ULShift;
    TRANSITION[State_UGreater][EqClass_Greater] = State_URShift;
    TRANSITION[State_UGreater][EqClass_Equal] = State_UGreaterEqual;
    TRANSITION[State_UAssign][EqClass_Equal] = State_UEqualEqual;
    TRANSITION[State_UAdd][EqClass_Equal] = State_UAddEqual;
    TRANSITION[State_USub][EqClass_Equal] = State_USubEqual;
    TRANSITION[State_UMul][EqClass_Equal] = State_UMulEqual;
    TRANSITION[State_UDivOrComment][EqClass_Equal] = State_UDivEqual;
    TRANSITION[State_UDivOrComment][EqClass_Slash] = State_UCommentBody;
    TRANSITION[State_UMod][EqClass_Equal] = State_UModEqual;
    TRANSITION[State_UBitAnd][EqClass_Equal] = State_UBitAndEqual;
    TRANSITION[State_UBitOr][EqClass_Equal] = State_UBitOrEqual;
    TRANSITION[State_UBitNot][EqClass_Equal] = State_UBitNotEqual;
    TRANSITION[State_UBitXor][EqClass_Equal] = State_UBitXorEqual;
    TRANSITION[State_ULShift][EqClass_Equal] = State_ULShiftEqual;
    TRANSITION[State_URShift][EqClass_Equal] = State_URShiftEqual;

    // Identifier token
    TRANSITION[State_Start][EqClass_Letter] = State_UIdentifier;
    TRANSITION[State_Start][EqClass_LetterX] = State_UIdentifier;
    TRANSITION[State_Start][EqClass_LetterHex] = State_UIdentifier;
    TRANSITION[State_Start][EqClass_Underscore] = State_UIdentifier;
    TRANSITION[State_UIdentifier][EqClass_Letter] = State_UIdentifier;
    TRANSITION[State_UIdentifier][EqClass_LetterX] = State_UIdentifier;
    TRANSITION[State_UIdentifier][EqClass_LetterHex] = State_UIdentifier;
    TRANSITION[State_UIdentifier][EqClass_Digit] = State_UIdentifier;
    TRANSITION[State_UIdentifier][EqClass_DigitZero] = State_UIdentifier;
    TRANSITION[State_UIdentifier][EqClass_Underscore] = State_UIdentifier;

    // Comptime identifier token
    TRANSITION[State_Start][EqClass_Pound] = State_UComptimeIdentifier;
    TRANSITION[State_UComptimeIdentifier][EqClass_Letter] =
        State_UComptimeIdentifier;
    TRANSITION[State_UComptimeIdentifier][EqClass_LetterX] =
        State_UComptimeIdentifier;
    TRANSITION[State_UComptimeIdentifier][EqClass_LetterHex] =
        State_UComptimeIdentifier;

    // Builtin identifier token
    TRANSITION[State_Start][EqClass_At] = State_UBuiltinIdentifier;
    TRANSITION[State_UBuiltinIdentifier][EqClass_Letter] =
        State_UBuiltinIdentifier;
    TRANSITION[State_UBuiltinIdentifier][EqClass_LetterX] =
        State_UBuiltinIdentifier;
    TRANSITION[State_UBuiltinIdentifier][EqClass_LetterHex] =
        State_UBuiltinIdentifier;

    // String literal token
    TRANSITION[State_Start][EqClass_DoubleQuote] = State_UOpenStringLiteral;
    for (size_t i = 0; i < EqClass_COUNT; ++i) {
        TRANSITION[State_UOpenStringLiteral][i] = State_UOpenStringLiteral;
    }
    TRANSITION[State_UOpenStringLiteral][EqClass_EOF] = State_Error;
    TRANSITION[State_UOpenStringLiteral][EqClass_BackSlash] =
        State_UOpenStringLiteralSlashEscape;
    TRANSITION[State_UOpenStringLiteral][EqClass_DoubleQuote] =
        State_UClosedStringLiteral;
    for (size_t i = 0; i < EqClass_COUNT; ++i) {
        TRANSITION[State_UOpenStringLiteralSlashEscape][i] =
            State_UOpenStringLiteral;
    }

    // Numeric literal tokens
    TRANSITION[State_Start][EqClass_Digit] = State_UIntLiteral;
    TRANSITION[State_Start][EqClass_DigitZero] = State_UZeroIntLiteral;
    TRANSITION[State_UIntLiteral][EqClass_Digit] = State_UIntLiteral;
    TRANSITION[State_UIntLiteral][EqClass_DigitZero] = State_UIntLiteral;
    TRANSITION[State_UIntLiteral][EqClass_Dot] = State_UFloatLiteral;
    TRANSITION[State_UZeroIntLiteral][EqClass_LetterX] = State_UHexIntLiteral;
    TRANSITION[State_UZeroIntLiteral][EqClass_Digit] = State_UIntLiteral;
    TRANSITION[State_UZeroIntLiteral][EqClass_DigitZero] = State_UIntLiteral;
    TRANSITION[State_UZeroIntLiteral][EqClass_Dot] = State_UFloatLiteral;
    TRANSITION[State_UHexIntLiteral][EqClass_Digit] = State_UHexIntLiteral;
    TRANSITION[State_UHexIntLiteral][EqClass_DigitZero] = State_UHexIntLiteral;
    TRANSITION[State_UHexIntLiteral][EqClass_LetterHex] = State_UHexIntLiteral;
    TRANSITION[State_UFloatLiteral][EqClass_Digit] = State_UFloatLiteral;
    TRANSITION[State_UFloatLiteral][EqClass_DigitZero] = State_UFloatLiteral;
    TRANSITION[State_UFloatLiteral][EqClass_Dot] = State_Error;
    TRANSITION[State_UDot][EqClass_Digit] = State_UFloatLiteral;
    TRANSITION[State_UDot][EqClass_DigitZero] = State_UFloatLiteral;
    TRANSITION[State_Start][EqClass_Dot] = State_UDot;

    STATE_TOKENS[State_Error] = TokenKind_Error;
    STATE_TOKENS[State_Identifier] = TokenKind_Identifier;
    STATE_TOKENS[State_BuiltinIdentifier] = TokenKind_BuiltinIdentifier;
    STATE_TOKENS[State_StringLiteral] = TokenKind_StringLiteral;
    STATE_TOKENS[State_IntLiteral] = TokenKind_IntLiteral;
    STATE_TOKENS[State_FloatLiteral] = TokenKind_FloatLiteral;
    STATE_TOKENS[State_Dot] = TokenKind_Dot;
    STATE_TOKENS[State_Comma] = TokenKind_Comma;
    STATE_TOKENS[State_Colon] = TokenKind_Colon;
    STATE_TOKENS[State_Semicolon] = TokenKind_Semicolon;
    STATE_TOKENS[State_LParen] = TokenKind_LParen;
    STATE_TOKENS[State_RParen] = TokenKind_RParen;
    STATE_TOKENS[State_LBracket] = TokenKind_LBracket;
    STATE_TOKENS[State_RBracket] = TokenKind_RBracket;
    STATE_TOKENS[State_LCurly] = TokenKind_LCurly;
    STATE_TOKENS[State_RCurly] = TokenKind_RCurly;
    STATE_TOKENS[State_Not] = TokenKind_Not;
    STATE_TOKENS[State_Question] = TokenKind_Question;
    STATE_TOKENS[State_BitAnd] = TokenKind_BitAnd;
    STATE_TOKENS[State_BitOr] = TokenKind_BitOr;
    STATE_TOKENS[State_BitNot] = TokenKind_BitNot;
    STATE_TOKENS[State_BitXor] = TokenKind_BitXor;
    STATE_TOKENS[State_Mul] = TokenKind_Mul;
    STATE_TOKENS[State_Add] = TokenKind_Add;
    STATE_TOKENS[State_Sub] = TokenKind_Sub;
    STATE_TOKENS[State_Div] = TokenKind_Div;
    STATE_TOKENS[State_Mod] = TokenKind_Mod;
    STATE_TOKENS[State_BitAndEqual] = TokenKind_BitAndEqual;
    STATE_TOKENS[State_BitOrEqual] = TokenKind_BitOrEqual;
    STATE_TOKENS[State_BitNotEqual] = TokenKind_BitNotEqual;
    STATE_TOKENS[State_BitXorEqual] = TokenKind_BitXorEqual;
    STATE_TOKENS[State_MulEqual] = TokenKind_MulEqual;
    STATE_TOKENS[State_AddEqual] = TokenKind_AddEqual;
    STATE_TOKENS[State_SubEqual] = TokenKind_SubEqual;
    STATE_TOKENS[State_DivEqual] = TokenKind_DivEqual;
    STATE_TOKENS[State_ModEqual] = TokenKind_ModEqual;
    STATE_TOKENS[State_EqualEqual] = TokenKind_EqualEqual;
    STATE_TOKENS[State_NotEqual] = TokenKind_NotEqual;
    STATE_TOKENS[State_Less] = TokenKind_Less;
    STATE_TOKENS[State_LessEqual] = TokenKind_LessEqual;
    STATE_TOKENS[State_Greater] = TokenKind_Greater;
    STATE_TOKENS[State_GreaterEqual] = TokenKind_GreaterEqual;
    STATE_TOKENS[State_LShift] = TokenKind_LShift;
    STATE_TOKENS[State_RShift] = TokenKind_RShift;
    STATE_TOKENS[State_LShiftEqual] = TokenKind_LShiftEqual;
    STATE_TOKENS[State_RShiftEqual] = TokenKind_RShiftEqual;
    STATE_TOKENS[State_Arrow] = TokenKind_Arrow;
    STATE_TOKENS[State_Assign] = TokenKind_Equal;
    STATE_TOKENS[State_EOF] = TokenKind_EOF;
}

void parse_file(Compiler *compiler, FileRef file_ref)
{
    ZoneScoped;

    File file = compiler->files[file_ref.id];

    ParserState parser_state = {};
    parser_state.tokens = Array<Token>::create(MallocAllocator::get_instance());

    {
        TokenizerState state =
            TokenizerState::create(file_ref, file.text.ptr, file.text.len);

        Token token = {};
        while (token.kind != TokenKind_EOF) {
            state = state.next_token(compiler, &token);
            parser_state.tokens.push_back(token);
        }
    }

    while (parser_state.peek_token().kind != TokenKind_EOF) {
        Token next_token = parser_state.peek_token();
        if (next_token.kind == TokenKind_Error) {
            String token_string = token_to_string(compiler, next_token);
            compiler->add_error(
                next_token.loc,
                "unexpected token: '%.*s'",
                (int)token_string.len,
                token_string.ptr);
            compiler->halt_compilation();
        }

        parse_top_level_decl(compiler, &parser_state, &file.top_level_decls);
    }

    file.line_count = parser_state.tokens[parser_state.tokens.len - 1].loc.line;

    compiler->files[file_ref.id] = file;

    parser_state.tokens.destroy();

    if (compiler->errors.len > 0) {
        compiler->halt_compilation();
    }
}
