#include "compiler.hpp"
#include <Tracy.hpp>

static inline SIRString token_kind_to_string(TokenKind kind)
{
    switch (kind) {
    case TokenKind_Unknown: return SIR_STR("<unknown>");
    case TokenKind_Error: return SIR_STR("<error>");

    case TokenKind_LParen: return SIR_STR("(");
    case TokenKind_RParen: return SIR_STR(")");
    case TokenKind_LBracket: return SIR_STR("[");
    case TokenKind_RBracket: return SIR_STR("]");
    case TokenKind_LCurly: return SIR_STR("{");
    case TokenKind_RCurly: return SIR_STR("}");

    case TokenKind_Colon: return SIR_STR(":");
    case TokenKind_Comma: return SIR_STR(",");
    case TokenKind_Dot: return SIR_STR(".");
    case TokenKind_Semicolon: return SIR_STR(";");
    case TokenKind_Question: return SIR_STR("?");

    case TokenKind_Equal: return SIR_STR("=");

    case TokenKind_And: return SIR_STR("and");
    case TokenKind_Or: return SIR_STR("or");

    case TokenKind_Sub: return SIR_STR("-");
    case TokenKind_Add: return SIR_STR("+");
    case TokenKind_Mul: return SIR_STR("*");
    case TokenKind_Div: return SIR_STR("/");
    case TokenKind_Mod: return SIR_STR("%");
    case TokenKind_Arrow: return SIR_STR("=>");

    case TokenKind_Not: return SIR_STR("!");
    case TokenKind_BitAnd: return SIR_STR("&");
    case TokenKind_BitOr: return SIR_STR("|");
    case TokenKind_BitXor: return SIR_STR("^");
    case TokenKind_BitNot: return SIR_STR("~");

    case TokenKind_EqualEqual: return SIR_STR("==");
    case TokenKind_NotEqual: return SIR_STR("!=");
    case TokenKind_Less: return SIR_STR("<");
    case TokenKind_LessEqual: return SIR_STR("<=");
    case TokenKind_Greater: return SIR_STR(">");
    case TokenKind_GreaterEqual: return SIR_STR(">=");

    case TokenKind_LShift: return SIR_STR("<<");
    case TokenKind_RShift: return SIR_STR(">>");

    case TokenKind_AddEqual: return SIR_STR("+=");
    case TokenKind_SubEqual: return SIR_STR("-=");
    case TokenKind_MulEqual: return SIR_STR("*=");
    case TokenKind_DivEqual: return SIR_STR("/=");
    case TokenKind_ModEqual: return SIR_STR("%=");

    case TokenKind_BitAndEqual: return SIR_STR("&=");
    case TokenKind_BitOrEqual: return SIR_STR("|=");
    case TokenKind_BitXorEqual: return SIR_STR("^=");
    case TokenKind_BitNotEqual: return SIR_STR("~=");
    case TokenKind_LShiftEqual: return SIR_STR("<<=");
    case TokenKind_RShiftEqual: return SIR_STR(">>=");

    case TokenKind_Const: return SIR_STR("const");
    case TokenKind_Extern: return SIR_STR("extern");
    case TokenKind_Export: return SIR_STR("export");
    case TokenKind_VarArg: return SIR_STR("vararg");
    case TokenKind_Global: return SIR_STR("global");
    case TokenKind_Inline: return SIR_STR("inline");
    case TokenKind_Macro: return SIR_STR("macro");
    case TokenKind_Def: return SIR_STR("def");
    case TokenKind_Type: return SIR_STR("type");
    case TokenKind_Struct: return SIR_STR("struct");
    case TokenKind_Union: return SIR_STR("union");
    case TokenKind_If: return SIR_STR("if");
    case TokenKind_Else: return SIR_STR("else");
    case TokenKind_While: return SIR_STR("while");
    case TokenKind_Break: return SIR_STR("break");
    case TokenKind_Continue: return SIR_STR("continue");
    case TokenKind_Return: return SIR_STR("return");
    case TokenKind_Void: return SIR_STR("void");
    case TokenKind_Bool: return SIR_STR("bool");
    case TokenKind_True: return SIR_STR("true");
    case TokenKind_False: return SIR_STR("false");
    case TokenKind_Null: return SIR_STR("null");
    case TokenKind_U8: return SIR_STR("u8");
    case TokenKind_U16: return SIR_STR("u16");
    case TokenKind_U32: return SIR_STR("u32");
    case TokenKind_U64: return SIR_STR("u64");
    case TokenKind_I8: return SIR_STR("i8");
    case TokenKind_I16: return SIR_STR("i16");
    case TokenKind_I32: return SIR_STR("i32");
    case TokenKind_I64: return SIR_STR("i64");
    case TokenKind_F32: return SIR_STR("f32");
    case TokenKind_F64: return SIR_STR("f64");
    case TokenKind_Identifier: return SIR_STR("<identifier>");
    case TokenKind_BuiltinIdentifier: return SIR_STR("<builtin identifier>");
    case TokenKind_StringLiteral: return SIR_STR("<string literal>");
    case TokenKind_CharLiteral: return SIR_STR("<character literal>");
    case TokenKind_IntLiteral: return SIR_STR("<integer literal>");
    case TokenKind_FloatLiteral: return SIR_STR("<float literal>");

    case TokenKind_EOF: return SIR_STR("<eof>");
    }

    return SIR_STR("<unknown>");
}

static SIRString token_to_string(Compiler *compiler, const Token &token)
{
    switch (token.kind) {
    case TokenKind_Error:
        return SIRAllocSprintf(
            compiler->arena, "error: %.*s", (int)token.str.len, token.str.ptr);
    case TokenKind_StringLiteral:
        return SIRAllocSprintf(
            compiler->arena,
            "string literal: \"%.*s\"",
            (int)token.str.len,
            token.str.ptr);
    case TokenKind_Identifier:
        return SIRAllocSprintf(
            compiler->arena,
            "identifier: \"%.*s\"",
            (int)token.str.len,
            token.str.ptr);
    default: return token_kind_to_string(token.kind);
    }

    SIR_ASSERT(0);
    return SIR_STR("");
}

SIR_INLINE static bool is_whitespace(char c)
{
    return c == ' ' || c == '\t' || c == '\r' || c == '\n';
}

SIR_INLINE static bool is_alpha(char c)
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_');
}

SIR_INLINE static bool is_alpha_num(char c)
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_') ||
           (c >= '0' && c <= '9');
}

SIR_INLINE static bool is_hex(char c)
{
    return (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') ||
           (c >= '0' && c <= '9');
}

SIR_INLINE static bool is_num(char c)
{
    return (c >= '0' && c <= '9');
}

enum EqClass : uint8_t {
    EqClass_Other = 0,
    EqClass_Whitespace,
    EqClass_LineFeed,       // \n
    EqClass_CarriageReturn, // \r
    EqClass_Letter,         // a..z A..Z
    EqClass_Digit,          // 0..9
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
    EqClass_EOF,            // \0
    EqClass_COUNT,
};

enum State : uint8_t {
    State_Error = 0,

    State_Whitespace,
    State_WhitespaceFinal,
    State_WhitespaceStart,
    State_UWhitespace,
    State_UCommentSlash,
    State_UCommentBody,

    State_Identifier,
    State_BuiltinIdentifier,
    State_StringLiteral,
    State_IntLiteral,
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

    State_UIdentifier,
    State_UBuiltinIdentifier,
    State_UIntLiteral,
    State_UFloatLiteral,
    State_UOpenStringLiteral,
    State_UOpenStringLiteralSlashEscape,
    State_UClosedStringLiteral,
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
    State_UDiv,
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
static uint32_t EQ_CLASS_LINE_INC[EqClass_COUNT];
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

    int64_t length_left(File *file, size_t offset = 0)
    {
        return ((int64_t)file->text.len) - (int64_t)(this->pos + offset);
    }

    Token consume_token(Compiler *compiler, TokenKind token_kind)
    {
        Token token = {};
        *this = this->next_token(compiler, &token);
        if (token.kind != token_kind) {
            SIRString token_string = token_to_string(compiler, token);
            if (token.kind == TokenKind_Error) {
                compiler->add_error(
                    token.loc,
                    "unexpected token: '%.*s'",
                    (int)token_string.len,
                    token_string.ptr);
                compiler->halt_compilation();
            }

            SIRString expected_token_string = token_kind_to_string(token_kind);

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

    TokenizerState next_token(Compiler *compiler, Token *token) const
    {
        ZoneScoped;

        TokenizerState state = *this;
        SIRAllocator *allocator = (SIRAllocator *)compiler->arena;

        State mstate = State_WhitespaceStart;

        do {
            char c = state.text[state.pos++];
            EqClass eq_class = EQ_CLASSES[(int)c];
            mstate = TRANSITION[mstate][eq_class];
            state.line += EQ_CLASS_LINE_INC[eq_class];
            state.col++;
            state.col &= EQ_CLASS_COLCOUNT_AND_MASK[eq_class];
            state.col |= EQ_CLASS_COLCOUNT_OR_MASK[eq_class];
        } while (mstate > State_WhitespaceFinal);

        state.col--;
        state.pos--;

        token->loc.file_ref = state.file_ref;
        token->loc.offset = state.pos;
        token->loc.col = state.col;
        token->loc.line = state.line;

        mstate = State_Start;

        do {
            char c = state.text[state.pos++];
            EqClass eq_class = EQ_CLASSES[(int)c];
            mstate = TRANSITION[mstate][eq_class];
        } while (mstate > State_Final);

        token->loc.len = state.pos - token->loc.offset;
        if (token->loc.len > 1) {
            token->loc.len--;
            state.pos--;
        }

        state.col += token->loc.len;
        token->kind = STATE_TOKENS[mstate];

        switch (token->kind) {
        case TokenKind_Identifier: {
            SIRString ident_str =
                SIRString{&state.text[token->loc.offset], token->loc.len};
            uintptr_t token_kind = TokenKind_Identifier;
            if (!SIRStringMapGet(
                    &compiler->keyword_map, ident_str, &token_kind)) {
                token->str = ident_str;
            }
            token->kind = (TokenKind)token_kind;
            break;
        }
        case TokenKind_BuiltinIdentifier: {
            SIRString ident_str = SIRString{
                &state.text[token->loc.offset + 1], token->loc.len - 1};
            token->str = ident_str;
            break;
        }
        case TokenKind_StringLiteral: {
            SIRString ident_str = SIRString{
                &state.text[token->loc.offset + 1], token->loc.len - 2};

            compiler->sb.reset();
            for (size_t i = 0; i < ident_str.len; ++i) {
                if (ident_str.ptr[i] == '\\') {
                    if (i + 1 >= ident_str.len) {
                        token->kind = TokenKind_Error;
                        token->str = SIR_STR("invalid string literal");
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

            token->str = compiler->sb.build_null_terminated(
                (SIRAllocator *)compiler->arena);
            break;
        }
        case TokenKind_FloatLiteral: {
            SIRString ident_str =
                SIRString{&state.text[token->loc.offset], token->loc.len};
            const char *strz = SIRAllocNullTerminate(allocator, ident_str);
            token->float_ = strtod(strz, NULL);
            break;
        }
        case TokenKind_IntLiteral: {
            SIRString ident_str =
                SIRString{&state.text[token->loc.offset], token->loc.len};
            const char *strz = SIRAllocNullTerminate(allocator, ident_str);
            token->int_ = strtol(strz, NULL, 10);
            break;
        }

        default: break;
        }

    end:
        return state;
    }
};

static Expr parse_expr(Compiler *compiler, TokenizerState *state);
static Stmt parse_stmt(Compiler *compiler, TokenizerState *state);

static Expr parse_primary_expr(Compiler *compiler, TokenizerState *state)
{
    ZoneScoped;

    Expr expr = {};

    Token next_token = {};
    state->next_token(compiler, &next_token);

    switch (next_token.kind) {
    case TokenKind_LParen: {
        state->consume_token(compiler, TokenKind_LParen);
        expr = parse_expr(compiler, state);
        state->consume_token(compiler, TokenKind_RParen);
        break;
    }
    case TokenKind_Identifier: {
        Token ident_token =
            state->consume_token(compiler, TokenKind_Identifier);
        expr.kind = ExprKind_Identifier;
        expr.loc = ident_token.loc;
        expr.ident.str = ident_token.str;
        break;
    }
    case TokenKind_StringLiteral: {
        Token str_token =
            state->consume_token(compiler, TokenKind_StringLiteral);
        expr.kind = ExprKind_StringLiteral;
        expr.loc = str_token.loc;
        expr.str_literal.str = str_token.str;
        break;
    }
    case TokenKind_IntLiteral: {
        Token int_token = state->consume_token(compiler, TokenKind_IntLiteral);
        expr.kind = ExprKind_IntLiteral;
        expr.loc = int_token.loc;
        expr.int_literal.i64 = int_token.int_;
        break;
    }
    case TokenKind_FloatLiteral: {
        Token float_token =
            state->consume_token(compiler, TokenKind_FloatLiteral);
        expr.kind = ExprKind_FloatLiteral;
        expr.loc = float_token.loc;
        expr.float_literal.f64 = float_token.float_;
        break;
    }
    case TokenKind_True: {
        *state = state->next_token(compiler, &next_token);

        expr.kind = ExprKind_BoolLiteral;
        expr.loc = next_token.loc;
        expr.bool_literal.bool_ = true;
        break;
    }
    case TokenKind_False: {
        *state = state->next_token(compiler, &next_token);

        expr.kind = ExprKind_BoolLiteral;
        expr.loc = next_token.loc;
        expr.bool_literal.bool_ = false;
        break;
    }
    case TokenKind_Null: {
        *state = state->next_token(compiler, &next_token);

        expr.kind = ExprKind_NullLiteral;
        expr.loc = next_token.loc;
        break;
    }
    case TokenKind_Mul: {
        Token asterisk_token = state->consume_token(compiler, TokenKind_Mul);

        Expr sub_expr = parse_expr(compiler, state);
        ExprRef sub_expr_ref = compiler->add_expr(sub_expr);

        expr.kind = ExprKind_PointerType;
        expr.loc = asterisk_token.loc;
        expr.ptr_type.sub_expr_ref = sub_expr_ref;
        break;
    }
    case TokenKind_LBracket: {
        Token lbracket_token =
            state->consume_token(compiler, TokenKind_LBracket);

        state->next_token(compiler, &next_token);
        if (next_token.kind != TokenKind_RBracket) {
            Expr size_expr = parse_expr(compiler, state);
            state->consume_token(compiler, TokenKind_RBracket);

            Expr subtype_expr = parse_expr(compiler, state);

            expr.kind = ExprKind_ArrayType;
            expr.loc = lbracket_token.loc;
            expr.array_type.size_expr_ref = compiler->add_expr(size_expr);
            expr.array_type.subtype_expr_ref = compiler->add_expr(subtype_expr);
        } else {
            state->consume_token(compiler, TokenKind_RBracket);

            Expr subtype_expr = parse_expr(compiler, state);

            expr.kind = ExprKind_SliceType;
            expr.loc = lbracket_token.loc;
            expr.slice_type.subtype_expr_ref = compiler->add_expr(subtype_expr);
        }
        break;
    }
    case TokenKind_Void: {
        *state = state->next_token(compiler, &next_token);

        expr.kind = ExprKind_VoidType;
        expr.loc = next_token.loc;
        break;
    }
    case TokenKind_Bool: {
        *state = state->next_token(compiler, &next_token);

        expr.kind = ExprKind_BoolType;
        expr.loc = next_token.loc;
        break;
    }
    case TokenKind_U8:
    case TokenKind_U16:
    case TokenKind_U32:
    case TokenKind_U64: {
        *state = state->next_token(compiler, &next_token);

        expr.kind = ExprKind_IntType;
        expr.loc = next_token.loc;
        expr.int_type.is_signed = false;

        switch (next_token.kind) {
        case TokenKind_U8: expr.int_type.bits = 8; break;
        case TokenKind_U16: expr.int_type.bits = 16; break;
        case TokenKind_U32: expr.int_type.bits = 32; break;
        case TokenKind_U64: expr.int_type.bits = 64; break;
        default: SIR_ASSERT(0);
        }

        break;
    }
    case TokenKind_I8:
    case TokenKind_I16:
    case TokenKind_I32:
    case TokenKind_I64: {
        *state = state->next_token(compiler, &next_token);

        expr.kind = ExprKind_IntType;
        expr.loc = next_token.loc;
        expr.int_type.is_signed = true;

        switch (next_token.kind) {
        case TokenKind_I8: expr.int_type.bits = 8; break;
        case TokenKind_I16: expr.int_type.bits = 16; break;
        case TokenKind_I32: expr.int_type.bits = 32; break;
        case TokenKind_I64: expr.int_type.bits = 64; break;
        default: SIR_ASSERT(0);
        }

        break;
    }
    case TokenKind_F32:
    case TokenKind_F64: {
        *state = state->next_token(compiler, &next_token);

        expr.kind = ExprKind_FloatType;
        expr.loc = next_token.loc;

        switch (next_token.kind) {
        case TokenKind_F32: expr.float_type.bits = 32; break;
        case TokenKind_F64: expr.float_type.bits = 64; break;
        default: SIR_ASSERT(0);
        }

        break;
    }
    case TokenKind_BuiltinIdentifier: {
        Token ident_token =
            state->consume_token(compiler, TokenKind_BuiltinIdentifier);

        state->consume_token(compiler, TokenKind_LParen);

        uintptr_t builtin_func_id = BuiltinFunction_Unknown;
        if (!SIRStringMapGet(
                &compiler->builtin_function_map,
                ident_token.str,
                &builtin_func_id)) {
            compiler->add_error(
                next_token.loc,
                "invalid builtin function: '@%.*s'",
                (int)ident_token.str.len,
                ident_token.str.ptr);
        }

        BuiltinFunction builtin_func = (BuiltinFunction)builtin_func_id;

        expr.kind = ExprKind_BuiltinCall;
        expr.loc = ident_token.loc;
        expr.builtin_call.builtin = builtin_func;
        expr.builtin_call.param_refs =
            SIRArray<ExprRef>::create((SIRAllocator *)compiler->arena);

        state->next_token(compiler, &next_token);
        while (next_token.kind != TokenKind_RParen) {
            Expr param_expr = parse_expr(compiler, state);
            ExprRef param_expr_ref = compiler->add_expr(param_expr);

            expr.builtin_call.param_refs.push_back(param_expr_ref);

            state->next_token(compiler, &next_token);
            if (next_token.kind != TokenKind_RParen) {
                state->consume_token(compiler, TokenKind_Comma);
            }

            state->next_token(compiler, &next_token);
        }

        state->consume_token(compiler, TokenKind_RParen);
        break;
    }
    case TokenKind_Struct: {
        Token struct_token = state->consume_token(compiler, TokenKind_Struct);

        expr.kind = ExprKind_StructType;
        expr.loc = struct_token.loc;
        expr.struct_type.field_names =
            SIRArray<SIRString>::create((SIRAllocator *)compiler->arena);
        expr.struct_type.field_type_expr_refs =
            SIRArray<ExprRef>::create((SIRAllocator *)compiler->arena);

        state->consume_token(compiler, TokenKind_LCurly);

        state->next_token(compiler, &next_token);
        while (next_token.kind != TokenKind_RCurly) {
            Token field_ident_token =
                state->consume_token(compiler, TokenKind_Identifier);

            Expr field_type_expr = parse_expr(compiler, state);

            expr.struct_type.field_names.push_back(field_ident_token.str);
            expr.struct_type.field_type_expr_refs.push_back(
                compiler->add_expr(field_type_expr));

            state->next_token(compiler, &next_token);
            if (next_token.kind != TokenKind_RCurly) {
                state->consume_token(compiler, TokenKind_Comma);
            }

            state->next_token(compiler, &next_token);
        }

        state->consume_token(compiler, TokenKind_RCurly);
        break;
    }
    default: {
        SIRString token_string = token_to_string(compiler, next_token);
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

static Expr parse_func_expr(Compiler *compiler, TokenizerState *state)
{
    ZoneScoped;

    Expr expr = {};

    Token next_token = {};
    state->next_token(compiler, &next_token);

    expr.loc = next_token.loc;
    expr.func = {};

    expr.func.param_decl_refs =
        SIRArray<DeclRef>::create((SIRAllocator *)compiler->arena);
    expr.func.return_type_expr_refs =
        SIRArray<ExprRef>::create((SIRAllocator *)compiler->arena);

    switch (next_token.kind) {
    case TokenKind_Inline: {
        state->consume_token(compiler, TokenKind_Inline);
        expr.func.flags |= FunctionFlags_Inline;
        break;
    }
    default: {
        SIRString token_string = token_to_string(compiler, next_token);
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

static Expr parse_func_call_expr(Compiler *compiler, TokenizerState *state)
{
    ZoneScoped;

    Expr expr = parse_primary_expr(compiler, state);

    Token next_token = {};
    state->next_token(compiler, &next_token);
    while (1) {
        switch (next_token.kind) {
        case TokenKind_LBracket: {
            state->consume_token(compiler, TokenKind_LBracket);

            Expr indexed_expr = expr;
            ExprRef indexed_expr_ref = compiler->add_expr(indexed_expr);

            expr = {};
            expr.loc = indexed_expr.loc;
            expr.kind = ExprKind_Subscript;
            expr.subscript.left_ref = indexed_expr_ref;
            expr.subscript.right_ref =
                compiler->add_expr(parse_expr(compiler, state));

            state->consume_token(compiler, TokenKind_RBracket);
            break;
        }
        case TokenKind_LParen: {
            state->consume_token(compiler, TokenKind_LParen);

            Expr func_expr = expr;
            ExprRef func_expr_ref = compiler->add_expr(func_expr);

            expr = {};
            expr.loc = func_expr.loc;
            expr.kind = ExprKind_FunctionCall;
            expr.func_call.func_expr_ref = func_expr_ref;
            expr.func_call.param_refs =
                SIRArray<ExprRef>::create((SIRAllocator *)compiler->arena);

            state->next_token(compiler, &next_token);
            while (next_token.kind != TokenKind_RParen) {
                Expr param_expr = parse_expr(compiler, state);
                ExprRef param_expr_ref = compiler->add_expr(param_expr);

                expr.func_call.param_refs.push_back(param_expr_ref);

                state->next_token(compiler, &next_token);
                if (next_token.kind != TokenKind_RParen) {
                    state->consume_token(compiler, TokenKind_Comma);
                }

                state->next_token(compiler, &next_token);
            }

            state->consume_token(compiler, TokenKind_RParen);
            break;
        }
        case TokenKind_Dot: {
            Token dot_tok = state->consume_token(compiler, TokenKind_Dot);

            state->next_token(compiler, &next_token);
            if (next_token.kind == TokenKind_Mul) {
                state->consume_token(compiler, TokenKind_Mul);

                Expr sub_expr = expr;
                ExprRef sub_expr_ref = compiler->add_expr(sub_expr);

                expr = {};
                expr.loc = dot_tok.loc;
                expr.kind = ExprKind_Unary;
                expr.unary.op = UnaryOp_Dereference;
                expr.unary.left_ref = sub_expr_ref;
            } else {
                Expr left_expr = expr;
                Token ident_tok =
                    state->consume_token(compiler, TokenKind_Identifier);

                Expr accessed_ident_expr = {};
                accessed_ident_expr.kind = ExprKind_Identifier;
                accessed_ident_expr.loc = ident_tok.loc;
                accessed_ident_expr.ident.str = ident_tok.str;

                ExprRef left_expr_ref = compiler->add_expr(left_expr);
                ExprRef right_expr_ref =
                    compiler->add_expr(accessed_ident_expr);

                expr = {};
                expr.loc = dot_tok.loc;
                expr.kind = ExprKind_Access;
                expr.access.left_ref = left_expr_ref;
                expr.access.accessed_ident_ref = right_expr_ref;
            }

            break;
        }
        default: goto end;
        }

        state->next_token(compiler, &next_token);
    }

end:

    return expr;
}

static Expr parse_unary_expr(Compiler *compiler, TokenizerState *state)
{
    ZoneScoped;

    Token next_token = {};
    state->next_token(compiler, &next_token);
    switch (next_token.kind) {
    case TokenKind_Sub:
    case TokenKind_Not:
    case TokenKind_BitAnd: {
        *state = state->next_token(compiler, &next_token);

        UnaryOp op = {};

        switch (next_token.kind) {
        case TokenKind_Sub: op = UnaryOp_Negate; break;
        case TokenKind_Not: op = UnaryOp_Not; break;
        case TokenKind_BitAnd: op = UnaryOp_AddressOf; break;
        default: SIR_ASSERT(0); break;
        }

        Expr expr = {};
        expr.kind = ExprKind_Unary;
        expr.loc = next_token.loc;
        expr.unary.left_ref =
            compiler->add_expr(parse_unary_expr(compiler, state));
        expr.unary.op = op;

        return expr;
    }

    default: break;
    }

    return parse_func_call_expr(compiler, state);
}

enum BinaryOpSymbolKind {
    BinaryOpSymbol_Expr,
    BinaryOpSymbol_Operator,
};

struct BinaryOpSymbol {
    BinaryOpSymbolKind kind;
    union {
        Expr expr;
        BinaryOp op;
    };
};

static Expr parse_binary_expr(Compiler *compiler, TokenizerState *state)
{
    ZoneScoped;

    Expr expr = parse_unary_expr(compiler, state);

    Token next_token = {};
    state->next_token(compiler, &next_token);
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
    case TokenKind_LessEqual: break;
    default: return expr;
    }

    SIRArray<BinaryOp> op_stack =
        SIRArray<BinaryOp>::create(&SIR_MALLOC_ALLOCATOR);
    SIRArray<BinaryOpSymbol> symbol_queue =
        SIRArray<BinaryOpSymbol>::create(&SIR_MALLOC_ALLOCATOR);

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
    };

    {
        BinaryOpSymbol expr_symbol = {};
        expr_symbol.kind = BinaryOpSymbol_Expr;
        expr_symbol.expr = expr;
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

        Expr right_expr = parse_unary_expr(compiler, state);

        {
            BinaryOpSymbol expr_symbol = {};
            expr_symbol.kind = BinaryOpSymbol_Expr;
            expr_symbol.expr = right_expr;
            symbol_queue.push_back(expr_symbol);
        }

        state->next_token(compiler, &next_token);
    }

    while (op_stack.len > 0) {
        BinaryOp popped_op = op_stack[op_stack.len - 1];
        op_stack.pop();

        BinaryOpSymbol op_symbol = {};
        op_symbol.kind = BinaryOpSymbol_Operator;
        op_symbol.op = popped_op;
        symbol_queue.push_back(op_symbol);
    }

    SIRArray<Expr> expr_stack = SIRArray<Expr>::create(&SIR_MALLOC_ALLOCATOR);

    for (size_t i = 0; i < symbol_queue.len; ++i) {
        BinaryOpSymbol symbol = symbol_queue[i];
        if (symbol.kind == BinaryOpSymbol_Operator) {
            SIR_ASSERT(expr_stack.len >= 2);
            Expr right_expr = expr_stack[expr_stack.len - 1];
            Expr left_expr = expr_stack[expr_stack.len - 2];
            expr_stack.pop();
            expr_stack.pop();

            Expr bin_expr = {};
            bin_expr.kind = ExprKind_Binary;
            bin_expr.loc = left_expr.loc;
            bin_expr.binary.op = symbol.op;
            bin_expr.binary.left_ref = compiler->add_expr(left_expr);
            bin_expr.binary.right_ref = compiler->add_expr(right_expr);

            expr_stack.push_back(bin_expr);
        } else {
            expr_stack.push_back(symbol.expr);
        }
    }

    SIR_ASSERT(expr_stack.len == 1);

    Expr result_expr = expr_stack[0];

    op_stack.destroy();
    symbol_queue.destroy();
    expr_stack.destroy();

    return result_expr;
}

static Expr parse_expr(Compiler *compiler, TokenizerState *state)
{
    ZoneScoped;

    return parse_binary_expr(compiler, state);
}

static Stmt parse_stmt(Compiler *compiler, TokenizerState *state)
{
    ZoneScoped;

    Stmt stmt = {};

    Token next_token = {};
    state->next_token(compiler, &next_token);

    switch (next_token.kind) {
    case TokenKind_If: {
        Token if_token = state->consume_token(compiler, TokenKind_If);
        stmt.kind = StmtKind_If;
        stmt.loc = if_token.loc;
        stmt.if_ = {};

        state->consume_token(compiler, TokenKind_LParen);

        stmt.if_.cond_expr_ref =
            compiler->add_expr(parse_expr(compiler, state));

        state->consume_token(compiler, TokenKind_RParen);

        stmt.if_.true_stmt_ref =
            compiler->add_stmt(parse_stmt(compiler, state));

        state->next_token(compiler, &next_token);
        if (next_token.kind == TokenKind_Else) {
            state->consume_token(compiler, TokenKind_Else);

            stmt.if_.false_stmt_ref =
                compiler->add_stmt(parse_stmt(compiler, state));
        }
        break;
    }
    case TokenKind_While: {
        Token while_token = state->consume_token(compiler, TokenKind_While);
        stmt.kind = StmtKind_While;
        stmt.loc = while_token.loc;
        stmt.while_ = {};

        state->consume_token(compiler, TokenKind_LParen);

        stmt.while_.cond_expr_ref =
            compiler->add_expr(parse_expr(compiler, state));

        state->consume_token(compiler, TokenKind_RParen);

        stmt.while_.true_stmt_ref =
            compiler->add_stmt(parse_stmt(compiler, state));
        break;
    }
    case TokenKind_Return: {
        Token return_token = state->consume_token(compiler, TokenKind_Return);
        stmt.kind = StmtKind_Return;
        stmt.loc = return_token.loc;
        stmt.return_ = {};

        state->next_token(compiler, &next_token);
        if (next_token.kind != TokenKind_Semicolon) {
            stmt.return_.returned_expr_ref =
                compiler->add_expr(parse_expr(compiler, state));
        }

        state->consume_token(compiler, TokenKind_Semicolon);

        break;
    }
    case TokenKind_LCurly: {
        Token lcurly_token = state->consume_token(compiler, TokenKind_LCurly);

        stmt.kind = StmtKind_Block;
        stmt.loc = lcurly_token.loc;
        stmt.block.stmt_refs =
            SIRArray<StmtRef>::create((SIRAllocator *)compiler->arena);

        state->next_token(compiler, &next_token);
        while (next_token.kind != TokenKind_RCurly) {
            Stmt sub_stmt = parse_stmt(compiler, state);
            StmtRef sub_stmt_ref = compiler->add_stmt(sub_stmt);

            stmt.block.stmt_refs.push_back(sub_stmt_ref);

            state->next_token(compiler, &next_token);
        }

        state->consume_token(compiler, TokenKind_RCurly);
        break;
    }
    case TokenKind_Global: {
        state->consume_token(compiler, TokenKind_Global);

        Token ident_tok = state->consume_token(compiler, TokenKind_Identifier);

        state->consume_token(compiler, TokenKind_Colon);

        ExprRef type_expr_ref = {0};
        ExprRef value_expr_ref = {0};

        state->next_token(compiler, &next_token);
        if (next_token.kind != TokenKind_Equal) {
            type_expr_ref = compiler->add_expr(parse_expr(compiler, state));

            state->next_token(compiler, &next_token);
            if (next_token.kind == TokenKind_Equal) {
                state->consume_token(compiler, TokenKind_Equal);

                value_expr_ref =
                    compiler->add_expr(parse_expr(compiler, state));
            }
        } else {
            state->consume_token(compiler, TokenKind_Equal);

            value_expr_ref = compiler->add_expr(parse_expr(compiler, state));
        }

        SIR_ASSERT(type_expr_ref.id > 0 || value_expr_ref.id > 0);

        Decl var_decl = {};
        var_decl.kind = DeclKind_GlobalVarDecl;
        var_decl.loc = ident_tok.loc;
        var_decl.name = ident_tok.str;
        var_decl.local_var_decl.type_expr = type_expr_ref;
        var_decl.local_var_decl.value_expr = value_expr_ref;

        state->consume_token(compiler, TokenKind_Semicolon);

        stmt.kind = StmtKind_Decl;
        stmt.loc = var_decl.loc;
        stmt.decl.decl_ref = compiler->add_decl(var_decl);

        break;
    }
    case TokenKind_Type: {
        state->consume_token(compiler, TokenKind_Type);

        Token ident_tok = state->consume_token(compiler, TokenKind_Identifier);

        ExprRef type_expr_ref = compiler->add_expr(parse_expr(compiler, state));

        state->consume_token(compiler, TokenKind_Semicolon);

        Decl type_decl = {};
        type_decl.kind = DeclKind_Type;
        type_decl.loc = ident_tok.loc;
        type_decl.name = ident_tok.str;
        type_decl.type_decl.type_expr = type_expr_ref;

        stmt.kind = StmtKind_Decl;
        stmt.loc = type_decl.loc;
        stmt.decl.decl_ref = compiler->add_decl(type_decl);

        break;
    }
    default: {
        Token ident_tok = {};
        Token colon_tok = {};
        TokenizerState temp_state = state->next_token(compiler, &ident_tok);
        temp_state.next_token(compiler, &colon_tok);

        if (ident_tok.kind == TokenKind_Identifier &&
            colon_tok.kind == TokenKind_Colon) {
            ident_tok = state->consume_token(compiler, TokenKind_Identifier);
            state->consume_token(compiler, TokenKind_Colon);

            ExprRef type_expr_ref = {0};
            ExprRef value_expr_ref = {0};

            state->next_token(compiler, &next_token);
            if (next_token.kind != TokenKind_Equal) {
                type_expr_ref = compiler->add_expr(parse_expr(compiler, state));

                state->next_token(compiler, &next_token);
                if (next_token.kind == TokenKind_Equal) {
                    state->consume_token(compiler, TokenKind_Equal);

                    value_expr_ref =
                        compiler->add_expr(parse_expr(compiler, state));
                }
            } else {
                state->consume_token(compiler, TokenKind_Equal);

                value_expr_ref =
                    compiler->add_expr(parse_expr(compiler, state));
            }

            SIR_ASSERT(type_expr_ref.id > 0 || value_expr_ref.id > 0);

            Decl var_decl = {};
            var_decl.kind = DeclKind_LocalVarDecl;
            var_decl.loc = ident_tok.loc;
            var_decl.name = ident_tok.str;
            var_decl.local_var_decl.type_expr = type_expr_ref;
            var_decl.local_var_decl.value_expr = value_expr_ref;

            stmt.kind = StmtKind_Decl;
            stmt.loc = var_decl.loc;
            stmt.decl.decl_ref = compiler->add_decl(var_decl);
        } else {
            Expr expr = parse_expr(compiler, state);
            ExprRef expr_ref = compiler->add_expr(expr);

            state->next_token(compiler, &next_token);
            if (next_token.kind == TokenKind_Equal) {
                *state = state->next_token(compiler, &next_token);

                stmt.kind = StmtKind_Assign;
                stmt.loc = expr.loc;
                stmt.assign.assigned_expr_ref = expr_ref;
                stmt.assign.value_expr_ref =
                    compiler->add_expr(parse_expr(compiler, state));
            } else {
                stmt.kind = StmtKind_Expr;
                stmt.loc = expr.loc;
                stmt.expr.expr_ref = expr_ref;
            }
        }

        state->consume_token(compiler, TokenKind_Semicolon);

        break;
    }
    }

    SIR_ASSERT(stmt.kind != StmtKind_Unknown);
    return stmt;
}

static void parse_top_level_decl(
    Compiler *compiler,
    TokenizerState *state,
    SIRArray<DeclRef> *top_level_decls)
{
    ZoneScoped;

    Token next_token = {};
    state->next_token(compiler, &next_token);
    switch (next_token.kind) {
    case TokenKind_Def: {
        Token func_token = state->consume_token(compiler, TokenKind_Def);

        Decl func_decl = {};
        func_decl.kind = DeclKind_Function;
        func_decl.loc = func_token.loc;
        func_decl.func = {};

        func_decl.func.param_decl_refs =
            SIRArray<DeclRef>::create((SIRAllocator *)compiler->arena);
        func_decl.func.return_type_expr_refs =
            SIRArray<ExprRef>::create((SIRAllocator *)compiler->arena);
        func_decl.func.body_stmts =
            SIRArray<StmtRef>::create((SIRAllocator *)compiler->arena);

        state->next_token(compiler, &next_token);
        switch (next_token.kind) {
        case TokenKind_Extern: {
            state->consume_token(compiler, TokenKind_Extern);
            func_decl.func.flags |= FunctionFlags_Extern;
            break;
        }
        case TokenKind_Export: {
            state->consume_token(compiler, TokenKind_Export);
            func_decl.func.flags |= FunctionFlags_Exported;
            break;
        }
        case TokenKind_Inline: {
            state->consume_token(compiler, TokenKind_Inline);
            func_decl.func.flags |= FunctionFlags_Inline;
            break;
        }
        default: break;
        }

        state->next_token(compiler, &next_token);
        if (next_token.kind == TokenKind_VarArg) {
            state->consume_token(compiler, TokenKind_VarArg);
            func_decl.func.flags |= FunctionFlags_VarArg;
        }

        Token ident_token =
            state->consume_token(compiler, TokenKind_Identifier);
        func_decl.name = ident_token.str;

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

            func_decl.func.param_decl_refs.push_back(param_decl_ref);

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
                ExprRef return_type_expr_ref =
                    compiler->add_expr(parse_expr(compiler, state));

                func_decl.func.return_type_expr_refs.push_back(
                    return_type_expr_ref);

                state->next_token(compiler, &next_token);
                if (next_token.kind == TokenKind_Comma) {
                    state->consume_token(compiler, TokenKind_Comma);
                    continue;
                } else {
                    break;
                }
            }
        }

        if (func_decl.func.flags & FunctionFlags_Extern) {
            state->consume_token(compiler, TokenKind_Semicolon);
        } else {
            state->consume_token(compiler, TokenKind_LCurly);

            state->next_token(compiler, &next_token);
            while (next_token.kind != TokenKind_RCurly) {
                Stmt stmt = parse_stmt(compiler, state);
                StmtRef stmt_ref = compiler->add_stmt(stmt);

                func_decl.func.body_stmts.push_back(stmt_ref);

                state->next_token(compiler, &next_token);
            }

            state->consume_token(compiler, TokenKind_RCurly);
        }

        DeclRef func_decl_ref = compiler->add_decl(func_decl);
        top_level_decls->push_back(func_decl_ref);

        break;
    }
    case TokenKind_Global: {
        state->consume_token(compiler, TokenKind_Global);

        Token ident_tok = state->consume_token(compiler, TokenKind_Identifier);

        state->consume_token(compiler, TokenKind_Colon);

        ExprRef type_expr_ref = {0};
        ExprRef value_expr_ref = {0};

        state->next_token(compiler, &next_token);
        if (next_token.kind != TokenKind_Equal) {
            type_expr_ref = compiler->add_expr(parse_expr(compiler, state));

            state->next_token(compiler, &next_token);
            if (next_token.kind == TokenKind_Equal) {
                state->consume_token(compiler, TokenKind_Equal);

                value_expr_ref =
                    compiler->add_expr(parse_expr(compiler, state));
            }
        } else {
            state->consume_token(compiler, TokenKind_Equal);

            value_expr_ref = compiler->add_expr(parse_expr(compiler, state));
        }

        SIR_ASSERT(type_expr_ref.id > 0 || value_expr_ref.id > 0);

        Decl var_decl = {};
        var_decl.kind = DeclKind_GlobalVarDecl;
        var_decl.loc = ident_tok.loc;
        var_decl.name = ident_tok.str;
        var_decl.local_var_decl.type_expr = type_expr_ref;
        var_decl.local_var_decl.value_expr = value_expr_ref;

        state->consume_token(compiler, TokenKind_Semicolon);

        DeclRef var_decl_ref = compiler->add_decl(var_decl);
        top_level_decls->push_back(var_decl_ref);

        break;
    }
    case TokenKind_Type: {
        state->consume_token(compiler, TokenKind_Type);

        Token ident_tok = state->consume_token(compiler, TokenKind_Identifier);

        ExprRef type_expr_ref = compiler->add_expr(parse_expr(compiler, state));

        state->consume_token(compiler, TokenKind_Semicolon);

        Decl type_decl = {};
        type_decl.kind = DeclKind_Type;
        type_decl.loc = ident_tok.loc;
        type_decl.name = ident_tok.str;
        type_decl.type_decl.type_expr = type_expr_ref;

        DeclRef type_decl_ref = compiler->add_decl(type_decl);
        top_level_decls->push_back(type_decl_ref);
        break;
    }
    default: {
        SIRString token_string = token_to_string(compiler, next_token);
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
    for (size_t i = 'a'; i <= 'z'; ++i) {
        EQ_CLASSES[i] = EqClass_Letter;
    }
    for (size_t i = 'A'; i <= 'Z'; ++i) {
        EQ_CLASSES[i] = EqClass_Letter;
    }
    for (size_t i = '0'; i <= '9'; ++i) {
        EQ_CLASSES[i] = EqClass_Digit;
    }

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
    EQ_CLASS_LINE_INC[EqClass_LineFeed] = 1;

    end_transition(State_UWhitespace, State_Whitespace);
    TRANSITION[State_WhitespaceStart][EqClass_Whitespace] = State_UWhitespace;
    TRANSITION[State_WhitespaceStart][EqClass_Slash] = State_UCommentSlash;
    TRANSITION[State_UWhitespace][EqClass_Slash] = State_UCommentSlash;
    TRANSITION[State_UCommentSlash][EqClass_Slash] = State_UCommentBody;
    for (size_t i = 0; i < EqClass_COUNT; ++i) {
        TRANSITION[State_UCommentBody][i] = State_UCommentBody;
    }
    TRANSITION[State_UCommentBody][EqClass_LineFeed] = State_UWhitespace;
    TRANSITION[State_UCommentBody][EqClass_CarriageReturn] = State_UWhitespace;

    TRANSITION[State_WhitespaceStart][EqClass_LineFeed] = State_UWhitespace;
    TRANSITION[State_WhitespaceStart][EqClass_CarriageReturn] =
        State_UWhitespace;
    TRANSITION[State_UWhitespace][EqClass_Whitespace] = State_UWhitespace;
    TRANSITION[State_UWhitespace][EqClass_LineFeed] = State_UWhitespace;
    TRANSITION[State_UWhitespace][EqClass_CarriageReturn] = State_UWhitespace;

    end_transition(State_UIdentifier, State_Identifier);
    end_transition(State_UBuiltinIdentifier, State_BuiltinIdentifier);
    end_transition(State_UIntLiteral, State_IntLiteral);
    end_transition(State_UFloatLiteral, State_FloatLiteral);
    end_transition(State_UClosedStringLiteral, State_StringLiteral);
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
    end_transition(State_UDiv, State_Div);
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
    TRANSITION[State_Start][EqClass_LParen] = State_LParen;
    TRANSITION[State_Start][EqClass_RParen] = State_RParen;
    TRANSITION[State_Start][EqClass_LBracket] = State_LBracket;
    TRANSITION[State_Start][EqClass_RBracket] = State_RBracket;
    TRANSITION[State_Start][EqClass_LCurly] = State_LCurly;
    TRANSITION[State_Start][EqClass_RCurly] = State_RCurly;
    TRANSITION[State_Start][EqClass_Colon] = State_Colon;
    TRANSITION[State_Start][EqClass_Comma] = State_Comma;
    TRANSITION[State_Start][EqClass_Semicolon] = State_Semicolon;
    TRANSITION[State_Start][EqClass_Question] = State_Question;

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
    TRANSITION[State_Start][EqClass_Slash] = State_UDiv;
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
    TRANSITION[State_UDiv][EqClass_Equal] = State_UDivEqual;
    TRANSITION[State_UMod][EqClass_Equal] = State_UModEqual;
    TRANSITION[State_UBitAnd][EqClass_Equal] = State_UBitAndEqual;
    TRANSITION[State_UBitOr][EqClass_Equal] = State_UBitOrEqual;
    TRANSITION[State_UBitNot][EqClass_Equal] = State_UBitNotEqual;
    TRANSITION[State_UBitXor][EqClass_Equal] = State_UBitXorEqual;
    TRANSITION[State_ULShift][EqClass_Equal] = State_ULShiftEqual;
    TRANSITION[State_URShift][EqClass_Equal] = State_URShiftEqual;

    // Identifier token
    TRANSITION[State_Start][EqClass_Letter] = State_UIdentifier;
    TRANSITION[State_Start][EqClass_Underscore] = State_UIdentifier;
    TRANSITION[State_UIdentifier][EqClass_Letter] = State_UIdentifier;
    TRANSITION[State_UIdentifier][EqClass_Digit] = State_UIdentifier;
    TRANSITION[State_UIdentifier][EqClass_Underscore] = State_UIdentifier;

    // Builtin identifier token
    TRANSITION[State_Start][EqClass_At] = State_UBuiltinIdentifier;
    TRANSITION[State_UBuiltinIdentifier][EqClass_Letter] =
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
    TRANSITION[State_UIntLiteral][EqClass_Digit] = State_UIntLiteral;
    TRANSITION[State_UIntLiteral][EqClass_Dot] = State_UFloatLiteral;
    TRANSITION[State_UFloatLiteral][EqClass_Digit] = State_UFloatLiteral;
    TRANSITION[State_UFloatLiteral][EqClass_Dot] = State_Error;
    TRANSITION[State_UDot][EqClass_Digit] = State_UFloatLiteral;
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

    TokenizerState state =
        TokenizerState::create(file_ref, file.text.ptr, file.text.len);

    /* Token token = {}; */
    /* size_t token_count = 0; */
    /* while (token.kind != TokenKind_EOF) { */
    /*     state = state.next_token(compiler, &token); */
    /*     token_count++; */
    /* } */
    /* printf("Got %zu tokens\n", token_count); */

    /* exit(1); */

    while (1) {
        Token token = {};
        state.next_token(compiler, &token);
        if (token.kind == TokenKind_Error) {
            SIRString token_string = token_to_string(compiler, token);
            compiler->add_error(
                token.loc,
                "unexpected token: '%.*s'",
                (int)token_string.len,
                token_string.ptr);
            compiler->halt_compilation();
        }

        if (token.kind == TokenKind_EOF) {
            break;
        }

        parse_top_level_decl(compiler, &state, &file.top_level_decls);
    }

    file.line_count = state.line + 1;

    compiler->files[file_ref.id] = file;

    if (compiler->errors.len > 0) {
        compiler->halt_compilation();
    }
}
