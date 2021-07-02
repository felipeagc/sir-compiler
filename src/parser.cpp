#include "compiler.hpp"

using namespace ace;

static bool expr_needs_semi(ExprKind kind)
{
    switch (kind) {
    case ExprKind_BoolLiteral:
    case ExprKind_BoolType:
    case ExprKind_FloatLiteral:
    case ExprKind_FloatType:
    case ExprKind_FunctionCall:
    case ExprKind_Identifier:
    case ExprKind_IntLiteral:
    case ExprKind_IntType:
    case ExprKind_NullLiteral:
    case ExprKind_UnitLiteral:
    case ExprKind_UnitType:
    case ExprKind_PointerType:
    case ExprKind_StringLiteral: return true;
    case ExprKind_Block:
    case ExprKind_Function: return false;
    }
    return true;
}

const char *token_kind_to_string(TokenKind kind)
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
    case TokenKind_ColonColon: return "::";
    case TokenKind_Comma: return ",";
    case TokenKind_Underscore: return "_";
    case TokenKind_Dot: return ".";
    case TokenKind_Semicolon: return ";";
    case TokenKind_Question: return "?";

    case TokenKind_Equal: return "=";

    case TokenKind_And: return "&&";
    case TokenKind_Or: return "||";

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
    case TokenKind_Global: return "global";
    case TokenKind_Inline: return "inline";
    case TokenKind_Macro: return "macro";
    case TokenKind_Def: return "def";
    case TokenKind_Struct: return "struct";
    case TokenKind_Union: return "union";
    case TokenKind_Unit: return "unit";
    case TokenKind_Bool: return "bool";
    case TokenKind_True: return "true";
    case TokenKind_False: return "false";
    case TokenKind_Null: return "null";
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

ACE_INLINE static bool is_whitespace(char c)
{
    return c == ' ' || c == '\t' || c == '\r' || c == '\n';
}

ACE_INLINE static bool is_alpha(char c)
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_');
}

ACE_INLINE static bool is_alpha_num(char c)
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_') ||
           (c >= '0' && c <= '9');
}

ACE_INLINE static bool is_hex(char c)
{
    return (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') ||
           (c >= '0' && c <= '9');
}

ACE_INLINE static bool is_num(char c)
{
    return (c >= '0' && c <= '9');
}

struct TokenizerState {
    File *file;
    uint32_t pos;
    uint32_t line;
    uint32_t col;

    static TokenizerState create(File *file)
    {
        TokenizerState state = {};
        state.file = file;
        state.pos = 0;
        state.line = 1;
        state.col = 1;
        return state;
    }

    int64_t length_left(size_t offset = 0)
    {
        return ((int64_t)this->file->text.len) - (int64_t)(this->pos + offset);
    }

    Token consume_token(Compiler *compiler, TokenKind token_kind)
    {
        Allocator *allocator = compiler->arena;

        Token token = {};
        *this = this->next_token(compiler, &token);
        if (token.kind != token_kind) {
            compiler->add_error(
                token.loc,
                "unexpected token: '%s', expecting '%s'",
                token_kind_to_string(token.kind),
                token_kind_to_string(token_kind));
            compiler->halt_compilation();
        }

        return token;
    }

    TokenizerState next_token(Compiler *compiler, Token *token) const
    {
        Allocator *allocator = compiler->arena;
        TokenizerState state = *this;

    begin:
        *token = Token{};

        // Skip whitespace
        for (size_t i = state.pos; i < state.file->text.len; ++i) {
            if (is_whitespace(state.file->text[i])) {
                state.pos++;
                state.col++;
                if (state.file->text[i] == '\n') {
                    state.line++;
                    state.col = 1;
                }
            } else
                break;
        }

        token->loc.offset = state.pos;
        token->loc.line = state.line;
        token->loc.col = state.col;
        token->loc.len = 1;
        token->loc.file = state.file;

        if (state.length_left() <= 0) {
            token->kind = TokenKind_EOF;
            return state;
        }

        char c = state.file->text[state.pos];
        switch (c) {
        case '\"': {
            // String
            state.pos++;

            const char *string = &state.file->text[state.pos];

            size_t content_length = 0;
            while (state.length_left(content_length) > 0 &&
                   state.file->text[state.pos + content_length] != '\"') {
                content_length++;
            }

            state.pos += content_length;

            if (state.length_left() > 0 &&
                state.file->text[state.pos] == '\"') {
                state.pos++;
            } else {
                token->kind = TokenKind_Error;
                token->str = allocator->clone("unclosed string");
                break;
            }

            token->kind = TokenKind_StringLiteral;
            token->str =
                allocator->null_terminate(String{string, content_length});

            break;
        }

        case '{':
            state.pos++;
            token->kind = TokenKind_LCurly;
            break;
        case '}':
            state.pos++;
            token->kind = TokenKind_RCurly;
            break;
        case '[':
            state.pos++;
            token->kind = TokenKind_LBracket;
            break;
        case ']':
            state.pos++;
            token->kind = TokenKind_RBracket;
            break;
        case '(':
            state.pos++;
            token->kind = TokenKind_LParen;
            break;
        case ')':
            state.pos++;
            token->kind = TokenKind_RParen;
            break;

        case '=': {
            state.pos++;
            token->kind = TokenKind_Equal;
            if (state.length_left() > 0) {
                switch (state.file->text[state.pos]) {
                case '=':
                    state.pos++;
                    token->kind = TokenKind_EqualEqual;
                    break;
                case '>':
                    state.pos++;
                    token->kind = TokenKind_Arrow;
                    break;
                default: break;
                }
            }
            break;
        }

        case '+': {
            state.pos++;
            token->kind = TokenKind_Add;
            if (state.length_left() > 0) {
                switch (state.file->text[state.pos]) {
                case '=':
                    state.pos++;
                    token->kind = TokenKind_AddEqual;
                    break;
                default: break;
                }
            }
            break;
        }

        case '-': {
            state.pos++;
            token->kind = TokenKind_Sub;
            if (state.length_left() > 0 && state.file->text[state.pos] == '=') {
                state.pos++;
                token->kind = TokenKind_SubEqual;
            }
            break;
        }

        case '*': {
            state.pos++;
            token->kind = TokenKind_Mul;
            if (state.length_left() > 0 && state.file->text[state.pos] == '=') {
                state.pos++;
                token->kind = TokenKind_MulEqual;
            }
            break;
        }

        case '/': {
            state.pos++;
            token->kind = TokenKind_Div;
            if (state.length_left() > 0) {
                switch (state.file->text[state.pos]) {
                case '=':
                    state.pos++;
                    token->kind = TokenKind_DivEqual;
                    break;
                case '/':
                    state.pos++;
                    while (state.length_left() > 0 &&
                           state.file->text[state.pos] != '\n') {
                        state.pos++;
                    }
                    goto begin;
                    break;
                default: break;
                }
            }
            break;
        }

        case '%': {
            state.pos++;
            token->kind = TokenKind_Mod;
            if (state.length_left() > 0 && state.file->text[state.pos] == '=') {
                state.pos++;
                token->kind = TokenKind_ModEqual;
            }
            break;
        }

        case '|': {
            state.pos++;
            token->kind = TokenKind_BitOr;
            if (state.length_left() > 0) {
                switch (state.file->text[state.pos]) {
                case '=':
                    state.pos++;
                    token->kind = TokenKind_BitOrEqual;
                    break;
                case '|':
                    state.pos++;
                    token->kind = TokenKind_Or;
                    break;
                default: break;
                }
            }
            break;
        }

        case '&': {
            state.pos++;
            token->kind = TokenKind_BitAnd;
            if (state.length_left() > 0) {
                switch (state.file->text[state.pos]) {
                case '=':
                    state.pos++;
                    token->kind = TokenKind_BitAndEqual;
                    break;
                case '&':
                    state.pos++;
                    token->kind = TokenKind_And;
                    break;
                default: break;
                }
            }
            break;
        }

        case '^': {
            state.pos++;
            token->kind = TokenKind_BitXor;
            if (state.length_left() > 0 && state.file->text[state.pos] == '=') {
                state.pos++;
                token->kind = TokenKind_BitXorEqual;
            }
            break;
        }

        case '~': {
            state.pos++;
            token->kind = TokenKind_BitNot;
            if (state.length_left() > 0 && state.file->text[state.pos] == '=') {
                state.pos++;
                token->kind = TokenKind_BitNotEqual;
            }
            break;
        }

        case '!': {
            state.pos++;
            token->kind = TokenKind_Not;
            if (state.length_left() > 0 && state.file->text[state.pos] == '=') {
                state.pos++;
                token->kind = TokenKind_NotEqual;
            }
            break;
        }

        case '<': {
            state.pos++;
            token->kind = TokenKind_Less;
            if (state.length_left() > 0) {
                switch (state.file->text[state.pos]) {
                case '=':
                    state.pos++;
                    token->kind = TokenKind_LessEqual;
                    break;
                case '<':
                    state.pos++;
                    token->kind = TokenKind_LShift;
                    if (state.length_left() > 0 &&
                        state.file->text[state.pos] == '=') {
                        token->kind = TokenKind_LShiftEqual;
                    }
                    break;
                default: break;
                }
            }
            break;
        }

        case '>': {
            state.pos++;
            token->kind = TokenKind_Greater;
            if (state.length_left() > 0) {
                switch (state.file->text[state.pos]) {
                case '=':
                    state.pos++;
                    token->kind = TokenKind_GreaterEqual;
                    break;
                case '>':
                    state.pos++;
                    token->kind = TokenKind_RShift;
                    if (state.length_left() > 0 &&
                        state.file->text[state.pos] == '=') {
                        token->kind = TokenKind_RShiftEqual;
                    }
                    break;
                default: break;
                }
            }
            break;
        }

        case ':': {
            state.pos++;
            token->kind = TokenKind_Colon;
            if (state.length_left() > 0 && state.file->text[state.pos] == ':') {
                state.pos++;
                token->kind = TokenKind_ColonColon;
            }
            break;
        }
        case ';':
            state.pos++;
            token->kind = TokenKind_Semicolon;
            break;
        case '.':
            state.pos++;
            token->kind = TokenKind_Dot;
            break;
        case ',':
            state.pos++;
            token->kind = TokenKind_Comma;
            break;
        case '?':
            state.pos++;
            token->kind = TokenKind_Question;
            break;

        default: {
            if (is_alpha(c)) {
                // Identifier
                size_t ident_length = 0;
                while (
                    state.length_left(ident_length) > 0 &&
                    is_alpha_num(state.file->text[state.pos + ident_length])) {
                    ident_length++;
                }

                const char *ident_start = &state.file->text[state.pos];
                String ident_str = String{ident_start, ident_length};

                if (!compiler->keyword_map.get(ident_str, &token->kind)) {
                    token->kind = TokenKind_Identifier;
                    token->str = allocator->null_terminate(ident_str);
                }

                state.pos += ident_length;
            } else if (c == '@' && is_alpha(state.file->text[state.pos + 1])) {
                // Builtin Identifier
                state.pos++;
                size_t ident_length = 0;
                while (
                    state.length_left(ident_length) > 0 &&
                    is_alpha_num(state.file->text[state.pos + ident_length])) {
                    ident_length++;
                }

                const char *ident_start = &state.file->text[state.pos];

                token->kind = TokenKind_BuiltinIdentifier;
                token->str = allocator->null_terminate(
                    String{ident_start, ident_length});
                state.pos += ident_length;
            } else if (is_num(c)) {
                if (state.length_left() >= 3 &&
                    state.file->text[state.pos] == '0' &&
                    state.file->text[state.pos + 1] == 'x' &&
                    is_hex(state.file->text[state.pos + 2])) {
                    token->kind = TokenKind_IntLiteral;
                    state.pos += 2;

                    size_t number_length = 0;
                    while (
                        state.length_left(number_length) > 0 &&
                        is_hex(state.file->text[state.pos + number_length])) {
                        number_length++;
                    }

                    const char *int_str = allocator->null_terminate(
                        String{&state.file->text[state.pos], number_length});
                    state.pos += number_length;
                    token->int_ = strtol(int_str, NULL, 16);
                } else {
                    token->kind = TokenKind_IntLiteral;

                    size_t number_length = 0;
                    while (
                        state.length_left(number_length) > 0 &&
                        is_num(state.file->text[state.pos + number_length])) {
                        number_length++;
                    }

                    if (state.length_left(number_length) > 1 &&
                        state.file->text[state.pos + number_length] == '.' &&
                        is_num(
                            state.file->text[state.pos + number_length + 1])) {
                        token->kind = TokenKind_FloatLiteral;
                        number_length++;
                    }

                    while (
                        state.length_left(number_length) > 0 &&
                        is_num(state.file->text[state.pos + number_length])) {
                        number_length++;
                    }

                    switch (token->kind) {
                    case TokenKind_IntLiteral: {
                        const char *int_str = allocator->null_terminate(String{
                            &state.file->text[state.pos], number_length});
                        token->int_ = strtol(int_str, NULL, 10);
                        break;
                    }
                    case TokenKind_FloatLiteral: {
                        const char *float_str =
                            allocator->null_terminate(String{
                                &state.file->text[state.pos], number_length});
                        token->float_ = strtod(float_str, NULL);
                        break;
                    }
                    default: ACE_ASSERT(0); break;
                    }

                    state.pos += number_length;
                }
            } else {
                token->kind = TokenKind_Error;
                token->str = allocator->sprintf(
                    "unknown token: '%c'", state.file->text[state.pos]);
                state.pos++;
            }
            break;
        }
        }

        token->loc.len = state.pos - token->loc.offset;
        state.col += token->loc.len;

        return state;
    }
};

static Expr parse_expr(Compiler *compiler, TokenizerState *state);

static Expr parse_primary_expr(Compiler *compiler, TokenizerState *state)
{
    Expr expr = {};

    Token next_token = {};
    state->next_token(compiler, &next_token);

    switch (next_token.kind) {
    case TokenKind_LParen: {
        Token lparen_token = state->consume_token(compiler, TokenKind_LParen);

        state->next_token(compiler, &next_token);
        if (next_token.kind == TokenKind_RParen) {
            expr.kind = ExprKind_UnitLiteral;
            expr.loc = next_token.loc;
            state->consume_token(compiler, TokenKind_RParen);
        } else {
            expr = parse_expr(compiler, state);
            state->consume_token(compiler, TokenKind_RParen);
        }
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
        ExprRef sub_expr_ref = {(uint32_t)compiler->exprs.len};
        compiler->exprs.push_back(sub_expr);

        expr.kind = ExprKind_PointerType;
        expr.loc = asterisk_token.loc;
        expr.ptr_type.sub_expr_ref = sub_expr_ref;
        break;
    }
    case TokenKind_Unit: {
        *state = state->next_token(compiler, &next_token);

        expr.kind = ExprKind_UnitType;
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
        default: ACE_ASSERT(0);
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
        default: ACE_ASSERT(0);
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
        default: ACE_ASSERT(0);
        }

        break;
    }
    default: {
        compiler->add_error(
            next_token.loc,
            "unexpected token: '%s', expecting primary expression",
            token_kind_to_string(next_token.kind));
        compiler->halt_compilation();
        break;
    }
    }

    return expr;
}

static Expr parse_function_expr(Compiler *compiler, TokenizerState *state)
{
    Expr expr = {};

    Token next_token = {};

    expr.loc = next_token.loc;
    expr.func = {};

    expr.func.param_decl_refs = ace::Array<DeclRef>::create(compiler->arena);
    expr.func.return_type_expr_refs =
        ace::Array<ExprRef>::create(compiler->arena);

    state->next_token(compiler, &next_token);
    switch (next_token.kind) {
    case TokenKind_Inline: {
        state->consume_token(compiler, TokenKind_Inline);
        expr.func.flags |= FunctionFlags_Inline;
        break;
    }
    default: {
        compiler->add_error(
            next_token.loc,
            "unexpected token: '%s', expecting function literal",
            token_kind_to_string(next_token.kind));
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
        ExprRef type_expr_ref = {(uint32_t)compiler->exprs.len};
        compiler->exprs.push_back(type_expr);

        Decl param_decl = {};
        param_decl.kind = DeclKind_FunctionParameter;
        param_decl.loc = ident_token.loc;
        param_decl.func_param.type_expr = type_expr_ref;

        DeclRef param_decl_ref = {(uint32_t)compiler->decls.len};
        compiler->decls.push_back(param_decl);

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
            ExprRef return_type_expr_ref = {(uint32_t)compiler->exprs.len};
            compiler->exprs.push_back(return_type_expr);

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

    Expr body_expr = parse_expr(compiler, state);
    ExprRef body_expr_ref = {(uint32_t)compiler->exprs.len};
    compiler->exprs.push_back(body_expr);

    expr.func.body_expr_ref = body_expr_ref;

    return expr;
}

static Expr parse_func_call_expr(Compiler *compiler, TokenizerState *state)
{
    Expr expr = parse_primary_expr(compiler, state);

    Token next_token = {};
    state->next_token(compiler, &next_token);
    if (next_token.kind == TokenKind_LParen) {
        state->consume_token(compiler, TokenKind_LParen);

        Expr func_expr = expr;
        ExprRef func_expr_ref = {(uint32_t)compiler->exprs.len};
        compiler->exprs.push_back(func_expr);

        expr = {};
        expr.loc = func_expr .loc;
        expr.kind = ExprKind_FunctionCall;
        expr.func_call.func_expr_ref = func_expr_ref;
        expr.func_call.param_refs = ace::Array<ExprRef>::create(compiler->arena);

        state->next_token(compiler, &next_token);
        while (next_token.kind != TokenKind_RParen) {
            Expr param_expr = parse_expr(compiler, state);
            ExprRef param_expr_ref = {(uint32_t)compiler->exprs.len};
            compiler->exprs.push_back(param_expr);

            expr.func_call.param_refs.push_back(param_expr_ref);

            state->next_token(compiler, &next_token);
            if (next_token.kind != TokenKind_RParen) {
                state->consume_token(compiler, TokenKind_Comma);
            }

            state->next_token(compiler, &next_token);
        }

        state->consume_token(compiler, TokenKind_RParen);
    }

    return expr;
}

static Expr parse_expr(Compiler *compiler, TokenizerState *state)
{
    Token next_token = {};
    state->next_token(compiler, &next_token);

    Expr expr = {};

    switch (next_token.kind) {
    case TokenKind_LCurly: {
        Token lcurly_token = state->consume_token(compiler, TokenKind_LCurly);

        expr.kind = ExprKind_Block;
        expr.loc = lcurly_token.loc;
        expr.block.expr_refs = ace::Array<ExprRef>::create(compiler->arena);

        state->next_token(compiler, &next_token);
        while (next_token.kind != TokenKind_RCurly) {
            Expr sub_expr = parse_expr(compiler, state);
            ExprRef sub_expr_ref = {(uint32_t)compiler->exprs.len};
            compiler->exprs.push_back(sub_expr);

            expr.block.expr_refs.push_back(sub_expr_ref);

            state->next_token(compiler, &next_token);
            if (next_token.kind != TokenKind_RCurly && expr_needs_semi(sub_expr.kind)) {
                state->consume_token(compiler, TokenKind_Semicolon);

                state->next_token(compiler, &next_token);
                if (next_token.kind == TokenKind_RCurly) {
                    Expr sub_expr = {};
                    sub_expr.loc = next_token.loc;
                    sub_expr.kind = ExprKind_UnitLiteral;

                    ExprRef sub_expr_ref = {(uint32_t)compiler->exprs.len};
                    compiler->exprs.push_back(sub_expr);

                    expr.block.expr_refs.push_back(sub_expr_ref);
                }
            }

            state->next_token(compiler, &next_token);
        }

        state->consume_token(compiler, TokenKind_RCurly);
        break;
    }
    default: {
        expr = parse_func_call_expr(compiler, state);
        break;
    }
    }

    return expr;
}

static void
parse_top_level_decl(Compiler *compiler, TokenizerState *state, File *file)
{
    Token next_token = {};
    state->next_token(compiler, &next_token);
    switch (next_token.kind) {
    case TokenKind_Def: {
        Token func_token = state->consume_token(compiler, TokenKind_Def);

        Decl func_decl = {};
        func_decl.loc = func_token.loc;
        func_decl.func = {};

        func_decl.func.param_decl_refs =
            ace::Array<DeclRef>::create(compiler->arena);
        func_decl.func.return_type_expr_refs =
            ace::Array<ExprRef>::create(compiler->arena);

        state->next_token(compiler, &next_token);
        switch (next_token.kind) {
        case TokenKind_Extern: {
            state->consume_token(compiler, TokenKind_Extern);
            func_decl.func.flags |= FunctionFlags_Extern;
            break;
        }
        case TokenKind_Inline: {
            state->consume_token(compiler, TokenKind_Inline);
            func_decl.func.flags |= FunctionFlags_Inline;
            break;
        }
        default: break;
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
            ExprRef type_expr_ref = {(uint32_t)compiler->exprs.len};
            compiler->exprs.push_back(type_expr);

            Decl param_decl = {};
            param_decl.kind = DeclKind_FunctionParameter;
            param_decl.loc = ident_token.loc;
            param_decl.func_param.type_expr = type_expr_ref;

            DeclRef param_decl_ref = {(uint32_t)compiler->decls.len};
            compiler->decls.push_back(param_decl);

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
                Expr return_type_expr = parse_expr(compiler, state);
                ExprRef return_type_expr_ref = {(uint32_t)compiler->exprs.len};
                compiler->exprs.push_back(return_type_expr);

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
            state->consume_token(compiler, TokenKind_Equal);

            Expr body_expr = parse_expr(compiler, state);
            ExprRef body_expr_ref = {(uint32_t)compiler->exprs.len};
            compiler->exprs.push_back(body_expr);

            if (expr_needs_semi(body_expr.kind)) {
                state->consume_token(compiler, TokenKind_Semicolon);
            }

            func_decl.func.body_expr_ref = body_expr_ref;
        }

        DeclRef func_decl_ref = {(uint32_t)compiler->decls.len};
        compiler->decls.push_back(func_decl);
        file->top_level_decls.push_back(func_decl_ref);

        break;
    }
    default: {
        compiler->add_error(
            next_token.loc,
            "unexpected token: '%s', expecting top level declaration",
            token_kind_to_string(next_token.kind));
        compiler->halt_compilation();
        break;
    }
    }
}

void parse_file(Compiler *compiler, File *file)
{
    file->top_level_decls.reserve(512);

    TokenizerState state = TokenizerState::create(file);
    while (1) {
        Token token = {};
        state.next_token(compiler, &token);
        if (token.kind == TokenKind_Error) {
            compiler->add_error(
                token.loc,
                "unexpected token: '%.*s'",
                (int)token.str.len,
                token.str.ptr);
            compiler->halt_compilation();
        }

        if (token.kind == TokenKind_EOF) {
            break;
        }

        parse_top_level_decl(compiler, &state, file);
    }
}
