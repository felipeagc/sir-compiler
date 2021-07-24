#include "compiler.hpp"
#include <Tracy.hpp>

struct AnalyzerState {
    FileRef file_ref;
    ace::Array<Scope *> scope_stack;
};

static bool
interp_expr(Compiler *compiler, ExprRef expr_ref, InterpValue *out_value)
{
    Expr expr = expr_ref.get(compiler);
    switch (expr.kind) {
    case ExprKind_IntLiteral: {
        InterpValue value = {};
        value.type_ref = compiler->untyped_int_type;
        value.i64 = expr.int_literal.i64;
        *out_value = value;
        return true;
    }
    case ExprKind_FloatLiteral: {
        InterpValue value = {};
        value.type_ref = compiler->untyped_float_type;
        value.f64 = expr.float_literal.f64;
        *out_value = value;
        return true;
    }
    default: break;
    }

    return false;
}

static void
analyze_stmt(Compiler *compiler, AnalyzerState *state, StmtRef stmt_ref);
static void
analyze_decl(Compiler *compiler, AnalyzerState *state, DeclRef decl_ref);

static void analyze_expr(
    Compiler *compiler,
    AnalyzerState *state,
    ExprRef expr_ref,
    TypeRef expected_type_ref = {0})
{
    ZoneScoped;

    ACE_ASSERT(expr_ref.id > 0);
    Expr expr = compiler->exprs[expr_ref.id];

    switch (expr.kind) {
    case ExprKind_Unknown: {
        ACE_ASSERT(0);
        break;
    }

    case ExprKind_VoidType: {
        expr.expr_type_ref = compiler->type_type;
        expr.as_type_ref = compiler->void_type;
        break;
    }

    case ExprKind_BoolType: {
        expr.expr_type_ref = compiler->type_type;
        expr.as_type_ref = compiler->bool_type;
        break;
    }

    case ExprKind_IntType: {
        expr.expr_type_ref = compiler->type_type;
        if (expr.int_type.is_signed) {
            switch (expr.int_type.bits) {
            case 8: expr.as_type_ref = compiler->i8_type; break;
            case 16: expr.as_type_ref = compiler->i16_type; break;
            case 32: expr.as_type_ref = compiler->i32_type; break;
            case 64: expr.as_type_ref = compiler->i64_type; break;
            }
        } else {
            switch (expr.int_type.bits) {
            case 8: expr.as_type_ref = compiler->u8_type; break;
            case 16: expr.as_type_ref = compiler->u16_type; break;
            case 32: expr.as_type_ref = compiler->u32_type; break;
            case 64: expr.as_type_ref = compiler->u64_type; break;
            }
        }
        break;
    }

    case ExprKind_FloatType: {
        expr.expr_type_ref = compiler->type_type;
        switch (expr.float_type.bits) {
        case 32: expr.as_type_ref = compiler->f32_type; break;
        case 64: expr.as_type_ref = compiler->f64_type; break;
        }
        break;
    }

    case ExprKind_PointerType: {
        analyze_expr(
            compiler, state, expr.ptr_type.sub_expr_ref, compiler->type_type);
        TypeRef sub_type = expr.ptr_type.sub_expr_ref.get(compiler).as_type_ref;
        if (sub_type.id) {
            expr.expr_type_ref = compiler->type_type;
            expr.as_type_ref = compiler->create_pointer_type(sub_type);
        }
        break;
    }

    case ExprKind_SliceType: {
        analyze_expr(
            compiler,
            state,
            expr.slice_type.subtype_expr_ref,
            compiler->type_type);
        TypeRef sub_type =
            expr.slice_type.subtype_expr_ref.get(compiler).as_type_ref;
        if (sub_type.id) {
            expr.expr_type_ref = compiler->type_type;
            expr.as_type_ref = compiler->create_slice_type(sub_type);
        }
        break;
    }

    case ExprKind_ArrayType: {
        analyze_expr(
            compiler,
            state,
            expr.array_type.subtype_expr_ref,
            compiler->type_type);
        analyze_expr(
            compiler,
            state,
            expr.array_type.size_expr_ref,
            compiler->untyped_int_type);

        TypeRef sub_type =
            expr.array_type.subtype_expr_ref.get(compiler).as_type_ref;
        if (sub_type.id) {
            InterpValue interp_value = {};
            if (interp_expr(
                    compiler, expr.array_type.size_expr_ref, &interp_value)) {
                ACE_ASSERT(
                    interp_value.type_ref.id == compiler->untyped_int_type.id);
                expr.expr_type_ref = compiler->type_type;
                expr.as_type_ref =
                    compiler->create_array_type(sub_type, interp_value.i64);
            } else {
                compiler->add_error(
                    expr.array_type.size_expr_ref.get(compiler).loc,
                    "array size expression does not evaluate to a compile-time "
                    "integer");
            }
        }
        break;
    }

    case ExprKind_BoolLiteral: {
        expr.expr_type_ref = compiler->bool_type;
        break;
    }

    case ExprKind_IntLiteral: {
        if (expected_type_ref.id &&
            (expected_type_ref.get(compiler).kind == TypeKind_Int ||
             expected_type_ref.get(compiler).kind == TypeKind_Float)) {
            expr.expr_type_ref = expected_type_ref;
        } else {
            expr.expr_type_ref = compiler->untyped_int_type;
        }
        break;
    }

    case ExprKind_FloatLiteral: {
        if (expected_type_ref.id &&
            expected_type_ref.get(compiler).kind == TypeKind_Float) {
            expr.expr_type_ref = expected_type_ref;
        } else {
            expr.expr_type_ref = compiler->untyped_float_type;
        }
        break;
    }

    case ExprKind_StringLiteral: {
        expr.expr_type_ref = compiler->create_slice_type(compiler->u8_type);
        if (expected_type_ref.id) {
            Type expected_type = expected_type_ref.get(compiler);
            if (expected_type.kind == TypeKind_Pointer &&
                expected_type.pointer.sub_type.id == compiler->u8_type.id) {
                expr.expr_type_ref = expected_type_ref;
            }
        }
        break;
    }

    case ExprKind_NullLiteral: {
        if (expected_type_ref.id &&
            expected_type_ref.get(compiler).kind == TypeKind_Pointer) {
            expr.expr_type_ref = expected_type_ref;
        } else {
            expr.expr_type_ref =
                compiler->create_pointer_type(compiler->void_type);
        }
        break;
    }

    case ExprKind_VoidLiteral: {
        expr.expr_type_ref = compiler->void_type;
        break;
    }

    case ExprKind_Identifier: {
        Scope *scope = *state->scope_stack.last();
        DeclRef decl_ref = scope->lookup(expr.ident.str);
        if (decl_ref.id) {
            Decl decl = decl_ref.get(compiler);
            expr.expr_type_ref = decl.decl_type_ref;
            expr.as_type_ref = decl.as_type_ref;
            expr.ident.decl_ref = decl_ref;
        } else {
            compiler->add_error(
                expr.loc,
                "identifier '%.*s' does not refer to a symbol",
                (int)expr.ident.str.len,
                expr.ident.str.ptr);
        }
        break;
    }

    case ExprKind_Function: {
        compiler->add_error(expr.loc, "unimplemented function expr");
        break;
    }

    case ExprKind_FunctionCall: {
        analyze_expr(compiler, state, expr.func_call.func_expr_ref);

        Expr func_expr = expr.func_call.func_expr_ref.get(compiler);
        Type func_type = func_expr.expr_type_ref.get(compiler);
        switch (func_type.kind) {
        case TypeKind_Function: {
            // Actual function call
            if (func_type.func.vararg) {
                if (func_type.func.param_types.len >
                    expr.func_call.param_refs.len) {
                    compiler->add_error(
                        expr.loc,
                        "expected at least '%zu' parameters for variadic "
                        "function call, instead got '%zu'",
                        func_type.func.param_types.len,
                        expr.func_call.param_refs.len);
                    break;
                }
            } else {
                if (func_type.func.param_types.len !=
                    expr.func_call.param_refs.len) {
                    compiler->add_error(
                        expr.loc,
                        "expected '%zu' parameters for function call, instead "
                        "got "
                        "'%zu'",
                        func_type.func.param_types.len,
                        expr.func_call.param_refs.len);
                    break;
                }
            }

            for (size_t i = 0; i < func_type.func.param_types.len; ++i) {
                TypeRef param_expected_type = func_type.func.param_types[i];
                analyze_expr(
                    compiler,
                    state,
                    expr.func_call.param_refs[i],
                    param_expected_type);
            }

            for (size_t i = func_type.func.param_types.len;
                 i < expr.func_call.param_refs.len;
                 ++i) {
                analyze_expr(compiler, state, expr.func_call.param_refs[i]);
            }

            expr.expr_type_ref = func_type.func.return_type;
            break;
        }

        case TypeKind_Type: {
            // Type cast

            if (expr.func_call.param_refs.len != 1) {
                compiler->add_error(
                    func_expr.loc, "expected type cast to have 1 parameter");
                break;
            }

            ACE_ASSERT(func_expr.as_type_ref.id != 0);

            Type dest_type = func_expr.as_type_ref.get(compiler);

            size_t error_checkpoint = compiler->get_error_checkpoint();

            analyze_expr(compiler, state, expr.func_call.param_refs[0]);

            TypeRef param_type_ref =
                expr.func_call.param_refs[0].get(compiler).expr_type_ref;
            Type param_type = param_type_ref.get(compiler);
            if (param_type.kind == TypeKind_Unknown ||
                param_type.kind == TypeKind_UntypedInt ||
                param_type.kind == TypeKind_UntypedFloat) {

                compiler->restore_error_checkpoint(error_checkpoint);

                analyze_expr(
                    compiler,
                    state,
                    expr.func_call.param_refs[0],
                    func_expr.as_type_ref);

            } else if (!((param_type.kind == TypeKind_Int ||
                          param_type.kind == TypeKind_Float) &&
                         (dest_type.kind == TypeKind_Int ||
                          dest_type.kind == TypeKind_Float))) {
                compiler->add_error(expr.loc, "invalid cast");
                break;
            }

            expr.expr_type_ref = func_expr.as_type_ref;

            break;
        }

        default: {
            compiler->add_error(
                func_expr.loc, "expected expression to have function type");
            break;
        }
        }

        break;
    }

    case ExprKind_BuiltinCall: {
        switch (expr.builtin_call.builtin) {
        case BuiltinFunction_Unknown: ACE_ASSERT(0); break;
        case BuiltinFunction_Sizeof: {
            if (expr.builtin_call.param_refs.len != 1) {
                compiler->add_error(
                    expr.loc, "expected 1 parameter for @sizeof");
                break;
            }

            analyze_expr(
                compiler,
                state,
                expr.builtin_call.param_refs[0],
                compiler->type_type);

            if (expected_type_ref.id &&
                (expected_type_ref.get(compiler).kind == TypeKind_Int ||
                 expected_type_ref.get(compiler).kind == TypeKind_Float)) {
                expr.expr_type_ref = expected_type_ref;
            } else {
                expr.expr_type_ref = compiler->untyped_int_type;
            }

            break;
        }
        case BuiltinFunction_Alignof: {
            if (expr.builtin_call.param_refs.len != 1) {
                compiler->add_error(
                    expr.loc, "expected 1 parameter for @alignof");
                break;
            }

            analyze_expr(
                compiler,
                state,
                expr.builtin_call.param_refs[0],
                compiler->type_type);

            if (expected_type_ref.id &&
                (expected_type_ref.get(compiler).kind == TypeKind_Int ||
                 expected_type_ref.get(compiler).kind == TypeKind_Float)) {
                expr.expr_type_ref = expected_type_ref;
            } else {
                expr.expr_type_ref = compiler->untyped_int_type;
            }

            break;
        }
        case BuiltinFunction_PtrCast: {
            if (expr.builtin_call.param_refs.len != 2) {
                compiler->add_error(
                    expr.loc, "expected 2 parameters for @ptrcast");
                break;
            }

            analyze_expr(
                compiler,
                state,
                expr.builtin_call.param_refs[0],
                compiler->type_type);

            analyze_expr(compiler, state, expr.builtin_call.param_refs[1]);

            Expr param0 = expr.builtin_call.param_refs[0].get(compiler);
            Expr param1 = expr.builtin_call.param_refs[1].get(compiler);
            if (param0.as_type_ref.get(compiler).kind != TypeKind_Pointer) {
                compiler->add_error(param0.loc, "expected pointer type");
                break;
            }

            if (param1.expr_type_ref.get(compiler).kind != TypeKind_Pointer) {
                compiler->add_error(
                    param1.loc, "expected expression of pointer type");
                break;
            }

            expr.expr_type_ref = param0.as_type_ref;
            break;
        }
        }

        break;
    }

    case ExprKind_Subscript: {
        analyze_expr(compiler, state, expr.subscript.left_ref);
        analyze_expr(
            compiler, state, expr.subscript.right_ref, compiler->u64_type);

        Expr left_expr = expr.subscript.left_ref.get(compiler);
        Expr right_expr = expr.subscript.right_ref.get(compiler);

        TypeRef indexed_type_ref = left_expr.expr_type_ref;
        TypeRef index_type_ref = right_expr.expr_type_ref;
        if (index_type_ref.id == 0 || indexed_type_ref.id == 0) {
            ACE_ASSERT(compiler->errors.len > 0);
            break;
        }

        Type index_type = index_type_ref.get(compiler);
        Type indexed_type = indexed_type_ref.get(compiler);

        if (index_type.kind != TypeKind_Int &&
            index_type.kind != TypeKind_UntypedInt) {
            compiler->add_error(
                right_expr.loc, "subscript index is not an integer");
        }

        if (indexed_type.kind != TypeKind_Array &&
            indexed_type.kind != TypeKind_Slice) {
            compiler->add_error(
                left_expr.loc,
                "accessed expression in subscript is not an array");
        }

        switch (indexed_type.kind) {
        case TypeKind_Array: {
            expr.expr_type_ref = indexed_type.array.sub_type;
            break;
        }
        case TypeKind_Slice: {
            expr.expr_type_ref = indexed_type.slice.sub_type;
            break;
        }
        default: ACE_ASSERT(0);
        }

        break;
    }

    case ExprKind_Unary: {
        switch (expr.unary.op) {
        case UnaryOp_Unknown: ACE_ASSERT(0); break;
        case UnaryOp_AddressOf: {
            analyze_expr(compiler, state, expr.unary.left_ref);

            TypeRef subtype_ref =
                expr.unary.left_ref.get(compiler).expr_type_ref;
            if (!subtype_ref.is_runtime(compiler)) {
                Type subtype = subtype_ref.get(compiler);
                ace::String type_string = subtype.to_string(compiler);
                compiler->add_error(
                    expr.loc,
                    "cannot take address of variable of non-runtime type: "
                    "'%.*s'",
                    (int)type_string.len,
                    type_string.ptr);
                break;
            }

            if (subtype_ref.id) {
                expr.expr_type_ref = compiler->create_pointer_type(subtype_ref);
            }

            break;
        }
        case UnaryOp_Dereference: {
            analyze_expr(compiler, state, expr.unary.left_ref);

            TypeRef subtype_ref =
                expr.unary.left_ref.get(compiler).expr_type_ref;
            Type subtype = subtype_ref.get(compiler);

            if (subtype.kind != TypeKind_Pointer) {
                ace::String type_string = subtype.to_string(compiler);
                compiler->add_error(
                    expr.loc,
                    "cannot dereference variable of type: '%.*s'",
                    (int)type_string.len,
                    type_string.ptr);
                break;
            }

            expr.expr_type_ref = subtype.pointer.sub_type;

            break;
        }
        case UnaryOp_Negate: {
            compiler->add_error(expr.loc, "'-' not implemented");
            break;
        }
        case UnaryOp_Not: {
            compiler->add_error(expr.loc, "'!' not implemented");
            break;
        }
        }

        break;
    }

    case ExprKind_Binary: {

        switch (expr.binary.op) {
        case BinaryOp_Unknown:
        case BinaryOp_MAX: ACE_ASSERT(0); break;

        case BinaryOp_Add:
        case BinaryOp_Sub:
        case BinaryOp_Mul:
        case BinaryOp_Div:
        case BinaryOp_Mod: {
            size_t error_checkpoint = compiler->get_error_checkpoint();

            analyze_expr(
                compiler, state, expr.binary.left_ref, expected_type_ref);
            Expr left = expr.binary.left_ref.get(compiler);
            Type left_type = left.expr_type_ref.get(compiler);

            if (left_type.kind == TypeKind_Unknown ||
                left_type.kind == TypeKind_UntypedInt ||
                left_type.kind == TypeKind_UntypedFloat) {
                compiler->restore_error_checkpoint(error_checkpoint);

                analyze_expr(
                    compiler, state, expr.binary.left_ref, expected_type_ref);
                left = expr.binary.left_ref.get(compiler);
                left_type = left.expr_type_ref.get(compiler);
            }

            error_checkpoint = compiler->get_error_checkpoint();

            analyze_expr(
                compiler, state, expr.binary.right_ref, expected_type_ref);
            Expr right = expr.binary.right_ref.get(compiler);
            Type right_type = right.expr_type_ref.get(compiler);

            if (right_type.kind == TypeKind_Unknown ||
                right_type.kind == TypeKind_UntypedInt ||
                right_type.kind == TypeKind_UntypedFloat) {
                compiler->restore_error_checkpoint(error_checkpoint);

                analyze_expr(
                    compiler, state, expr.binary.right_ref, expected_type_ref);
                right = expr.binary.right_ref.get(compiler);
                right_type = right.expr_type_ref.get(compiler);
            }

            if (left.expr_type_ref.id != right.expr_type_ref.id) {
                compiler->add_error(
                    expr.loc,
                    "mismatched types for binary expression operands");
                break;
            }

            Type type = left_type;
            if (type.kind == TypeKind_UntypedInt ||
                type.kind == TypeKind_UntypedFloat) {
                compiler->add_error(
                    expr.loc,
                    "cannot use compile-time integers in binary expression");
                break;
            }

            if (type.kind != TypeKind_Int && type.kind != TypeKind_Float) {
                compiler->add_error(
                    expr.loc, "binary expression expects numeric operands");
                break;
            }

            expr.expr_type_ref = left.expr_type_ref;
            break;
        }
        case BinaryOp_BitAnd:
        case BinaryOp_BitOr:
        case BinaryOp_BitXor: {
            size_t error_checkpoint = compiler->get_error_checkpoint();

            analyze_expr(compiler, state, expr.binary.left_ref);
            Expr left = expr.binary.left_ref.get(compiler);
            Type left_type = left.expr_type_ref.get(compiler);

            if (left_type.kind == TypeKind_Unknown ||
                left_type.kind == TypeKind_UntypedInt ||
                left_type.kind == TypeKind_UntypedFloat) {
                compiler->restore_error_checkpoint(error_checkpoint);

                analyze_expr(
                    compiler, state, expr.binary.left_ref, expected_type_ref);
                left = expr.binary.left_ref.get(compiler);
                left_type = left.expr_type_ref.get(compiler);
            }

            error_checkpoint = compiler->get_error_checkpoint();

            analyze_expr(compiler, state, expr.binary.right_ref);
            Expr right = expr.binary.right_ref.get(compiler);
            Type right_type = right.expr_type_ref.get(compiler);

            if (right_type.kind == TypeKind_Unknown ||
                right_type.kind == TypeKind_UntypedInt ||
                right_type.kind == TypeKind_UntypedFloat) {
                compiler->restore_error_checkpoint(error_checkpoint);

                analyze_expr(
                    compiler, state, expr.binary.right_ref, expected_type_ref);
                right = expr.binary.right_ref.get(compiler);
                right_type = right.expr_type_ref.get(compiler);
            }

            if (left.expr_type_ref.id != right.expr_type_ref.id) {
                compiler->add_error(
                    expr.loc,
                    "mismatched types for bitwise expression operands");
                break;
            }

            Type type = left_type;
            if (type.kind == TypeKind_UntypedInt ||
                type.kind == TypeKind_UntypedFloat) {
                ace::String type_string = type.to_string(compiler);
                compiler->add_error(
                    expr.loc,
                    "bitwise expression expects runtime numeric types, "
                    "instead got '%.*s'",
                    (int)type_string.len,
                    type_string.ptr);
                break;
            }

            if (type.kind != TypeKind_Int) {
                compiler->add_error(
                    expr.loc, "bitwise expression expects numeric operands");
                break;
            }

            expr.expr_type_ref = left.expr_type_ref;
            break;
        }

        case BinaryOp_Equal:
        case BinaryOp_NotEqual:
        case BinaryOp_Greater:
        case BinaryOp_GreaterEqual:
        case BinaryOp_Less:
        case BinaryOp_LessEqual: {
            analyze_expr(compiler, state, expr.binary.left_ref);
            analyze_expr(compiler, state, expr.binary.right_ref);
            Expr left = expr.binary.left_ref.get(compiler);
            Expr right = expr.binary.right_ref.get(compiler);

            if (left.expr_type_ref.id != right.expr_type_ref.id) {
                compiler->add_error(
                    expr.loc, "mismatched types for comparison operands");
                break;
            }

            Type type = left.expr_type_ref.get(compiler);
            if (type.kind == TypeKind_UntypedInt ||
                type.kind == TypeKind_UntypedFloat) {
                ace::String type_string = type.to_string(compiler);
                compiler->add_error(
                    expr.loc,
                    "comparison expression expects runtime numeric types, "
                    "instead got '%.*s'",
                    (int)type_string.len,
                    type_string.ptr);
                break;
            }

            if (type.kind != TypeKind_Int && type.kind != TypeKind_Float) {
                compiler->add_error(
                    expr.loc, "comparison expression expects numeric operands");
                break;
            }

            expr.expr_type_ref = compiler->bool_type;
            break;
        }

        case BinaryOp_LShift:
        case BinaryOp_RShift: {
            analyze_expr(compiler, state, expr.binary.left_ref);
            analyze_expr(compiler, state, expr.binary.right_ref);
            Expr left = expr.binary.left_ref.get(compiler);
            Expr right = expr.binary.right_ref.get(compiler);

            Type left_type = left.expr_type_ref.get(compiler);
            if (left_type.kind != TypeKind_Int) {
                ace::String type_string = left_type.to_string(compiler);
                compiler->add_error(
                    left.loc,
                    "bit shift expects runtime numeric types, instead "
                    "got '%.*s'",
                    (int)type_string.len,
                    type_string.ptr);
                break;
            }

            Type right_type = right.expr_type_ref.get(compiler);
            if (right_type.kind != TypeKind_Int) {
                ace::String type_string = right_type.to_string(compiler);
                compiler->add_error(
                    right.loc,
                    "bit shift expects runtime numeric types, instead "
                    "got '%.*s'",
                    (int)type_string.len,
                    type_string.ptr);
                break;
            }

            if (right_type.int_.is_signed) {
                compiler->add_error(
                    right.loc,
                    "right side of bit shift operation needs to be unsigned");
                break;
            }

            expr.expr_type_ref = left.expr_type_ref;
            break;
        }
        }

        break;
    }
    }

    if (expected_type_ref.id != 0 &&
        expected_type_ref.id != expr.expr_type_ref.id) {
        Type expected_type = expected_type_ref.get(compiler);
        Type expr_type = expr.expr_type_ref.get(compiler);

        ace::String expected_type_str = expected_type.to_string(compiler);
        ace::String expr_type_str = expr_type.to_string(compiler);

        compiler->add_error(
            expr.loc,
            "unmatched types, expecting '%.*s', but got '%.*s'",
            (int)expected_type_str.len,
            expected_type_str.ptr,
            (int)expr_type_str.len,
            expr_type_str.ptr);
    }

    compiler->exprs[expr_ref.id] = expr;
}

static void
analyze_stmt(Compiler *compiler, AnalyzerState *state, StmtRef stmt_ref)
{
    ZoneScoped;

    ACE_ASSERT(stmt_ref.id > 0);
    Stmt stmt = compiler->stmts[stmt_ref.id];

    switch (stmt.kind) {
    case StmtKind_Unknown: {
        ACE_ASSERT(0);
        break;
    }

    case StmtKind_Assign: {
        analyze_expr(compiler, state, stmt.assign.assigned_expr_ref);

        Expr assigned_expr = stmt.assign.assigned_expr_ref.get(compiler);
        if (!stmt.assign.assigned_expr_ref.is_lvalue(compiler)) {
            compiler->add_error(
                assigned_expr.loc, "expression is not assignable");
            break;
        }

        analyze_expr(
            compiler,
            state,
            stmt.assign.value_expr_ref,
            assigned_expr.expr_type_ref);

        break;
    }

    case StmtKind_Block: {
        Scope *block_scope = Scope::create(
            compiler, state->file_ref, *state->scope_stack.last());

        state->scope_stack.push_back(block_scope);
        for (StmtRef sub_stmt_ref : stmt.block.stmt_refs) {
            analyze_stmt(compiler, state, sub_stmt_ref);
        }
        state->scope_stack.pop();
        break;
    }

    case StmtKind_Expr: {
        analyze_expr(compiler, state, stmt.expr.expr_ref);
        break;
    }

    case StmtKind_Decl: {
        analyze_decl(compiler, state, stmt.decl.decl_ref);
        break;
    }

    case StmtKind_Return: {
        compiler->add_error(stmt.loc, "unimplemented return stmt");
        break;
    }

    case StmtKind_If: {
        TypeRef bool_type = compiler->bool_type;
        analyze_expr(compiler, state, stmt.if_.cond_expr_ref, bool_type);

        analyze_stmt(compiler, state, stmt.if_.true_stmt_ref);
        if (stmt.if_.false_stmt_ref.id) {
            analyze_stmt(compiler, state, stmt.if_.false_stmt_ref);
        }

        break;
    }

    case StmtKind_While: {
        TypeRef bool_type = compiler->bool_type;
        analyze_expr(compiler, state, stmt.while_.cond_expr_ref, bool_type);

        analyze_stmt(compiler, state, stmt.while_.true_stmt_ref);
        break;
    }
    }

    compiler->stmts[stmt_ref.id] = stmt;
}

static void
analyze_decl(Compiler *compiler, AnalyzerState *state, DeclRef decl_ref)
{
    ZoneScoped;

    ACE_ASSERT(decl_ref.id > 0);
    Decl decl = compiler->decls[decl_ref.id];

    switch (decl.kind) {
    case DeclKind_Unknown: {
        ACE_ASSERT(0);
        break;
    }
    case DeclKind_ConstDecl: {
        compiler->add_error(decl.loc, "const decl unimplemented");
        break;
    }
    case DeclKind_Function: {
        decl.func.scope = Scope::create(
            compiler, state->file_ref, *state->scope_stack.last());

        for (ExprRef return_type_expr_ref : decl.func.return_type_expr_refs) {
            analyze_expr(
                compiler, state, return_type_expr_ref, compiler->type_type);
        }

        TypeRef return_type = {};
        if (decl.func.return_type_expr_refs.len == 0) {
            return_type = compiler->void_type;
        } else if (decl.func.return_type_expr_refs.len == 1) {
            return_type =
                decl.func.return_type_expr_refs[0].get(compiler).as_type_ref;
        } else {
            ace::Slice<TypeRef> fields = compiler->arena->alloc<TypeRef>(
                decl.func.return_type_expr_refs.len);

            for (size_t i = 0; i < decl.func.return_type_expr_refs.len; ++i) {
                fields[i] = decl.func.return_type_expr_refs[i]
                                .get(compiler)
                                .as_type_ref;
            }

            return_type = compiler->create_tuple_type(fields);
        }

        ace::Slice<TypeRef> param_types =
            compiler->arena->alloc<TypeRef>(decl.func.param_decl_refs.len);

        for (size_t i = 0; i < decl.func.param_decl_refs.len; ++i) {
            DeclRef param_decl_ref = decl.func.param_decl_refs[i];
            analyze_decl(compiler, state, param_decl_ref);
            param_types[i] = param_decl_ref.get(compiler).decl_type_ref;

            decl.func.scope->add(compiler, param_decl_ref);
        }

        decl.decl_type_ref = compiler->create_func_type(
            return_type, param_types, decl.func.flags & FunctionFlags_VarArg);

        state->scope_stack.push_back(decl.func.scope);
        for (StmtRef stmt_ref : decl.func.body_stmts) {
            analyze_stmt(compiler, state, stmt_ref);
        }
        state->scope_stack.pop();

        break;
    }
    case DeclKind_FunctionParameter: {
        analyze_expr(
            compiler, state, decl.func_param.type_expr, compiler->type_type);
        decl.decl_type_ref =
            decl.func_param.type_expr.get(compiler).as_type_ref;
        break;
    }
    case DeclKind_LocalVarDecl: {
        Scope *scope = *state->scope_stack.last();
        scope->add(compiler, decl_ref);

        TypeRef var_type = {0};

        if (decl.local_var_decl.type_expr.id > 0) {
            analyze_expr(
                compiler,
                state,
                decl.local_var_decl.type_expr,
                compiler->type_type);
            var_type = decl.local_var_decl.type_expr.get(compiler).as_type_ref;
        }

        if (decl.local_var_decl.value_expr.id > 0) {
            analyze_expr(
                compiler, state, decl.local_var_decl.value_expr, var_type);
            if (var_type.id == 0) {
                var_type =
                    decl.local_var_decl.value_expr.get(compiler).expr_type_ref;
            }
        }

        decl.decl_type_ref = var_type;

        if (decl.decl_type_ref.id == 0) {
            compiler->add_error(
                decl.loc, "could not resolve type for variable declaration");
            break;
        }

        if (!decl.decl_type_ref.is_runtime(compiler)) {
            Type decl_type = decl.decl_type_ref.get(compiler);
            ace::String type_string = decl_type.to_string(compiler);
            compiler->add_error(
                decl.loc,
                "cannot create variable of non-runtime type: "
                "'%.*s'",
                (int)type_string.len,
                type_string.ptr);
        }

        break;
    }
    case DeclKind_GlobalVarDecl: {
        Scope *scope = *state->scope_stack.last();
        if (scope->lookup(decl.name).id != decl_ref.id) {
            scope->add(compiler, decl_ref);
        }

        TypeRef var_type = {0};

        if (decl.local_var_decl.type_expr.id > 0) {
            analyze_expr(
                compiler,
                state,
                decl.local_var_decl.type_expr,
                compiler->type_type);
            var_type = decl.local_var_decl.type_expr.get(compiler).as_type_ref;
        }

        if (decl.local_var_decl.value_expr.id > 0) {
            analyze_expr(
                compiler, state, decl.local_var_decl.value_expr, var_type);
            if (var_type.id == 0) {
                var_type =
                    decl.local_var_decl.value_expr.get(compiler).expr_type_ref;
            }

            compiler->add_error(
                decl.loc,
                "global variable initializers are not yet implemented");
        }

        decl.decl_type_ref = var_type;

        if (decl.decl_type_ref.id == 0) {
            compiler->add_error(
                decl.loc, "could not resolve type for variable declaration");
            break;
        }

        if (!decl.decl_type_ref.is_runtime(compiler)) {
            Type decl_type = decl.decl_type_ref.get(compiler);
            ace::String type_string = decl_type.to_string(compiler);
            compiler->add_error(
                decl.loc,
                "cannot create variable of non-runtime type: "
                "'%.*s'",
                (int)type_string.len,
                type_string.ptr);
        }

        break;
    }
    }

    compiler->decls[decl_ref.id] = decl;
}

void analyze_file(Compiler *compiler, FileRef file_ref)
{
    ZoneScoped;

    File file = compiler->files[file_ref.id];

    AnalyzerState state = {};
    state.file_ref = file_ref;
    state.scope_stack = ace::Array<Scope *>::create(compiler->arena);

    state.scope_stack.push_back(file.scope);

    // Register top level symbols
    for (DeclRef decl_ref : file.top_level_decls) {
        file.scope->add(compiler, decl_ref);
    }

    for (DeclRef decl_ref : file.top_level_decls) {
        analyze_decl(compiler, &state, decl_ref);
    }

    state.scope_stack.pop();

    ACE_ASSERT(state.scope_stack.len == 0);

    compiler->files[file_ref.id] = file;

    if (compiler->errors.len > 0) {
        compiler->halt_compilation();
    }
}
