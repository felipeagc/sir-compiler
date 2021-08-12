#include "compiler.hpp"

struct AnalyzerState {
    FileRef file_ref;
    Array<Scope *> scope_stack;
    Array<DeclRef> func_stack;
};

static bool
interp_expr(Compiler *compiler, ExprRef expr_ref, InterpValue *out_value)
{
    InterpValue value = {};
    Expr expr = expr_ref.get(compiler);

    switch (expr.kind) {
    case ExprKind_IntLiteral: {
        value.type_ref = compiler->untyped_int_type;
        value.i64 = expr.int_literal.u64;
        *out_value = value;
        return true;
    }
    case ExprKind_FloatLiteral: {
        value.type_ref = compiler->untyped_float_type;
        value.f64 = expr.float_literal.f64;
        *out_value = value;
        return true;
    }
    case ExprKind_BoolLiteral: {
        value.type_ref = compiler->bool_type;
        value.boolean = expr.bool_literal.bool_;
        *out_value = value;
        return true;
    }
    case ExprKind_BuiltinCall: {
        switch (expr.builtin_call.builtin) {
        case BuiltinFunction_Sizeof: {
            ExprRef param0_ref = expr.builtin_call.param_refs[0];
            LANG_ASSERT(compiler->expr_as_types[param0_ref.id].id);

            uint32_t size =
                compiler->expr_as_types[param0_ref.id].get(compiler).size_of(
                    compiler);

            value.type_ref = compiler->untyped_int_type;
            value.i64 = size;
            *out_value = value;
            return true;
        }
        case BuiltinFunction_Alignof: {
            ExprRef param0_ref = expr.builtin_call.param_refs[0];
            LANG_ASSERT(compiler->expr_as_types[param0_ref.id].id);

            uint32_t alignment =
                compiler->expr_as_types[param0_ref.id].get(compiler).align_of(
                    compiler);

            value.type_ref = compiler->untyped_int_type;
            value.i64 = alignment;
            *out_value = value;
            return true;
        }
        case BuiltinFunction_Defined: {
            ExprRef param0_ref = expr.builtin_call.param_refs[0];
            Expr param0 = param0_ref.get(compiler);
            LANG_ASSERT(param0.kind == ExprKind_StringLiteral);

            value.type_ref = compiler->bool_type;
            value.boolean = compiler->defines.get(param0.str_literal.str);
            *out_value = value;
            return true;
        }
        default: break;
        }
    }
    case ExprKind_Binary: {
        InterpValue left = {};
        InterpValue right = {};
        if (!interp_expr(compiler, expr.binary.left_ref, &left)) {
            return false;
        }
        if (!interp_expr(compiler, expr.binary.right_ref, &right)) {
            return false;
        }

        LANG_ASSERT(left.type_ref.id == right.type_ref.id);

        switch (expr.binary.op) {
        case BinaryOp_Unknown:
        case BinaryOp_MAX: LANG_ASSERT(0); break;

        default: return false;

        case BinaryOp_And: {
            value.type_ref = compiler->bool_type;
            value.boolean = left.boolean && right.boolean;
            *out_value = value;
            return true;
        }
        case BinaryOp_Or: {
            value.type_ref = compiler->bool_type;
            value.boolean = left.boolean || right.boolean;
            *out_value = value;
            return true;
        }
        }
        break;
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

    LANG_ASSERT(expr_ref.id > 0);
    Expr expr = compiler->exprs[expr_ref.id];

    switch (expr.kind) {
    case ExprKind_Unknown: {
        LANG_ASSERT(0);
        break;
    }

    case ExprKind_VoidType: {
        compiler->expr_types[expr_ref] = compiler->type_type;
        compiler->expr_as_types[expr_ref] = compiler->void_type;
        break;
    }

    case ExprKind_BoolType: {
        compiler->expr_types[expr_ref] = compiler->type_type;
        compiler->expr_as_types[expr_ref] = compiler->bool_type;
        break;
    }

    case ExprKind_IntType: {
        compiler->expr_types[expr_ref] = compiler->type_type;
        if (expr.int_type.is_signed) {
            switch (expr.int_type.bits) {
            case 8:
                compiler->expr_as_types[expr_ref] = compiler->i8_type;
                break;
            case 16:
                compiler->expr_as_types[expr_ref] = compiler->i16_type;
                break;
            case 32:
                compiler->expr_as_types[expr_ref] = compiler->i32_type;
                break;
            case 64:
                compiler->expr_as_types[expr_ref] = compiler->i64_type;
                break;
            }
        } else {
            switch (expr.int_type.bits) {
            case 8:
                compiler->expr_as_types[expr_ref] = compiler->u8_type;
                break;
            case 16:
                compiler->expr_as_types[expr_ref] = compiler->u16_type;
                break;
            case 32:
                compiler->expr_as_types[expr_ref] = compiler->u32_type;
                break;
            case 64:
                compiler->expr_as_types[expr_ref] = compiler->u64_type;
                break;
            }
        }
        break;
    }

    case ExprKind_FloatType: {
        compiler->expr_types[expr_ref] = compiler->type_type;
        switch (expr.float_type.bits) {
        case 32: compiler->expr_as_types[expr_ref] = compiler->f32_type; break;
        case 64: compiler->expr_as_types[expr_ref] = compiler->f64_type; break;
        }
        break;
    }

    case ExprKind_ISizeType: {
        compiler->expr_types[expr_ref] = compiler->type_type;
        compiler->expr_as_types[expr_ref] = compiler->isize_type;
        break;
    }

    case ExprKind_USizeType: {
        compiler->expr_types[expr_ref] = compiler->type_type;
        compiler->expr_as_types[expr_ref] = compiler->usize_type;
        break;
    }

    case ExprKind_PointerType: {
        analyze_expr(
            compiler, state, expr.ptr_type.sub_expr_ref, compiler->type_type);
        TypeRef sub_type = compiler->expr_as_types[expr.ptr_type.sub_expr_ref];
        if (sub_type.id) {
            compiler->expr_types[expr_ref] = compiler->type_type;
            compiler->expr_as_types[expr_ref] =
                compiler->create_pointer_type(sub_type);
        }
        break;
    }

    case ExprKind_DistinctType: {
        analyze_expr(
            compiler,
            state,
            expr.distinct_type.sub_expr_ref,
            compiler->type_type);
        TypeRef sub_type =
            compiler->expr_as_types[expr.distinct_type.sub_expr_ref];
        if (sub_type.id) {
            compiler->expr_types[expr_ref] = compiler->type_type;
            compiler->expr_as_types[expr_ref] =
                compiler->create_distinct_type(sub_type);
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
            compiler->expr_as_types[expr.slice_type.subtype_expr_ref];
        if (sub_type.id) {
            compiler->expr_types[expr_ref] = compiler->type_type;
            compiler->expr_as_types[expr_ref] =
                compiler->create_slice_type(sub_type);
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
            compiler->expr_as_types[expr.array_type.subtype_expr_ref];
        if (sub_type.id) {
            InterpValue interp_value = {};
            if (interp_expr(
                    compiler, expr.array_type.size_expr_ref, &interp_value)) {
                LANG_ASSERT(
                    interp_value.type_ref.id == compiler->untyped_int_type.id);
                compiler->expr_types[expr_ref] = compiler->type_type;
                compiler->expr_as_types[expr_ref] =
                    compiler->create_array_type(sub_type, interp_value.i64);
            } else {
                compiler->add_error(
                    compiler->expr_locs[expr.array_type.size_expr_ref],
                    "array size expression does not evaluate to a compile-time "
                    "integer");
            }
        }
        break;
    }

    case ExprKind_StructType: {
        LANG_ASSERT(
            expr.struct_type.field_type_expr_refs.len ==
            expr.struct_type.field_names.len);

        bool has_invalid_type = false;

        for (ExprRef field_type_expr_ref :
             expr.struct_type.field_type_expr_refs) {
            analyze_expr(
                compiler, state, field_type_expr_ref, compiler->type_type);
            if (compiler->expr_as_types[field_type_expr_ref].id == 0) {
                has_invalid_type = true;
            }
        }

        StringMap<uint32_t> field_map =
            StringMap<uint32_t>::create(compiler->arena, 32);

        bool has_conflict = false;

        for (size_t i = 0; i < expr.struct_type.field_names.len; ++i) {
            auto field_name = expr.struct_type.field_names[i];
            if (field_map.get(field_name)) {
                has_conflict = true;
                compiler->add_error(
                    compiler->expr_locs[expr_ref],
                    "duplicate struct field: '%.*s'",
                    (int)field_name.len,
                    field_name.ptr);
                continue;
            }
            field_map.set(field_name, (uint32_t)i);
        }

        if (has_conflict || has_invalid_type) break;

        Slice<TypeRef> field_types = compiler->arena->alloc<TypeRef>(
            expr.struct_type.field_type_expr_refs.len);

        for (size_t i = 0; i < expr.struct_type.field_type_expr_refs.len; ++i) {
            field_types[i] =
                compiler
                    ->expr_as_types[expr.struct_type.field_type_expr_refs[i]];
        }

        compiler->expr_types[expr_ref] = compiler->type_type;
        compiler->expr_as_types[expr_ref] = compiler->create_struct_type(
            field_types, expr.struct_type.field_names.as_slice());

        break;
    }

    case ExprKind_BoolLiteral: {
        compiler->expr_types[expr_ref] = compiler->bool_type;
        break;
    }

    case ExprKind_IntLiteral: {
        if (expected_type_ref.id &&
            (expected_type_ref.get(compiler).is_runtime_numeric())) {
            compiler->expr_types[expr_ref] = expected_type_ref;
        } else {
            compiler->expr_types[expr_ref] = compiler->untyped_int_type;
        }
        break;
    }

    case ExprKind_FloatLiteral: {
        if (expected_type_ref.id &&
            expected_type_ref.get(compiler).kind == TypeKind_Float) {
            compiler->expr_types[expr_ref] = expected_type_ref;
        } else {
            compiler->expr_types[expr_ref] = compiler->untyped_float_type;
        }
        break;
    }

    case ExprKind_StringLiteral: {
        compiler->expr_types[expr_ref] =
            compiler->create_slice_type(compiler->u8_type);
        if (expected_type_ref.id) {
            Type expected_type = expected_type_ref.get(compiler);
            if (expected_type.kind == TypeKind_Pointer &&
                expected_type.pointer.sub_type.id == compiler->u8_type.id) {
                compiler->expr_types[expr_ref] = expected_type_ref;
            }
        }
        break;
    }

    case ExprKind_NullLiteral: {
        if (expected_type_ref.id &&
            expected_type_ref.get(compiler).kind == TypeKind_Pointer) {
            compiler->expr_types[expr_ref] = expected_type_ref;
        } else {
            compiler->expr_types[expr_ref] =
                compiler->create_pointer_type(compiler->void_type);
        }
        break;
    }

    case ExprKind_VoidLiteral: {
        compiler->expr_types[expr_ref] = compiler->void_type;
        break;
    }

    case ExprKind_Identifier: {
        Scope *scope = *state->scope_stack.last();
        DeclRef decl_ref = scope->lookup(expr.ident.str);
        if (decl_ref.id) {
            compiler->expr_types[expr_ref] = compiler->decl_types[decl_ref];
            compiler->expr_as_types[expr_ref] =
                compiler->decl_as_types[decl_ref];
            expr.ident.decl_ref = decl_ref;
        } else {
            compiler->add_error(
                compiler->expr_locs[expr_ref],
                "identifier '%.*s' does not refer to a symbol",
                (int)expr.ident.str.len,
                expr.ident.str.ptr);
        }

        break;
    }

    case ExprKind_Function: {
        compiler->add_error(
            compiler->expr_locs[expr_ref], "unimplemented function expr");
        break;
    }

    case ExprKind_FunctionCall: {
        analyze_expr(compiler, state, expr.func_call.func_expr_ref);

        ExprRef func_expr_ref = expr.func_call.func_expr_ref;
        Type func_type = compiler->expr_types[func_expr_ref].get(compiler);
        switch (func_type.kind) {
        case TypeKind_Function: {
            // Actual function call
            if (func_type.func.vararg) {
                if (func_type.func.param_types.len >
                    expr.func_call.param_refs.len) {
                    compiler->add_error(
                        compiler->expr_locs[expr_ref],
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
                        compiler->expr_locs[expr_ref],
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

            compiler->expr_types[expr_ref] = func_type.func.return_type;
            break;
        }

        case TypeKind_Type: {
            // Type cast

            if (expr.func_call.param_refs.len != 1) {
                compiler->add_error(
                    compiler->expr_locs[func_expr_ref],
                    "expected type cast to have 1 parameter");
                break;
            }

            LANG_ASSERT(compiler->expr_as_types[func_expr_ref].id != 0);

            TypeRef dest_type_ref = compiler->expr_as_types[func_expr_ref];
            Type dest_type = dest_type_ref.get(compiler);

            size_t error_checkpoint = compiler->get_error_checkpoint();

            analyze_expr(compiler, state, expr.func_call.param_refs[0]);

            TypeRef param_type_ref =
                compiler->expr_types[expr.func_call.param_refs[0]];
            Type param_type = param_type_ref.get(compiler);
            if (param_type.kind == TypeKind_Unknown ||
                param_type.kind == TypeKind_UntypedInt ||
                param_type.kind == TypeKind_UntypedFloat) {

                compiler->restore_error_checkpoint(error_checkpoint);

                analyze_expr(
                    compiler,
                    state,
                    expr.func_call.param_refs[0],
                    dest_type_ref.inner(compiler));

            } else if (!((param_type.kind == TypeKind_Int ||
                          param_type.kind == TypeKind_Float) &&
                         (dest_type.kind == TypeKind_Int ||
                          dest_type.kind == TypeKind_Float))) {
                compiler->add_error(
                    compiler->expr_locs[expr_ref], "invalid cast");
                break;
            }

            compiler->expr_types[expr_ref] = dest_type_ref;

            break;
        }

        default: {
            compiler->add_error(
                compiler->expr_locs[func_expr_ref],
                "expected expression to have function type");
            break;
        }
        }

        break;
    }

    case ExprKind_BuiltinCall: {
        switch (expr.builtin_call.builtin) {
        case BuiltinFunction_Unknown: LANG_ASSERT(0); break;
        case BuiltinFunction_Sizeof: {
            if (expr.builtin_call.param_refs.len != 1) {
                compiler->add_error(
                    compiler->expr_locs[expr_ref],
                    "expected 1 parameter for @sizeof");
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
                compiler->expr_types[expr_ref] = expected_type_ref;
            } else {
                compiler->expr_types[expr_ref] = compiler->untyped_int_type;
            }

            break;
        }
        case BuiltinFunction_Alignof: {
            if (expr.builtin_call.param_refs.len != 1) {
                compiler->add_error(
                    compiler->expr_locs[expr_ref],
                    "expected 1 parameter for @alignof");
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
                compiler->expr_types[expr_ref] = expected_type_ref;
            } else {
                compiler->expr_types[expr_ref] = compiler->untyped_int_type;
            }

            break;
        }
        case BuiltinFunction_PtrCast: {
            if (expr.builtin_call.param_refs.len != 2) {
                compiler->add_error(
                    compiler->expr_locs[expr_ref],
                    "expected 2 parameters for @ptrcast");
                break;
            }

            analyze_expr(
                compiler,
                state,
                expr.builtin_call.param_refs[0],
                compiler->type_type);

            analyze_expr(compiler, state, expr.builtin_call.param_refs[1]);

            ExprRef param0 = expr.builtin_call.param_refs[0];
            if (compiler->expr_as_types[param0].get(compiler).kind !=
                TypeKind_Pointer) {
                compiler->add_error(
                    compiler->expr_locs[param0], "expected pointer type");
                break;
            }

            if (compiler->expr_as_types[param0].get(compiler).kind !=
                TypeKind_Pointer) {
                compiler->add_error(
                    compiler->expr_locs[param0],
                    "expected expression of pointer type");
                break;
            }

            compiler->expr_types[expr_ref] = compiler->expr_as_types[param0];
            break;
        }
        case BuiltinFunction_Defined: {
            if (expr.builtin_call.param_refs.len != 1) {
                compiler->add_error(
                    compiler->expr_locs[expr_ref],
                    "expected 1 parameter for @defined");
                break;
            }

            ExprRef param0_ref = expr.builtin_call.param_refs[0];
            Expr param0 = param0_ref.get(compiler);

            if (param0.kind != ExprKind_StringLiteral) {
                compiler->add_error(
                    compiler->expr_locs[param0_ref], "expected string literal");
                break;
            }

            compiler->expr_types[expr_ref] = compiler->bool_type;
            break;
        }
        }

        break;
    }

    case ExprKind_Subscript: {
        analyze_expr(compiler, state, expr.subscript.left_ref);
        analyze_expr(
            compiler, state, expr.subscript.right_ref, compiler->u64_type);

        ExprRef left_ref = expr.subscript.left_ref;
        ExprRef right_ref = expr.subscript.right_ref;

        TypeRef indexed_type_ref = compiler->expr_types[left_ref];
        TypeRef index_type_ref = compiler->expr_types[right_ref];
        if (index_type_ref.id == 0 || indexed_type_ref.id == 0) {
            LANG_ASSERT(compiler->errors.len > 0);
            break;
        }

        Type index_type = index_type_ref.get(compiler);
        Type indexed_type = indexed_type_ref.get(compiler);

        if (index_type.kind != TypeKind_Int &&
            index_type.kind != TypeKind_UntypedInt) {
            compiler->add_error(
                compiler->expr_locs[right_ref],
                "subscript index is not an integer");
        }

        if (indexed_type.kind != TypeKind_Array &&
            indexed_type.kind != TypeKind_Slice) {
            compiler->add_error(
                compiler->expr_locs[left_ref],
                "accessed expression in subscript is not an array");
        }

        switch (indexed_type.kind) {
        case TypeKind_Array: {
            compiler->expr_types[expr_ref] = indexed_type.array.sub_type;
            break;
        }
        case TypeKind_Slice: {
            compiler->expr_types[expr_ref] = indexed_type.slice.sub_type;
            break;
        }
        default: LANG_ASSERT(0);
        }

        break;
    }

    case ExprKind_Access: {
        analyze_expr(compiler, state, expr.access.left_ref);

        Type accessed_type =
            compiler->expr_types[expr.access.left_ref].get(compiler);
        String type_name = accessed_type.to_string(compiler);

        Expr ident_expr = expr.access.accessed_ident_ref.get(compiler);
        LANG_ASSERT(ident_expr.kind == ExprKind_Identifier);
        String accessed_field = ident_expr.ident.str;

        switch (accessed_type.kind) {
        case TypeKind_Struct: {
            uint32_t field_index = 0;
            if (!accessed_type.struct_.field_map.get(
                    accessed_field, &field_index)) {
                compiler->add_error(
                    compiler->expr_locs[expr_ref],
                    "no struct field named '%.*s' for struct type '%.*s'",
                    (int)accessed_field.len,
                    accessed_field.ptr,
                    (int)type_name.len,
                    type_name.ptr);
                break;
            }

            TypeRef field_type = accessed_type.struct_.field_types[field_index];
            compiler->expr_types[expr_ref] = field_type;

            break;
        }
        default: {
            compiler->add_error(
                compiler->expr_locs[expr_ref],
                "invalid type for left side of access expression: '%.*s'",
                (int)type_name.len,
                type_name.ptr);
            break;
        }
        }

        break;
    }

    case ExprKind_Unary: {
        switch (expr.unary.op) {
        case UnaryOp_Unknown: LANG_ASSERT(0); break;
        case UnaryOp_AddressOf: {
            analyze_expr(compiler, state, expr.unary.left_ref);

            TypeRef subtype_ref = compiler->expr_types[expr.unary.left_ref];
            if (!subtype_ref.get(compiler).is_runtime()) {
                Type subtype = subtype_ref.get(compiler);
                String type_string = subtype.to_string(compiler);
                compiler->add_error(
                    compiler->expr_locs[expr_ref],
                    "cannot take address of variable of non-runtime type: "
                    "'%.*s'",
                    (int)type_string.len,
                    type_string.ptr);
                break;
            }

            if (!expr.unary.left_ref.is_lvalue(compiler)) {
                compiler->add_error(
                    compiler->expr_locs[expr_ref],
                    "cannot take address of non-lvalue");
                break;
            }

            if (subtype_ref.id) {
                compiler->expr_types[expr_ref] =
                    compiler->create_pointer_type(subtype_ref);
            }

            break;
        }
        case UnaryOp_Dereference: {
            analyze_expr(compiler, state, expr.unary.left_ref);

            TypeRef subtype_ref = compiler->expr_types[expr.unary.left_ref];
            Type subtype = subtype_ref.get(compiler);

            if (subtype.kind != TypeKind_Pointer) {
                String type_string = subtype.to_string(compiler);
                compiler->add_error(
                    compiler->expr_locs[expr_ref],
                    "cannot dereference variable of type: '%.*s'",
                    (int)type_string.len,
                    type_string.ptr);
                break;
            }

            compiler->expr_types[expr_ref] = subtype.pointer.sub_type;

            break;
        }
        case UnaryOp_Negate: {
            compiler->add_error(
                compiler->expr_locs[expr_ref], "'-' not implemented");
            break;
        }
        case UnaryOp_Not: {
            compiler->add_error(
                compiler->expr_locs[expr_ref], "'!' not implemented");
            break;
        }
        }

        break;
    }

    case ExprKind_Binary: {

        switch (expr.binary.op) {
        case BinaryOp_Unknown:
        case BinaryOp_MAX: LANG_ASSERT(0); break;

        case BinaryOp_Add:
        case BinaryOp_Sub:
        case BinaryOp_Mul:
        case BinaryOp_Div:
        case BinaryOp_Mod: {
            size_t error_checkpoint = compiler->get_error_checkpoint();

            if (expected_type_ref.id == 0) {
                TypeRef type_refs[2] = {};

                analyze_expr(compiler, state, expr.binary.left_ref);
                type_refs[0] = compiler->expr_types[expr.binary.left_ref];

                analyze_expr(compiler, state, expr.binary.right_ref);
                type_refs[1] = compiler->expr_types[expr.binary.right_ref];

                for (size_t i = 0; i < LANG_CARRAY_LENGTH(type_refs); ++i) {
                    Type type = type_refs[i].get(compiler);
                    if (type.is_runtime_numeric()) {
                        expected_type_ref = type_refs[i];
                        break;
                    }
                }
            }

            compiler->restore_error_checkpoint(error_checkpoint);

            if (expected_type_ref.id == 0) {
                compiler->add_error(
                    compiler->expr_locs[expr_ref],
                    "cannot infer type for binary expression");
                break;
            }

            analyze_expr(
                compiler, state, expr.binary.left_ref, expected_type_ref);
            analyze_expr(
                compiler, state, expr.binary.right_ref, expected_type_ref);
            ExprRef left_ref = expr.binary.left_ref;
            ExprRef right_ref = expr.binary.right_ref;

            TypeRef left_type_ref = compiler->expr_types[left_ref];
            TypeRef right_type_ref = compiler->expr_types[right_ref];
            if (left_type_ref.id != right_type_ref.id) {
                String left_type_str =
                    left_type_ref.get(compiler).to_string(compiler);
                String right_type_str =
                    right_type_ref.get(compiler).to_string(compiler);
                compiler->add_error(
                    compiler->expr_locs[expr_ref],
                    "mismatched types for binary expression operands, got "
                    "'%.*s' and '%.*s'",
                    (int)left_type_str.len,
                    left_type_str.ptr,
                    (int)right_type_str.len,
                    right_type_str.ptr);
                break;
            }

            Type type = left_type_ref.inner(compiler).get(compiler);
            if (!type.is_numeric()) {
                compiler->add_error(
                    compiler->expr_locs[expr_ref],
                    "binary expression expects numeric operands");
                break;
            }

            if (!type.is_runtime_numeric()) {
                String type_string = type.to_string(compiler);
                compiler->add_error(
                    compiler->expr_locs[expr_ref],
                    "binary expression expects runtime numeric types, "
                    "instead got '%.*s'",
                    (int)type_string.len,
                    type_string.ptr);
                break;
            }

            compiler->expr_types[expr_ref] = left_type_ref;
            break;
        }
        case BinaryOp_BitAnd:
        case BinaryOp_BitOr:
        case BinaryOp_BitXor: {
            size_t error_checkpoint = compiler->get_error_checkpoint();

            if (expected_type_ref.id == 0) {
                TypeRef type_refs[2] = {};

                analyze_expr(compiler, state, expr.binary.left_ref);
                type_refs[0] = compiler->expr_types[expr.binary.left_ref];

                analyze_expr(compiler, state, expr.binary.right_ref);
                type_refs[1] = compiler->expr_types[expr.binary.right_ref];

                for (size_t i = 0; i < LANG_CARRAY_LENGTH(type_refs); ++i) {
                    Type type = type_refs[i].get(compiler);
                    if (type.is_runtime_int()) {
                        expected_type_ref = type_refs[i];
                        break;
                    }
                }
            }

            compiler->restore_error_checkpoint(error_checkpoint);

            if (expected_type_ref.id == 0) {
                compiler->add_error(
                    compiler->expr_locs[expr_ref],
                    "cannot infer type for binary expression");
                break;
            }

            analyze_expr(
                compiler, state, expr.binary.left_ref, expected_type_ref);
            analyze_expr(
                compiler, state, expr.binary.right_ref, expected_type_ref);
            ExprRef left_ref = expr.binary.left_ref;
            ExprRef right_ref = expr.binary.right_ref;

            TypeRef left_type_ref = compiler->expr_types[left_ref];
            TypeRef right_type_ref = compiler->expr_types[right_ref];
            if (left_type_ref.id != right_type_ref.id) {
                String left_type_str =
                    left_type_ref.get(compiler).to_string(compiler);
                String right_type_str =
                    right_type_ref.get(compiler).to_string(compiler);
                compiler->add_error(
                    compiler->expr_locs[expr_ref],
                    "mismatched types for binary expression operands, got "
                    "'%.*s' and '%.*s'",
                    (int)left_type_str.len,
                    left_type_str.ptr,
                    (int)right_type_str.len,
                    right_type_str.ptr);
                break;
            }

            Type type = left_type_ref.inner(compiler).get(compiler);
            if (!type.is_int()) {
                compiler->add_error(
                    compiler->expr_locs[expr_ref],
                    "binary expression expects integer operands");
                break;
            }

            if (!type.is_runtime_int()) {
                String type_string = type.to_string(compiler);
                compiler->add_error(
                    compiler->expr_locs[expr_ref],
                    "binary expression expects runtime integer types, "
                    "instead got '%.*s'",
                    (int)type_string.len,
                    type_string.ptr);
                break;
            }

            compiler->expr_types[expr_ref] = left_type_ref;
            break;
        }

        case BinaryOp_Equal:
        case BinaryOp_NotEqual:
        case BinaryOp_Greater:
        case BinaryOp_GreaterEqual:
        case BinaryOp_Less:
        case BinaryOp_LessEqual: {
            size_t error_checkpoint = compiler->get_error_checkpoint();

            TypeRef common_type_ref = {0};

            {
                TypeRef type_refs[2] = {};

                analyze_expr(compiler, state, expr.binary.left_ref);
                type_refs[0] = compiler->expr_types[expr.binary.left_ref];

                analyze_expr(compiler, state, expr.binary.right_ref);
                type_refs[1] = compiler->expr_types[expr.binary.right_ref];

                for (size_t i = 0; i < LANG_CARRAY_LENGTH(type_refs); ++i) {
                    Type type = type_refs[i].get(compiler);
                    if (type.kind == TypeKind_Int ||
                        type.kind == TypeKind_Float) {
                        common_type_ref = type_refs[i];
                        break;
                    }
                }
            }

            compiler->restore_error_checkpoint(error_checkpoint);

            analyze_expr(
                compiler, state, expr.binary.left_ref, common_type_ref);
            analyze_expr(
                compiler, state, expr.binary.right_ref, common_type_ref);
            ExprRef left_ref = expr.binary.left_ref;
            ExprRef right_ref = expr.binary.right_ref;

            TypeRef left_type_ref = compiler->expr_types[left_ref];
            TypeRef right_type_ref = compiler->expr_types[right_ref];

            if (left_type_ref.id != right_type_ref.id) {
                compiler->add_error(
                    compiler->expr_locs[expr_ref],
                    "mismatched types for comparison operands");
                break;
            }

            Type type = left_type_ref.inner(compiler).get(compiler);

            bool is_valid_bool_op = type.kind == TypeKind_Bool &&
                                    (expr.binary.op == BinaryOp_Equal ||
                                     expr.binary.op == BinaryOp_NotEqual);

            if (type.is_numeric()) {
                if (!type.is_runtime_numeric()) {
                    String type_string = type.to_string(compiler);
                    compiler->add_error(
                        compiler->expr_locs[expr_ref],
                        "comparison expression expects runtime numeric types, "
                        "instead got '%.*s'",
                        (int)type_string.len,
                        type_string.ptr);
                    break;
                }
            } else if (!is_valid_bool_op) {
                String type_string = type.to_string(compiler);
                compiler->add_error(
                    compiler->expr_locs[expr_ref],
                    "invalid type for binary expression '%*.s'",
                    (int)type_string.len,
                    type_string.ptr);
                break;
            }

            compiler->expr_types[expr_ref] = compiler->bool_type;
            break;
        }

        case BinaryOp_LShift:
        case BinaryOp_RShift: {
            analyze_expr(
                compiler, state, expr.binary.left_ref, expected_type_ref);
            analyze_expr(compiler, state, expr.binary.right_ref);
            ExprRef left_ref = expr.binary.left_ref;
            ExprRef right_ref = expr.binary.right_ref;

            Type left_type =
                compiler->expr_types[left_ref].inner(compiler).get(compiler);
            if (!left_type.is_runtime_int()) {
                String type_string = left_type.to_string(compiler);
                compiler->add_error(
                    compiler->expr_locs[left_ref],
                    "bit shift expects runtime integer types, instead "
                    "got '%.*s'",
                    (int)type_string.len,
                    type_string.ptr);
                break;
            }

            Type right_type =
                compiler->expr_types[right_ref].inner(compiler).get(compiler);
            if (!right_type.is_runtime_int()) {
                String type_string = right_type.to_string(compiler);
                compiler->add_error(
                    compiler->expr_locs[right_ref],
                    "bit shift expects runtime integer types, instead "
                    "got '%.*s'",
                    (int)type_string.len,
                    type_string.ptr);
                break;
            }

            if (right_type.int_.is_signed) {
                compiler->add_error(
                    compiler->expr_locs[right_ref],
                    "right side of bit shift operation needs to be unsigned");
                break;
            }

            compiler->expr_types[expr_ref] = compiler->expr_types[left_ref];
            break;
        }

        case BinaryOp_And:
        case BinaryOp_Or: {
            analyze_expr(
                compiler, state, expr.binary.left_ref, compiler->bool_type);
            analyze_expr(
                compiler, state, expr.binary.right_ref, compiler->bool_type);

            compiler->expr_types[expr_ref] = compiler->bool_type;
            break;
        }
        }

        break;
    }
    }

    if (expected_type_ref.id != 0 &&
        expected_type_ref.id != compiler->expr_types[expr_ref].id) {
        Type expected_type = expected_type_ref.get(compiler);
        Type expr_type = compiler->expr_types[expr_ref].get(compiler);

        String expected_type_str = expected_type.to_string(compiler);
        String expr_type_str = expr_type.to_string(compiler);

        compiler->add_error(
            compiler->expr_locs[expr_ref],
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

    LANG_ASSERT(stmt_ref.id > 0);
    Stmt stmt = compiler->stmts[stmt_ref.id];

    switch (stmt.kind) {
    case StmtKind_Unknown: {
        LANG_ASSERT(0);
        break;
    }

    case StmtKind_Assign: {
        analyze_expr(compiler, state, stmt.assign.assigned_expr_ref);

        if (!stmt.assign.assigned_expr_ref.is_lvalue(compiler)) {
            compiler->add_error(
                compiler->expr_locs[stmt.assign.assigned_expr_ref],
                "expression is not assignable");
            break;
        }

        analyze_expr(
            compiler,
            state,
            stmt.assign.value_expr_ref,
            compiler->expr_types[stmt.assign.assigned_expr_ref]);

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
        LANG_ASSERT(state->func_stack.len > 0);

        DeclRef func_decl_ref = *state->func_stack.last();
        Type func_type = compiler->decl_types[func_decl_ref].get(compiler);
        LANG_ASSERT(func_type.kind == TypeKind_Function);

        String func_name = compiler->decl_names[func_decl_ref];

        if (func_type.func.return_type.id == 0) {
            if (stmt.return_.returned_expr_ref.id > 0) {
                compiler->add_error(
                    compiler->stmt_locs[stmt_ref],
                    "function '%.*s' does not return a value",
                    (int)func_name.len,
                    func_name.ptr);
            }
        } else {
            if (stmt.return_.returned_expr_ref.id == 0) {
                compiler->add_error(
                    compiler->stmt_locs[stmt_ref],
                    "function '%.*s' must return a value",
                    (int)func_name.len,
                    func_name.ptr);
            } else {
                analyze_expr(
                    compiler,
                    state,
                    stmt.return_.returned_expr_ref,
                    func_type.func.return_type);
            }
        }

        break;
    }

    case StmtKind_When: {
        TypeRef bool_type = compiler->bool_type;
        analyze_expr(compiler, state, stmt.when.cond_expr_ref, bool_type);

        InterpValue value = {};
        if (!interp_expr(compiler, stmt.when.cond_expr_ref, &value)) {
            compiler->add_error(
                compiler->expr_locs[stmt.when.cond_expr_ref],
                "could not evaluate compile time expression");
            break;
        }

        LANG_ASSERT(value.type_ref.id == bool_type.id);
        stmt.when.cond_value = value.boolean;

        if (stmt.when.cond_value) {
            analyze_stmt(compiler, state, stmt.when.true_stmt_ref);
        } else if (stmt.when.false_stmt_ref.id) {
            analyze_stmt(compiler, state, stmt.when.false_stmt_ref);
        }

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

    LANG_ASSERT(decl_ref.id > 0);
    Decl decl = compiler->decls[decl_ref.id];
    String decl_name = compiler->decl_names[decl_ref.id];

    switch (decl.kind) {
    case DeclKind_Unknown: {
        LANG_ASSERT(0);
        break;
    }

    case DeclKind_Type: {
        Scope *scope = *state->scope_stack.last();
        if (scope->lookup(decl_name).id != decl_ref.id) {
            scope->add(compiler, decl_ref);
        }

        analyze_expr(
            compiler, state, decl.type_decl.type_expr, compiler->type_type);
        TypeRef as_type_ref = compiler->expr_as_types[decl.type_decl.type_expr];
        if (as_type_ref.id > 0) {
            compiler->decl_as_types[decl_ref] = as_type_ref;
            compiler->decl_types[decl_ref] = compiler->type_type;
        }
        break;
    }

    case DeclKind_ConstDecl: {
        compiler->add_error(
            compiler->decl_locs[decl_ref], "const decl unimplemented");
        break;
    }

    case DeclKind_Function: {
        decl.func->scope = Scope::create(
            compiler, state->file_ref, *state->scope_stack.last());

        for (ExprRef return_type_expr_ref : decl.func->return_type_expr_refs) {
            analyze_expr(
                compiler, state, return_type_expr_ref, compiler->type_type);
        }

        TypeRef return_type = {};
        if (decl.func->return_type_expr_refs.len == 0) {
            return_type = compiler->void_type;
        } else if (decl.func->return_type_expr_refs.len == 1) {
            return_type =
                compiler->expr_as_types[decl.func->return_type_expr_refs[0]];
        } else {
            Slice<TypeRef> fields = compiler->arena->alloc<TypeRef>(
                decl.func->return_type_expr_refs.len);

            for (size_t i = 0; i < decl.func->return_type_expr_refs.len; ++i) {
                fields[i] =
                    compiler
                        ->expr_as_types[decl.func->return_type_expr_refs[i]];
            }

            return_type = compiler->create_tuple_type(fields);
        }

        Slice<TypeRef> param_types =
            compiler->arena->alloc<TypeRef>(decl.func->param_decl_refs.len);

        for (size_t i = 0; i < decl.func->param_decl_refs.len; ++i) {
            DeclRef param_decl_ref = decl.func->param_decl_refs[i];
            analyze_decl(compiler, state, param_decl_ref);
            param_types[i] = compiler->decl_types[param_decl_ref];

            decl.func->scope->add(compiler, param_decl_ref);
        }

        compiler->decl_types[decl_ref] = compiler->create_func_type(
            return_type, param_types, decl.func->flags & FunctionFlags_VarArg);

        compiler->decls[decl_ref.id] = decl;

        state->func_stack.push_back(decl_ref);
        state->scope_stack.push_back(decl.func->scope);
        for (StmtRef stmt_ref : decl.func->body_stmts) {
            analyze_stmt(compiler, state, stmt_ref);
        }
        state->scope_stack.pop();
        state->func_stack.pop();

        break;
    }

    case DeclKind_FunctionParameter: {
        analyze_expr(
            compiler, state, decl.func_param.type_expr, compiler->type_type);
        compiler->decl_types[decl_ref] =
            compiler->expr_as_types[decl.func_param.type_expr];
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
            var_type = compiler->expr_as_types[decl.local_var_decl.type_expr];
        }

        if (decl.local_var_decl.value_expr.id > 0) {
            analyze_expr(
                compiler, state, decl.local_var_decl.value_expr, var_type);
            if (var_type.id == 0) {
                var_type = compiler->expr_types[decl.local_var_decl.value_expr];
            }
        }

        compiler->decl_types[decl_ref] = var_type;

        if (var_type.id == 0) {
            compiler->add_error(
                compiler->decl_locs[decl_ref],
                "could not resolve type for variable declaration");
            break;
        }

        if (!var_type.get(compiler).is_runtime()) {
            Type decl_type = var_type.get(compiler);
            String type_string = decl_type.to_string(compiler);
            compiler->add_error(
                compiler->decl_locs[decl_ref],
                "cannot create variable of non-runtime type: "
                "'%.*s'",
                (int)type_string.len,
                type_string.ptr);
        }

        break;
    }

    case DeclKind_GlobalVarDecl: {
        Scope *scope = *state->scope_stack.last();
        if (scope->lookup(decl_name).id != decl_ref.id) {
            scope->add(compiler, decl_ref);
        }

        TypeRef var_type = {0};

        if (decl.local_var_decl.type_expr.id > 0) {
            analyze_expr(
                compiler,
                state,
                decl.local_var_decl.type_expr,
                compiler->type_type);
            var_type = compiler->expr_as_types[decl.local_var_decl.type_expr];
        }

        if (decl.local_var_decl.value_expr.id > 0) {
            analyze_expr(
                compiler, state, decl.local_var_decl.value_expr, var_type);
            if (var_type.id == 0) {
                var_type = compiler->expr_types[decl.local_var_decl.value_expr];
            }

            compiler->add_error(
                compiler->decl_locs[decl_ref],
                "global variable initializers are not yet implemented");
        }

        compiler->decl_types[decl_ref] = var_type;

        if (var_type.id == 0) {
            compiler->add_error(
                compiler->decl_locs[decl_ref],
                "could not resolve type for variable declaration");
            break;
        }

        if (!var_type.get(compiler).is_runtime()) {
            Type decl_type = var_type.get(compiler);
            String type_string = decl_type.to_string(compiler);
            compiler->add_error(
                compiler->decl_locs[decl_ref],
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
    state.scope_stack = Array<Scope *>::create(compiler->arena);
    state.func_stack = Array<DeclRef>::create(compiler->arena);

    state.scope_stack.push_back(file.scope);

    // Register top level symbols
    for (DeclRef decl_ref : file.top_level_decls) {
        file.scope->add(compiler, decl_ref);
    }

    for (DeclRef decl_ref : file.top_level_decls) {
        analyze_decl(compiler, &state, decl_ref);
    }

    state.scope_stack.pop();

    LANG_ASSERT(state.scope_stack.len == 0);

    compiler->files[file_ref.id] = file;

    if (compiler->errors.len > 0) {
        compiler->halt_compilation();
    }
}
