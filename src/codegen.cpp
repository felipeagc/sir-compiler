#include "compiler.hpp"

#include <sir.h>

struct CodegenContext;

struct CodegenValue {
    bool is_lvalue;
    SIRInstRef inst_ref;
};

struct CodegenContext {
    FileRef file_ref;
    SIRModule *module;
    SIRBuilder *builder;
    Array<SIRType *> type_values;
    Array<CodegenValue> expr_values;
    Array<CodegenValue> decl_values;

    Array<SIRInstRef> function_stack;
};

static void
codegen_decl(Compiler *compiler, CodegenContext *ctx, DeclRef decl_ref);

static SIRInstRef load_lvalue(CodegenContext *ctx, const CodegenValue &value)
{
    if (value.is_lvalue) {
        return SIRBuilderInsertLoad(ctx->builder, value.inst_ref);
    }
    return value.inst_ref;
}

static SIRInstRef value_into_bool(CodegenContext *ctx, SIRInstRef inst_ref)
{
    SIRType *type = SIRModuleGetInstType(ctx->module, inst_ref);
    if (SIRModuleGetTypeKind(ctx->module, type) == SIRTypeKind_Int) {
        return SIRBuilderInsertBinop(
            ctx->builder,
            SIRBinaryOperation_INE,
            inst_ref,
            SIRBuilderInsertImmInt(ctx->builder, type, 0));
    }
    return inst_ref;
}

static SIRType *
get_ir_type(Compiler *compiler, SIRModule *module, const Type &type)
{
    (void)compiler;

    switch (type.kind) {
    case TypeKind_Unknown:
    case TypeKind_MAX:
    case TypeKind_Function:
    case TypeKind_UntypedInt:
    case TypeKind_UntypedFloat:
    case TypeKind_Type: return nullptr;

    case TypeKind_Void: {
        return SIRModuleGetVoidType(module);
    }
    case TypeKind_Bool: {
        return SIRModuleGetU8Type(module);
    }
    case TypeKind_Distinct: {
        return get_ir_type(compiler, module, type.distinct.sub_type.get(compiler));
    }
    case TypeKind_Int: {
        if (type.int_.is_signed) {
            switch (type.int_.bits) {
            case 8: return SIRModuleGetI8Type(module);
            case 16: return SIRModuleGetI16Type(module);
            case 32: return SIRModuleGetI32Type(module);
            case 64: return SIRModuleGetI64Type(module);
            }
        } else {
            switch (type.int_.bits) {
            case 8: return SIRModuleGetU8Type(module);
            case 16: return SIRModuleGetU16Type(module);
            case 32: return SIRModuleGetU32Type(module);
            case 64: return SIRModuleGetU64Type(module);
            }
        }
        break;
    }
    case TypeKind_Float: {
        switch (type.float_.bits) {
        case 32: return SIRModuleGetF32Type(module);
        case 64: return SIRModuleGetF64Type(module);
        }
        break;
    }
    case TypeKind_Pointer: {
        SIRType *subtype =
            get_ir_type(compiler, module, type.pointer.sub_type.get(compiler));
        return SIRModuleCreatePointerType(module, subtype);
    }
    case TypeKind_Slice: {
        SIRType *field_types[2] = {
            get_ir_type(
                compiler,
                module,
                compiler->create_pointer_type(type.slice.sub_type)
                    .get(compiler)),
            get_ir_type(compiler, module, compiler->u64_type.get(compiler)),
        };

        return SIRModuleCreateStructType(
            module, field_types, LANG_CARRAY_LENGTH(field_types), false);
    }
    case TypeKind_Array: {
        SIRType *subtype =
            get_ir_type(compiler, module, type.array.sub_type.get(compiler));
        return SIRModuleCreateArrayType(module, subtype, type.array.size);
    }
    case TypeKind_Tuple: {
        Slice<SIRType *> field_types =
            compiler->arena->alloc<SIRType *>(type.tuple.field_types.len);

        for (size_t i = 0; i < type.tuple.field_types.len; ++i) {
            field_types[i] = get_ir_type(
                compiler, module, type.tuple.field_types[i].get(compiler));
        }

        return SIRModuleCreateStructType(
            module, field_types.ptr, field_types.len, false);
    }
    case TypeKind_Struct: {
        Slice<SIRType *> field_types =
            compiler->arena->alloc<SIRType *>(type.struct_.field_types.len);

        for (size_t i = 0; i < type.struct_.field_types.len; ++i) {
            field_types[i] = get_ir_type(
                compiler, module, type.struct_.field_types[i].get(compiler));
        }

        return SIRModuleCreateStructType(
            module, field_types.ptr, field_types.len, false);
    }
    }

    return NULL;
}

static CodegenValue
codegen_expr(Compiler *compiler, CodegenContext *ctx, ExprRef expr_ref)
{
    ZoneScoped;

    Expr expr = expr_ref.get(compiler);
    CodegenValue value = {};

    switch (expr.kind) {
    case ExprKind_Unknown: LANG_ASSERT(0); break;

    case ExprKind_VoidLiteral:
    case ExprKind_VoidType:
    case ExprKind_PointerType:
    case ExprKind_DistinctType:
    case ExprKind_BoolType:
    case ExprKind_FloatType:
    case ExprKind_IntType:
    case ExprKind_ISizeType:
    case ExprKind_USizeType:
    case ExprKind_SliceType:
    case ExprKind_ArrayType:
    case ExprKind_StructType: break;

    case ExprKind_BoolLiteral: {
        value = {
            false,
            SIRBuilderInsertImmBool(ctx->builder, expr.bool_literal.bool_)};
        break;
    }

    case ExprKind_IntLiteral: {
        TypeRef type_ref = compiler->expr_types[expr_ref];
        Type type = type_ref.get(compiler);

        switch (type.kind) {
        case TypeKind_Int: {
            value = {
                false,
                SIRBuilderInsertImmInt(
                    ctx->builder,
                    ctx->type_values[type_ref.id],
                    expr.int_literal.u64),
            };
            break;
        }
        case TypeKind_Float: {
            value = {
                false,
                SIRBuilderInsertImmFloat(
                    ctx->builder,
                    ctx->type_values[type_ref.id],
                    (double)expr.int_literal.u64),
            };
            break;
        }
        default: LANG_ASSERT(0);
        }

        break;
    }

    case ExprKind_FloatLiteral: {
        LANG_ASSERT(
            compiler->expr_types[expr_ref].get(compiler).kind ==
            TypeKind_Float);

        value = {
            false,
            SIRBuilderInsertImmFloat(
                ctx->builder,
                ctx->type_values[compiler->expr_types[expr_ref].id],
                (double)expr.float_literal.f64)};

        break;
    }

    case ExprKind_NullLiteral: {
        LANG_ASSERT(!"unimplemented");
        break;
    }

    case ExprKind_StringLiteral: {
        Type type = compiler->expr_types[expr_ref].get(compiler);

        switch (type.kind) {
        case TypeKind_Pointer: {
            LANG_ASSERT(type.pointer.sub_type.id == compiler->u8_type.id);

            value = {
                false,
                SIRModuleAddGlobalString(
                    ctx->module,
                    expr.str_literal.str.ptr,
                    expr.str_literal.str.len)};

            break;
        }

        case TypeKind_Slice: {
            LANG_ASSERT(!"unimplemented");
            break;
        }

        default: LANG_ASSERT(0); break;
        }

        break;
    }

    case ExprKind_Identifier: {
        LANG_ASSERT(expr.ident.decl_ref.id > 0);

        Decl decl = expr.ident.decl_ref.get(compiler);
        switch (decl.kind) {
        case DeclKind_Function:
        case DeclKind_FunctionParameter: {
            value = ctx->decl_values[expr.ident.decl_ref.id];
            break;
        }

        case DeclKind_GlobalVarDecl: {
            value = ctx->decl_values[expr.ident.decl_ref.id];
            break;
        }

        case DeclKind_LocalVarDecl: {
            value = ctx->decl_values[expr.ident.decl_ref.id];
            break;
        }

        default: LANG_ASSERT(0); break;
        }

        break;
    }

    case ExprKind_Function: {
        LANG_ASSERT(!"unimplemented");
        break;
    }

    case ExprKind_FunctionCall: {
        ExprRef func_expr_ref = expr.func_call.func_expr_ref;
        Type func_type = compiler->expr_types[func_expr_ref].get(compiler);

        switch (func_type.kind) {
        case TypeKind_Function: {
            // Actual function call

            Slice<SIRInstRef> params = compiler->arena->alloc<SIRInstRef>(
                expr.func_call.param_refs.len);

            for (size_t i = 0; i < expr.func_call.param_refs.len; ++i) {
                CodegenValue param_value =
                    codegen_expr(compiler, ctx, expr.func_call.param_refs[i]);

                params[i] = load_lvalue(ctx, param_value);
            }

            CodegenValue func_value =
                codegen_expr(compiler, ctx, expr.func_call.func_expr_ref);

            SIRInstKind func_inst_kind =
                SIRModuleGetInstKind(ctx->module, func_value.inst_ref);
            switch (func_inst_kind) {
            case SIRInstKind_Function: {
                value = {
                    false,
                    SIRBuilderInsertFuncCall(
                        ctx->builder,
                        load_lvalue(ctx, func_value),
                        params.ptr,
                        params.len)};
                break;
            }

            default: LANG_ASSERT(0); break;
            }

            break;
        }

        case TypeKind_Type: {
            // Type cast

            LANG_ASSERT(expr.func_call.param_refs.len == 1);

            ExprRef param_expr_ref = expr.func_call.param_refs[0];

            Type dest_type = compiler->expr_types[expr_ref].inner(compiler).get(compiler);
            Type source_type =
                compiler->expr_types[param_expr_ref].inner(compiler).get(compiler);

            SIRType *dest_type_ir =
                ctx->type_values[compiler->expr_types[expr_ref].id];

            SIRInstRef source_value = load_lvalue(
                ctx, codegen_expr(compiler, ctx, expr.func_call.param_refs[0]));

            if (compiler->expr_types[param_expr_ref].id ==
                compiler->expr_types[expr_ref].id) {
                value = {false, source_value};
            } else if (
                dest_type.is_runtime_int() && source_type.is_runtime_int()) {

                if (dest_type.int_.bits < source_type.int_.bits) {
                    value = {
                        false,
                        SIRBuilderInsertTrunc(
                            ctx->builder, dest_type_ir, source_value)};
                } else if (dest_type.int_.bits > source_type.int_.bits) {
                    if (source_type.int_.is_signed) {
                        value = {
                            false,
                            SIRBuilderInsertSext(
                                ctx->builder, dest_type_ir, source_value)};
                    } else {
                        value = {
                            false,
                            SIRBuilderInsertZext(
                                ctx->builder, dest_type_ir, source_value)};
                    }
                } else {
                    value = {false, source_value};
                }
            } else {
                LANG_ASSERT(!"type cast unimplemented");
            }
            break;
        }

        default: LANG_ASSERT(0); break;
        }

        break;
    }

    case ExprKind_BuiltinCall: {
        switch (expr.builtin_call.builtin) {
        case BuiltinFunction_Unknown: LANG_ASSERT(0); break;
        case BuiltinFunction_Sizeof: {
            ExprRef param0_ref = expr.builtin_call.param_refs[0];
            uint64_t size =
                compiler->expr_as_types[param0_ref].get(compiler).size_of(
                    compiler);

            value = {
                false,
                SIRBuilderInsertImmInt(
                    ctx->builder,
                    ctx->type_values[compiler->expr_types[expr_ref].id],
                    size)};

            break;
        }
        case BuiltinFunction_Alignof: {
            ExprRef param0_ref = expr.builtin_call.param_refs[0];
            uint64_t size =
                compiler->expr_as_types[param0_ref].get(compiler).align_of(
                    compiler);

            value = {
                false,
                SIRBuilderInsertImmInt(
                    ctx->builder,
                    ctx->type_values[compiler->expr_types[expr_ref].id],
                    size)};

            break;
        }
        case BuiltinFunction_PtrCast: {
            ExprRef param0_ref = expr.builtin_call.param_refs[0];

            CodegenValue ptr_value =
                codegen_expr(compiler, ctx, expr.builtin_call.param_refs[1]);

            value = {
                false,
                SIRBuilderInsertPtrCast(
                    ctx->builder,
                    ctx->type_values[compiler->expr_as_types[param0_ref]],
                    load_lvalue(ctx, ptr_value))};

            break;
        }
        }

        break;
    }

    case ExprKind_Subscript: {
        CodegenValue indexed_ref =
            codegen_expr(compiler, ctx, expr.subscript.left_ref);
        CodegenValue index_ref =
            codegen_expr(compiler, ctx, expr.subscript.right_ref);

        if (indexed_ref.is_lvalue) {
            value = {
                true,
                SIRBuilderInsertArrayElemPtr(
                    ctx->builder,
                    indexed_ref.inst_ref,
                    load_lvalue(ctx, index_ref))};
        } else {
            value = {
                false,
                SIRBuilderInsertExtractArrayElem(
                    ctx->builder,
                    indexed_ref.inst_ref,
                    load_lvalue(ctx, index_ref))};
        }

        break;
    }

    case ExprKind_Access: {
        Type accessed_type =
            compiler->expr_types[expr.access.left_ref].get(compiler);
        Expr ident_expr = expr.access.accessed_ident_ref.get(compiler);
        LANG_ASSERT(ident_expr.kind == ExprKind_Identifier);
        String accessed_field = ident_expr.ident.str;

        switch (accessed_type.kind) {
        case TypeKind_Struct: {
            CodegenValue accessed_ref =
                codegen_expr(compiler, ctx, expr.access.left_ref);

            uint32_t field_index = 0;
            if (!accessed_type.struct_.field_map.get(
                    accessed_field, &field_index)) {
                LANG_ASSERT(0);
            }

            if (accessed_ref.is_lvalue) {
                value = {
                    true,
                    SIRBuilderInsertStructElemPtr(
                        ctx->builder, accessed_ref.inst_ref, field_index)};
            } else {
                value = {
                    false,
                    SIRBuilderInsertExtractStructElem(
                        ctx->builder, accessed_ref.inst_ref, field_index)};
            }

            TypeRef field_type = accessed_type.struct_.field_types[field_index];
            compiler->expr_types[expr_ref] = field_type;

            break;
        }
        default: {
            LANG_ASSERT(0);
            break;
        }
        }

        break;
    }

    case ExprKind_Unary: {

        switch (expr.unary.op) {
        case UnaryOp_Unknown: LANG_ASSERT(0); break;
        case UnaryOp_AddressOf: {
            CodegenValue operand_value =
                codegen_expr(compiler, ctx, expr.unary.left_ref);

            if (!operand_value.is_lvalue) {
                SIRInstRef func_ref = *ctx->function_stack.last();

                ExprRef left_expr_ref = expr.unary.left_ref;
                SIRType *ir_type =
                    ctx->type_values[compiler->expr_types[left_expr_ref]];

                value = {
                    false,
                    SIRModuleAddStackSlot(ctx->module, func_ref, ir_type)};
            } else {
                value = {false, operand_value.inst_ref};
            }

            break;
        }
        case UnaryOp_Dereference: {
            CodegenValue operand_value =
                codegen_expr(compiler, ctx, expr.unary.left_ref);
            LANG_ASSERT(operand_value.is_lvalue);

            value = {
                true,
                SIRBuilderInsertLoad(ctx->builder, operand_value.inst_ref)};

            break;
        }
        case UnaryOp_Negate: {
            LANG_ASSERT(!"unimplemented");
            break;
        }
        case UnaryOp_Not: {
            LANG_ASSERT(!"unimplemented");
            break;
        }
        }

        break;
    }

    case ExprKind_Binary: {
        CodegenValue left_val =
            codegen_expr(compiler, ctx, expr.binary.left_ref);
        CodegenValue right_val =
            codegen_expr(compiler, ctx, expr.binary.right_ref);

        SIRInstRef left_inst = load_lvalue(ctx, left_val);
        SIRInstRef right_inst = load_lvalue(ctx, right_val);

        SIRBinaryOperation op = {};

        ExprRef left_ref = expr.binary.left_ref;
        Type operand_type = compiler->expr_types[left_ref].inner(compiler).get(compiler);

        switch (expr.binary.op) {
        case BinaryOp_Unknown:
        case BinaryOp_MAX: LANG_ASSERT(0); break;

        case BinaryOp_Add: {
            switch (operand_type.kind) {
            case TypeKind_Int: op = SIRBinaryOperation_IAdd; break;
            case TypeKind_Float: op = SIRBinaryOperation_FAdd; break;
            default: LANG_ASSERT(0);
            }
            break;
        }

        case BinaryOp_Sub: {
            switch (operand_type.kind) {
            case TypeKind_Int: op = SIRBinaryOperation_ISub; break;
            case TypeKind_Float: op = SIRBinaryOperation_FSub; break;
            default: LANG_ASSERT(0);
            }
            break;
        }

        case BinaryOp_Mul: {
            switch (operand_type.kind) {
            case TypeKind_Int: op = SIRBinaryOperation_IMul; break;
            case TypeKind_Float: op = SIRBinaryOperation_FMul; break;
            default: LANG_ASSERT(0);
            }
            break;
        }

        case BinaryOp_Div: {
            switch (operand_type.kind) {
            case TypeKind_Int:
                op = (operand_type.int_.is_signed) ? SIRBinaryOperation_SDiv
                                                   : SIRBinaryOperation_UDiv;
                break;
            case TypeKind_Float: op = SIRBinaryOperation_FDiv; break;
            default: LANG_ASSERT(0);
            }
            break;
        }
        case BinaryOp_Mod: {
            switch (operand_type.kind) {
            case TypeKind_Int:
                op = (operand_type.int_.is_signed) ? SIRBinaryOperation_SRem
                                                   : SIRBinaryOperation_URem;
                break;
            case TypeKind_Float: op = SIRBinaryOperation_FRem; break;
            default: LANG_ASSERT(0);
            }
            break;
        }

        case BinaryOp_Equal: {
            switch (operand_type.kind) {
            case TypeKind_Int: op = SIRBinaryOperation_IEQ; break;
            case TypeKind_Float: op = SIRBinaryOperation_FEQ; break;
            default: LANG_ASSERT(0);
            }
            break;
        }

        case BinaryOp_NotEqual: {
            switch (operand_type.kind) {
            case TypeKind_Int: op = SIRBinaryOperation_INE; break;
            case TypeKind_Float: op = SIRBinaryOperation_FNE; break;
            default: LANG_ASSERT(0);
            }
            break;
        }

        case BinaryOp_Greater: {
            switch (operand_type.kind) {
            case TypeKind_Int:
                op = (operand_type.int_.is_signed) ? SIRBinaryOperation_SGT
                                                   : SIRBinaryOperation_UGT;
                break;
            case TypeKind_Float: op = SIRBinaryOperation_FGT; break;
            default: LANG_ASSERT(0);
            }
            break;
        }

        case BinaryOp_GreaterEqual: {
            switch (operand_type.kind) {
            case TypeKind_Int:
                op = (operand_type.int_.is_signed) ? SIRBinaryOperation_SGE
                                                   : SIRBinaryOperation_UGE;
                break;
            case TypeKind_Float: op = SIRBinaryOperation_FGE; break;
            default: LANG_ASSERT(0);
            }
            break;
        }

        case BinaryOp_Less: {
            switch (operand_type.kind) {
            case TypeKind_Int:
                op = (operand_type.int_.is_signed) ? SIRBinaryOperation_SLT
                                                   : SIRBinaryOperation_ULT;
                break;
            case TypeKind_Float: op = SIRBinaryOperation_FLT; break;
            default: LANG_ASSERT(0);
            }
            break;
        }

        case BinaryOp_LessEqual: {
            switch (operand_type.kind) {
            case TypeKind_Int:
                op = (operand_type.int_.is_signed) ? SIRBinaryOperation_SLE
                                                   : SIRBinaryOperation_ULE;
                break;
            case TypeKind_Float: op = SIRBinaryOperation_FLE; break;
            default: LANG_ASSERT(0);
            }
            break;
        }

        case BinaryOp_LShift: {
            LANG_ASSERT(operand_type.is_runtime_int());
            op = SIRBinaryOperation_Shl;
            break;
        }

        case BinaryOp_RShift: {
            LANG_ASSERT(operand_type.is_runtime_int());
            op = (operand_type.int_.is_signed) ? SIRBinaryOperation_AShr
                                               : SIRBinaryOperation_LShr;
            break;
        }

        case BinaryOp_BitAnd: {
            LANG_ASSERT(operand_type.is_runtime_int());
            op = SIRBinaryOperation_And;
            break;
        }

        case BinaryOp_BitOr: {
            LANG_ASSERT(operand_type.is_runtime_int());
            op = SIRBinaryOperation_Or;
            break;
        }

        case BinaryOp_BitXor: {
            LANG_ASSERT(operand_type.is_runtime_int());
            op = SIRBinaryOperation_Xor;
            break;
        }
        }

        value = {
            false,
            SIRBuilderInsertBinop(ctx->builder, op, left_inst, right_inst)};

        break;
    }
    }

    ctx->expr_values[expr_ref.id] = value;
    return value;
}

static void
codegen_stmt(Compiler *compiler, CodegenContext *ctx, StmtRef stmt_ref)
{
    ZoneScoped;

    Stmt stmt = stmt_ref.get(compiler);
    switch (stmt.kind) {
    case StmtKind_Unknown: LANG_ASSERT(0); break;

    case StmtKind_Expr: {
        codegen_expr(compiler, ctx, stmt.expr.expr_ref);
        break;
    }

    case StmtKind_Decl: {
        codegen_decl(compiler, ctx, stmt.decl.decl_ref);
        break;
    }

    case StmtKind_Assign: {
        CodegenValue receiver_value =
            codegen_expr(compiler, ctx, stmt.assign.assigned_expr_ref);
        CodegenValue value =
            codegen_expr(compiler, ctx, stmt.assign.value_expr_ref);

        LANG_ASSERT(receiver_value.is_lvalue);

        SIRBuilderInsertStore(
            ctx->builder, receiver_value.inst_ref, load_lvalue(ctx, value));
        break;
    }

    case StmtKind_Block: {
        for (StmtRef sub_stmt_ref : stmt.block.stmt_refs) {
            codegen_stmt(compiler, ctx, sub_stmt_ref);
        }
        break;
    }

    case StmtKind_Return: {
        if (stmt.return_.returned_expr_ref.id > 0) {
            SIRInstRef returned_value = load_lvalue(
                ctx,
                codegen_expr(compiler, ctx, stmt.return_.returned_expr_ref));
            SIRBuilderInsertReturnValue(ctx->builder, returned_value);
        } else {
            SIRBuilderInsertReturnVoid(ctx->builder);
        }
        break;
    }

    case StmtKind_If: {
        SIRInstRef cond_value = load_lvalue(
            ctx, codegen_expr(compiler, ctx, stmt.if_.cond_expr_ref));
        cond_value = value_into_bool(ctx, cond_value);

        SIRInstRef current_func = SIRBuilderGetCurrentFunction(ctx->builder);

        if (stmt.if_.false_stmt_ref.id == 0) {
            SIRInstRef true_block =
                SIRModuleInsertBlockAtEnd(ctx->module, current_func);
            SIRInstRef merge_block =
                SIRModuleInsertBlockAtEnd(ctx->module, current_func);

            SIRBuilderInsertBranch(
                ctx->builder, cond_value, true_block, merge_block);

            SIRBuilderPositionAtEnd(ctx->builder, true_block);
            codegen_stmt(compiler, ctx, stmt.if_.true_stmt_ref);
            SIRBuilderInsertJump(ctx->builder, merge_block);

            SIRBuilderPositionAtEnd(ctx->builder, merge_block);
        } else {
            SIRInstRef true_block =
                SIRModuleInsertBlockAtEnd(ctx->module, current_func);
            SIRInstRef false_block =
                SIRModuleInsertBlockAtEnd(ctx->module, current_func);
            SIRInstRef merge_block =
                SIRModuleInsertBlockAtEnd(ctx->module, current_func);

            SIRBuilderInsertBranch(
                ctx->builder, cond_value, true_block, false_block);

            SIRBuilderPositionAtEnd(ctx->builder, true_block);
            codegen_stmt(compiler, ctx, stmt.if_.true_stmt_ref);
            SIRBuilderInsertJump(ctx->builder, merge_block);

            SIRBuilderPositionAtEnd(ctx->builder, false_block);
            codegen_stmt(compiler, ctx, stmt.if_.false_stmt_ref);
            SIRBuilderInsertJump(ctx->builder, merge_block);

            SIRBuilderPositionAtEnd(ctx->builder, merge_block);
        }

        break;
    }

    case StmtKind_While: {
        SIRInstRef current_func = SIRBuilderGetCurrentFunction(ctx->builder);

        SIRInstRef cond_block =
            SIRModuleInsertBlockAtEnd(ctx->module, current_func);
        SIRInstRef true_block =
            SIRModuleInsertBlockAtEnd(ctx->module, current_func);
        SIRInstRef merge_block =
            SIRModuleInsertBlockAtEnd(ctx->module, current_func);

        SIRBuilderInsertJump(ctx->builder, cond_block);
        SIRBuilderPositionAtEnd(ctx->builder, cond_block);

        SIRInstRef cond_value = load_lvalue(
            ctx, codegen_expr(compiler, ctx, stmt.while_.cond_expr_ref));

        SIRBuilderInsertBranch(
            ctx->builder, cond_value, true_block, merge_block);

        SIRBuilderPositionAtEnd(ctx->builder, true_block);
        codegen_stmt(compiler, ctx, stmt.while_.true_stmt_ref);
        SIRBuilderInsertJump(ctx->builder, cond_block);

        SIRBuilderPositionAtEnd(ctx->builder, merge_block);

        break;
    }
    }
}

static void
codegen_decl(Compiler *compiler, CodegenContext *ctx, DeclRef decl_ref)
{
    ZoneScoped;

    SIRModule *module = ctx->module;
    Decl decl = decl_ref.get(compiler);
    CodegenValue value = {};

    switch (decl.kind) {
    case DeclKind_Unknown: LANG_ASSERT(0); break;

    case DeclKind_Type: {
        break;
    }

    case DeclKind_ConstDecl: {
        LANG_ASSERT(!"unimplemented");
        break;
    }

    case DeclKind_Function: {
        String decl_name = compiler->decl_names[decl_ref];
        TypeRef func_type_ref = compiler->decl_types[decl_ref];
        Type func_type = func_type_ref.get(compiler);

        Slice<SIRType *> param_types =
            compiler->arena->alloc<SIRType *>(func_type.func.param_types.len);
        for (size_t i = 0; i < func_type.func.param_types.len; ++i) {
            param_types[i] = ctx->type_values[func_type.func.param_types[i].id];
        }

        SIRType *return_type = ctx->type_values[func_type.func.return_type.id];

        SIRLinkage linkage = SIRLinkage_Internal;
        if ((decl.func->flags & FunctionFlags_Exported) ||
            (decl.func->flags & FunctionFlags_Extern)) {
            linkage = SIRLinkage_External;
        }

        value = {
            false,
            SIRModuleAddFunction(
                module,
                decl_name.ptr,
                decl_name.len,
                SIRCallingConvention_SystemV,
                linkage,
                func_type.func.vararg,
                param_types.ptr,
                param_types.len,
                return_type)};

        ctx->decl_values[decl_ref.id] = value;

        ctx->function_stack.push_back(value.inst_ref);

        for (size_t i = 0; i < decl.func->param_decl_refs.len; ++i) {
            DeclRef param_decl_ref = decl.func->param_decl_refs[i];

            SIRInstRef param_value = {};
            param_value = SIRModuleGetFuncParam(module, value.inst_ref, i);
            ctx->decl_values[param_decl_ref.id] = {false, param_value};
        }

        if (!(decl.func->flags & FunctionFlags_Extern)) {
            SIRBuilderSetFunction(ctx->builder, value.inst_ref);

            auto block = SIRModuleInsertBlockAtEnd(module, value.inst_ref);
            SIRBuilderPositionAtEnd(ctx->builder, block);

            for (StmtRef stmt_ref : decl.func->body_stmts) {
                codegen_stmt(compiler, ctx, stmt_ref);
            }

            SIRInstRef last_block = SIRBuilderGetCurrentBlock(ctx->builder);

            uint32_t last_block_inst_count =
                SIRModuleGetBlockInstructionCount(module, last_block);
            SIRInstRef last_inst = SIRModuleGetBlockInstruction(
                module, last_block, last_block_inst_count - 1);
            SIRInstKind last_inst_kind =
                SIRModuleGetInstKind(module, last_inst);

            SIRTypeKind return_type_kind =
                SIRModuleGetTypeKind(module, return_type);

            if (return_type_kind == SIRTypeKind_Void) {
                if (last_block_inst_count == 0 ||
                    last_inst_kind != SIRInstKind_ReturnVoid) {
                    SIRBuilderInsertReturnVoid(ctx->builder);
                }
            } else {
                if (last_block_inst_count == 0 ||
                    last_inst_kind != SIRInstKind_ReturnValue) {
                    compiler->add_error(
                        compiler->decl_locs[decl_ref],
                        "no return statement for '%.*s'",
                        (int)decl_name.len,
                        decl_name.ptr);
                }
            }
        }

        ctx->function_stack.pop();

        break;
    }

    case DeclKind_FunctionParameter: {
        // No need to implement this
        LANG_ASSERT(0);
        break;
    }

    case DeclKind_LocalVarDecl: {
        SIRInstRef func_ref = *ctx->function_stack.last();
        SIRType *ir_type = ctx->type_values[compiler->decl_types[decl_ref]];
        LANG_ASSERT(ir_type);

        value = {true, SIRModuleAddStackSlot(module, func_ref, ir_type)};

        ctx->decl_values[decl_ref.id] = value;

        if (decl.local_var_decl.value_expr.id) {
            CodegenValue assigned_value =
                codegen_expr(compiler, ctx, decl.local_var_decl.value_expr);

            SIRBuilderInsertStore(
                ctx->builder, value.inst_ref, load_lvalue(ctx, assigned_value));
        }

        break;
    }

    case DeclKind_GlobalVarDecl: {
        SIRType *ir_type = ctx->type_values[compiler->decl_types[decl_ref]];

        Slice<uint8_t> global_data = compiler->arena->alloc_init<uint8_t>(
            SIRTypeSizeOf(ctx->module, ir_type));

        value = {
            true,
            SIRModuleAddGlobal(
                module,
                ir_type,
                SIRGlobalFlags_Initialized,
                global_data.ptr,
                global_data.len)};

        ctx->decl_values[decl_ref.id] = value;

        if (decl.local_var_decl.value_expr.id) {
            LANG_ASSERT(!"unimplemented");
        }

        break;
    }
    }
}

void codegen_file(Compiler *compiler, FileRef file_ref)
{
    ZoneScoped;

    CodegenContext ctx = {};

    File file = compiler->files[file_ref.id];

    ctx.file_ref = file_ref;
    ctx.module =
        SIRModuleCreate(SIRTargetArch_X86_64, SIREndianness_LittleEndian);
    ctx.builder = SIRBuilderCreate(ctx.module);

    ctx.type_values = Array<SIRType *>::create(MallocAllocator::get_instance());
    ctx.type_values.resize(compiler->types.len);
    for (size_t i = 0; i < compiler->types.len; ++i) {
        ctx.type_values[i] =
            get_ir_type(compiler, ctx.module, compiler->types[i]);
    }

    ctx.decl_values =
        Array<CodegenValue>::create(MallocAllocator::get_instance());
    ctx.decl_values.resize(compiler->decls.len);
    for (size_t i = 0; i < compiler->decls.len; ++i) {
        ctx.decl_values[i] = {};
    }

    ctx.expr_values =
        Array<CodegenValue>::create(MallocAllocator::get_instance());
    ctx.expr_values.resize(compiler->exprs.len);
    for (size_t i = 0; i < compiler->exprs.len; ++i) {
        ctx.expr_values[i] = {};
    }

    ctx.function_stack = Array<SIRInstRef>::create(compiler->arena);

    for (DeclRef decl_ref : file.top_level_decls) {
        codegen_decl(compiler, &ctx, decl_ref);
    }

    if (compiler->errors.len > 0) {
        compiler->halt_compilation();
    }

#if !NDEBUG
    {
        size_t str_len = 0;
        char *str = SIRModulePrintToString(ctx.module, &str_len);
        printf("%.*s", (int)str_len, str);
        free(str);
    }
#endif

    SIRObjectBuilder *obj_builder = SIRCreateELF64Builder(ctx.module);
    SIRAsmBuilder *asm_builder = SIRCreateX64Builder(ctx.module, obj_builder);

    SIRAsmBuilderGenerate(asm_builder);

    const char *path = "./main.o";
    SIRObjectBuilderOutputToFile(obj_builder, path, strlen(path));

    SIRAsmBuilderDestroy(asm_builder);
    SIRObjectBuilderDestroy(obj_builder);

    ctx.type_values.destroy();
    ctx.decl_values.destroy();
    ctx.expr_values.destroy();

    SIRModuleDestroy(ctx.module);
}
