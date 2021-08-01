#include "compiler.hpp"
#include <Tracy.hpp>

#include <sir_ir.hpp>
#include <sir_obj.hpp>

struct CodegenContext;

struct CodegenValue {
    bool is_lvalue;
    SIRInstRef inst_ref;
};

struct CodegenContext {
    FileRef file_ref;
    SIRModule *module;
    SIRBuilder *builder;
    SIRArray<SIRType *> type_values;
    SIRArray<CodegenValue> expr_values;
    SIRArray<CodegenValue> decl_values;

    SIRArray<SIRInstRef> function_stack;
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
    SIRType *type = SIRModuleGetInst(ctx->module, inst_ref).type;
    if (type->kind == SIRTypeKind_Int) {
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
    case TypeKind_Function:
    case TypeKind_UntypedInt:
    case TypeKind_UntypedFloat:
    case TypeKind_Type: return nullptr;

    case TypeKind_Void: {
        return module->void_type;
    }
    case TypeKind_Bool: {
        return module->i8_type;
    }
    case TypeKind_Int: {
        switch (type.int_.bits) {
        case 8: return module->i8_type;
        case 16: return module->i16_type;
        case 32: return module->i32_type;
        case 64: return module->i64_type;
        }
        break;
    }
    case TypeKind_Float: {
        switch (type.float_.bits) {
        case 32: return module->f32_type;
        case 64: return module->f64_type;
        }
        break;
    }
    case TypeKind_Pointer: {
        SIRType *subtype =
            get_ir_type(compiler, module, type.pointer.sub_type.get(compiler));
        return SIRModuleCreatePointerType(module, subtype);
    }
    case TypeKind_Slice: {
        return SIRModuleCreateStructType(
            module,
            {
                get_ir_type(
                    compiler,
                    module,
                    compiler->create_pointer_type(type.slice.sub_type)
                        .get(compiler)),
                get_ir_type(compiler, module, compiler->u64_type.get(compiler)),
            },
            false);
    }
    case TypeKind_Array: {
        SIRType *subtype =
            get_ir_type(compiler, module, type.array.sub_type.get(compiler));
        return SIRModuleCreateArrayType(module, subtype, type.array.size);
    }
    case TypeKind_Tuple: {
        SIRSlice<SIRType *> field_types =
            compiler->arena->alloc<SIRType *>(type.tuple.field_types.len);

        for (size_t i = 0; i < type.tuple.field_types.len; ++i) {
            field_types[i] = get_ir_type(
                compiler, module, type.tuple.field_types[i].get(compiler));
        }

        return SIRModuleCreateStructType(module, field_types, false);
    }
    case TypeKind_Struct: {
        SIRSlice<SIRType *> field_types =
            compiler->arena->alloc<SIRType *>(type.struct_.field_types.len);

        for (size_t i = 0; i < type.struct_.field_types.len; ++i) {
            field_types[i] = get_ir_type(
                compiler, module, type.struct_.field_types[i].get(compiler));
        }

        return SIRModuleCreateStructType(module, field_types, false);
    }
    }

    return nullptr;
}

static CodegenValue
codegen_expr(Compiler *compiler, CodegenContext *ctx, ExprRef expr_ref)
{
    ZoneScoped;

    Expr expr = expr_ref.get(compiler);
    CodegenValue value = {};

    switch (expr.kind) {
    case ExprKind_Unknown: SIR_ASSERT(0); break;

    case ExprKind_VoidLiteral:
    case ExprKind_VoidType:
    case ExprKind_PointerType:
    case ExprKind_BoolType:
    case ExprKind_FloatType:
    case ExprKind_IntType:
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
        Type type = expr.expr_type_ref.get(compiler);

        switch (type.kind) {
        case TypeKind_Int: {
            value = {
                false,
                SIRBuilderInsertImmInt(
                    ctx->builder,
                    ctx->type_values[expr.expr_type_ref.id],
                    expr.int_literal.i64),
            };
            break;
        }
        case TypeKind_Float: {
            value = {
                false,
                SIRBuilderInsertImmFloat(
                    ctx->builder,
                    ctx->type_values[expr.expr_type_ref.id],
                    (double)expr.int_literal.i64),
            };
            break;
        }
        default: SIR_ASSERT(0);
        }

        break;
    }

    case ExprKind_FloatLiteral: {
        SIR_ASSERT(expr.expr_type_ref.get(compiler).kind == TypeKind_Float);

        value = {
            false,
            SIRBuilderInsertImmFloat(
                ctx->builder,
                ctx->type_values[expr.expr_type_ref.id],
                (double)expr.float_literal.f64)};

        break;
    }

    case ExprKind_NullLiteral: {
        SIR_ASSERT(!"unimplemented");
        break;
    }

    case ExprKind_StringLiteral: {
        Type type = expr.expr_type_ref.get(compiler);

        switch (type.kind) {
        case TypeKind_Pointer: {
            SIR_ASSERT(type.pointer.sub_type.id == compiler->u8_type.id);

            value = {
                false,
                SIRModuleAddGlobalString(ctx->module, expr.str_literal.str)};

            break;
        }

        case TypeKind_Slice: {
            SIR_ASSERT(!"unimplemented");
            break;
        }

        default: SIR_ASSERT(0); break;
        }

        break;
    }

    case ExprKind_Identifier: {
        SIR_ASSERT(expr.ident.decl_ref.id > 0);

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

        default: SIR_ASSERT(0); break;
        }

        break;
    }

    case ExprKind_Function: {
        SIR_ASSERT(!"unimplemented");
        break;
    }

    case ExprKind_FunctionCall: {
        Expr func_expr = expr.func_call.func_expr_ref.get(compiler);
        Type func_type = func_expr.expr_type_ref.get(compiler);

        switch (func_type.kind) {
        case TypeKind_Function: {
            // Actual function call

            SIRSlice<SIRInstRef> params = compiler->arena->alloc<SIRInstRef>(
                expr.func_call.param_refs.len);

            for (size_t i = 0; i < expr.func_call.param_refs.len; ++i) {
                CodegenValue param_value =
                    codegen_expr(compiler, ctx, expr.func_call.param_refs[i]);

                params[i] = load_lvalue(ctx, param_value);
            }

            CodegenValue func_value =
                codegen_expr(compiler, ctx, expr.func_call.func_expr_ref);

            SIRInst func_inst =
                SIRModuleGetInst(ctx->module, func_value.inst_ref);
            switch (func_inst.kind) {
            case SIRInstKind_Function: {
                value = {
                    false,
                    SIRBuilderInsertFuncCall(
                        ctx->builder, load_lvalue(ctx, func_value), params)};
                break;
            }

            default: SIR_ASSERT(0); break;
            }

            break;
        }

        case TypeKind_Type: {
            // Type cast

            SIR_ASSERT(expr.func_call.param_refs.len == 1);

            Expr param_expr = expr.func_call.param_refs[0].get(compiler);

            Type dest_type = expr.expr_type_ref.get(compiler);
            Type source_type = param_expr.expr_type_ref.get(compiler);

            SIRInstRef source_value = load_lvalue(
                ctx, codegen_expr(compiler, ctx, expr.func_call.param_refs[0]));

            if (param_expr.expr_type_ref.id == expr.expr_type_ref.id) {
                value = {false, source_value};
            } else if (
                dest_type.kind == TypeKind_Int &&
                source_type.kind == TypeKind_Int) {

                if (dest_type.int_.bits < source_type.int_.bits) {
                    value = {
                        false,
                        SIRBuilderInsertTrunc(
                            ctx->builder,
                            get_ir_type(compiler, ctx->module, dest_type),
                            source_value)};
                } else if (dest_type.int_.bits > source_type.int_.bits) {
                    if (source_type.int_.is_signed) {
                        value = {
                            false,
                            SIRBuilderInsertSext(
                                ctx->builder,
                                get_ir_type(compiler, ctx->module, dest_type),
                                source_value)};
                    } else {
                        value = {
                            false,
                            SIRBuilderInsertZext(
                                ctx->builder,
                                get_ir_type(compiler, ctx->module, dest_type),
                                source_value)};
                    }
                } else {
                    value = {false, source_value};
                }
            } else {
                SIR_ASSERT(!"type cast unimplemented");
            }
            break;
        }

        default: SIR_ASSERT(0); break;
        }

        break;
    }

    case ExprKind_BuiltinCall: {
        switch (expr.builtin_call.builtin) {
        case BuiltinFunction_Unknown: SIR_ASSERT(0); break;
        case BuiltinFunction_Sizeof: {
            Expr param0 = expr.builtin_call.param_refs[0].get(compiler);
            uint64_t size = param0.as_type_ref.get(compiler).size_of(compiler);

            value = {
                false,
                SIRBuilderInsertImmInt(
                    ctx->builder,
                    ctx->type_values[expr.expr_type_ref.id],
                    size)};

            break;
        }
        case BuiltinFunction_Alignof: {
            Expr param0 = expr.builtin_call.param_refs[0].get(compiler);
            uint64_t size = param0.as_type_ref.get(compiler).align_of(compiler);

            value = {
                false,
                SIRBuilderInsertImmInt(
                    ctx->builder,
                    ctx->type_values[expr.expr_type_ref.id],
                    size)};

            break;
        }
        case BuiltinFunction_PtrCast: {
            Expr param0 = expr.builtin_call.param_refs[0].get(compiler);

            CodegenValue ptr_value =
                codegen_expr(compiler, ctx, expr.builtin_call.param_refs[1]);

            value = {
                false,
                SIRBuilderInsertPtrCast(
                    ctx->builder,
                    ctx->type_values[param0.as_type_ref.id],
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
            expr.access.left_ref.get(compiler).expr_type_ref.get(compiler);
        Expr ident_expr = expr.access.accessed_ident_ref.get(compiler);
        SIR_ASSERT(ident_expr.kind == ExprKind_Identifier);
        SIRString accessed_field = ident_expr.ident.str;

        switch (accessed_type.kind) {
        case TypeKind_Struct: {
            CodegenValue accessed_ref =
                codegen_expr(compiler, ctx, expr.access.left_ref);

            uintptr_t field_index = 0;
            if (!SIRStringMapGet(
                    &accessed_type.struct_.field_map,
                    accessed_field,
                    &field_index)) {
                SIR_ASSERT(0);
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
            expr.expr_type_ref = field_type;

            break;
        }
        default: {
            SIR_ASSERT(0);
            break;
        }
        }

        break;
    }

    case ExprKind_Unary: {

        switch (expr.unary.op) {
        case UnaryOp_Unknown: SIR_ASSERT(0); break;
        case UnaryOp_AddressOf: {
            CodegenValue operand_value =
                codegen_expr(compiler, ctx, expr.unary.left_ref);

            if (!operand_value.is_lvalue) {
                SIRInstRef func_ref = *ctx->function_stack.last();

                Expr left_expr = expr.unary.left_ref.get(compiler);
                SIRType *ir_type = ctx->type_values[left_expr.expr_type_ref.id];

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
            SIR_ASSERT(operand_value.is_lvalue);

            value = {
                true,
                SIRBuilderInsertLoad(ctx->builder, operand_value.inst_ref)};

            break;
        }
        case UnaryOp_Negate: {
            SIR_ASSERT(!"unimplemented");
            break;
        }
        case UnaryOp_Not: {
            SIR_ASSERT(!"unimplemented");
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

        Expr left = expr.binary.left_ref.get(compiler);
        Type operand_type = left.expr_type_ref.get(compiler);

        switch (expr.binary.op) {
        case BinaryOp_Unknown:
        case BinaryOp_MAX: SIR_ASSERT(0); break;

        case BinaryOp_Add: {
            switch (operand_type.kind) {
            case TypeKind_Int: op = SIRBinaryOperation_IAdd; break;
            case TypeKind_Float: op = SIRBinaryOperation_FAdd; break;
            default: SIR_ASSERT(0);
            }
            break;
        }

        case BinaryOp_Sub: {
            switch (operand_type.kind) {
            case TypeKind_Int: op = SIRBinaryOperation_ISub; break;
            case TypeKind_Float: op = SIRBinaryOperation_FSub; break;
            default: SIR_ASSERT(0);
            }
            break;
        }

        case BinaryOp_Mul: {
            switch (operand_type.kind) {
            case TypeKind_Int: op = SIRBinaryOperation_IMul; break;
            case TypeKind_Float: op = SIRBinaryOperation_FMul; break;
            default: SIR_ASSERT(0);
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
            default: SIR_ASSERT(0);
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
            default: SIR_ASSERT(0);
            }
            break;
        }

        case BinaryOp_Equal: {
            switch (operand_type.kind) {
            case TypeKind_Int: op = SIRBinaryOperation_IEQ; break;
            case TypeKind_Float: op = SIRBinaryOperation_FEQ; break;
            default: SIR_ASSERT(0);
            }
            break;
        }

        case BinaryOp_NotEqual: {
            switch (operand_type.kind) {
            case TypeKind_Int: op = SIRBinaryOperation_INE; break;
            case TypeKind_Float: op = SIRBinaryOperation_FNE; break;
            default: SIR_ASSERT(0);
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
            default: SIR_ASSERT(0);
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
            default: SIR_ASSERT(0);
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
            default: SIR_ASSERT(0);
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
            default: SIR_ASSERT(0);
            }
            break;
        }

        case BinaryOp_LShift: {
            SIR_ASSERT(operand_type.kind == TypeKind_Int);
            op = SIRBinaryOperation_Shl;
            break;
        }

        case BinaryOp_RShift: {
            SIR_ASSERT(operand_type.kind == TypeKind_Int);
            op = (operand_type.int_.is_signed) ? SIRBinaryOperation_AShr
                                               : SIRBinaryOperation_LShr;
            break;
        }

        case BinaryOp_BitAnd: {
            SIR_ASSERT(operand_type.kind == TypeKind_Int);
            op = SIRBinaryOperation_And;
            break;
        }

        case BinaryOp_BitOr: {
            SIR_ASSERT(operand_type.kind == TypeKind_Int);
            op = SIRBinaryOperation_Or;
            break;
        }

        case BinaryOp_BitXor: {
            SIR_ASSERT(operand_type.kind == TypeKind_Int);
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
    case StmtKind_Unknown: SIR_ASSERT(0); break;

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

        SIR_ASSERT(receiver_value.is_lvalue);

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

        auto current_func = ctx->builder->current_func_ref;

        if (stmt.if_.false_stmt_ref.id == 0) {
            auto true_block =
                SIRModuleInsertBlockAtEnd(ctx->module, current_func);
            auto merge_block =
                SIRModuleInsertBlockAtEnd(ctx->module, current_func);

            SIRBuilderInsertBranch(
                ctx->builder, cond_value, true_block, merge_block);

            SIRBuilderPositionAtEnd(ctx->builder, true_block);
            codegen_stmt(compiler, ctx, stmt.if_.true_stmt_ref);
            SIRBuilderInsertJump(ctx->builder, merge_block);

            SIRBuilderPositionAtEnd(ctx->builder, merge_block);
        } else {
            auto true_block =
                SIRModuleInsertBlockAtEnd(ctx->module, current_func);
            auto false_block =
                SIRModuleInsertBlockAtEnd(ctx->module, current_func);
            auto merge_block =
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
        auto current_func = ctx->builder->current_func_ref;

        auto cond_block = SIRModuleInsertBlockAtEnd(ctx->module, current_func);
        auto true_block = SIRModuleInsertBlockAtEnd(ctx->module, current_func);
        auto merge_block = SIRModuleInsertBlockAtEnd(ctx->module, current_func);

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
    case DeclKind_Unknown: SIR_ASSERT(0); break;

    case DeclKind_Type: {
        break;
    }

    case DeclKind_ConstDecl: {
        SIR_ASSERT(!"unimplemented");
        break;
    }

    case DeclKind_Function: {
        TypeRef func_type_ref = decl.decl_type_ref;
        Type func_type = func_type_ref.get(compiler);

        SIRSlice<SIRType *> param_types =
            compiler->arena->alloc<SIRType *>(func_type.func.param_types.len);
        for (size_t i = 0; i < func_type.func.param_types.len; ++i) {
            param_types[i] = ctx->type_values[func_type.func.param_types[i].id];
        }

        SIRType *return_type = ctx->type_values[func_type.func.return_type.id];

        SIRLinkage linkage = SIRLinkage_Internal;
        if ((decl.func.flags & FunctionFlags_Exported) ||
            (decl.func.flags & FunctionFlags_Extern)) {
            linkage = SIRLinkage_External;
        }

        value = {
            false,
            SIRModuleAddFunction(
                module,
                decl.name,
                SIRCallingConvention_SystemV,
                linkage,
                func_type.func.vararg,
                param_types,
                return_type)};

        ctx->decl_values[decl_ref.id] = value;

        ctx->function_stack.push_back(value.inst_ref);

        for (size_t i = 0; i < decl.func.param_decl_refs.len; ++i) {
            DeclRef param_decl_ref = decl.func.param_decl_refs[i];

            SIRInstRef param_value = {};
            param_value = SIRModuleGetFuncParam(module, value.inst_ref, i);
            ctx->decl_values[param_decl_ref.id] = {false, param_value};
        }

        if (!(decl.func.flags & FunctionFlags_Extern)) {
            SIRBuilderSetFunction(ctx->builder, value.inst_ref);

            auto block = SIRModuleInsertBlockAtEnd(module, value.inst_ref);
            SIRBuilderPositionAtEnd(ctx->builder, block);

            for (StmtRef stmt_ref : decl.func.body_stmts) {
                codegen_stmt(compiler, ctx, stmt_ref);
            }

            SIRInst last_block =
                SIRModuleGetInst(module, ctx->builder->current_block_ref);
            if (return_type->kind == SIRTypeKind_Void) {
                if (last_block.block.inst_refs.len == 0 ||
                    SIRModuleGetInst(module, *last_block.block.inst_refs.last())
                            .kind != SIRInstKind_ReturnVoid) {
                    SIRBuilderInsertReturnVoid(ctx->builder);
                }
            } else {
                if (last_block.block.inst_refs.len == 0 ||
                    SIRModuleGetInst(module, *last_block.block.inst_refs.last())
                            .kind != SIRInstKind_ReturnValue) {
                    compiler->add_error(
                        decl.loc,
                        "no return statement for '%.*s'",
                        (int)decl.name.len,
                        decl.name.ptr);
                }
            }
        }

        ctx->function_stack.pop();

        break;
    }

    case DeclKind_FunctionParameter: {
        // No need to implement this
        SIR_ASSERT(0);
        break;
    }

    case DeclKind_LocalVarDecl: {
        SIRInstRef func_ref = *ctx->function_stack.last();
        SIRType *ir_type = ctx->type_values[decl.decl_type_ref.id];
        SIR_ASSERT(ir_type);

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
        SIRType *ir_type = ctx->type_values[decl.decl_type_ref.id];

        SIRSlice<uint8_t> global_data = compiler->arena->alloc_init<uint8_t>(
            SIRTypeSizeOf(ctx->module, ir_type));

        value = {
            true,
            SIRModuleAddGlobal(
                module, ir_type, SIRGlobalFlags_Initialized, global_data)};

        ctx->decl_values[decl_ref.id] = value;

        if (decl.local_var_decl.value_expr.id) {
            SIR_ASSERT(!"unimplemented");
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

    ctx.type_values =
        SIRArray<SIRType *>::create(SIRMallocAllocator::get_instance());
    ctx.type_values.resize(compiler->types.len);
    for (size_t i = 0; i < compiler->types.len; ++i) {
        ctx.type_values[i] =
            get_ir_type(compiler, ctx.module, compiler->types[i]);
    }

    ctx.decl_values =
        SIRArray<CodegenValue>::create(SIRMallocAllocator::get_instance());
    ctx.decl_values.resize(compiler->decls.len);
    for (size_t i = 0; i < compiler->decls.len; ++i) {
        ctx.decl_values[i] = {};
    }

    ctx.expr_values =
        SIRArray<CodegenValue>::create(SIRMallocAllocator::get_instance());
    ctx.expr_values.resize(compiler->exprs.len);
    for (size_t i = 0; i < compiler->exprs.len; ++i) {
        ctx.expr_values[i] = {};
    }

    ctx.function_stack = SIRArray<SIRInstRef>::create(ctx.module->arena);

    for (DeclRef decl_ref : file.top_level_decls) {
        codegen_decl(compiler, &ctx, decl_ref);
    }

    if (compiler->errors.len > 0) {
        compiler->halt_compilation();
    }

#if !NDEBUG
    {
        auto allocator = SIRMallocAllocator::get_instance();
        SIRString str = SIRModulePrintAlloc(ctx.module, allocator);
        printf("%.*s", (int)str.len, str.ptr);
        allocator->free(str);
    }
#endif

    SIRObjectBuilder *obj_builder = SIRCreateELF64Bbuilder(ctx.module);
    SIRAsmBuilder *asm_builder =
        SIRCreateX86_64Builder(ctx.module, obj_builder);

    asm_builder->generate(asm_builder);

    obj_builder->output_to_file(obj_builder, "./main.o");

    asm_builder->destroy(asm_builder);
    obj_builder->destroy(obj_builder);

    ctx.type_values.destroy();
    ctx.decl_values.destroy();
    ctx.expr_values.destroy();

    SIRModuleDestroy(ctx.module);
}
