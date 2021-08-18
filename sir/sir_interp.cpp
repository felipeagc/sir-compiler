#include "sir_interp.h"
#include "sir_base.hpp"
#include "sir_ir.hpp"
#include <math.h>

struct SIRInterpContext {
    SIRModule *mod;
    char *ret_addr;
    SIRArray<SIRInstRef> func_stack;
    SIRArray<SIRInstRef> block_stack;
    SIRArray<char *> value_addrs;
    char *stack_memory;
    size_t stack_memory_size;
    size_t stack_memory_used;
};

SIRInterpContext *SIRInterpContextCreate(SIRModule *mod)
{
    SIRInterpContext *ctx =
        SIRAllocInit(&SIR_MALLOC_ALLOCATOR, SIRInterpContext);
    ctx->mod = mod;
    ctx->stack_memory_used = 0;
    ctx->stack_memory_size = 1 << 23;
    ctx->stack_memory =
        SIRAllocSlice(&SIR_MALLOC_ALLOCATOR, char, ctx->stack_memory_size);
    ctx->value_addrs = SIRArray<char *>::create(&SIR_MALLOC_ALLOCATOR);
    ctx->func_stack = SIRArray<SIRInstRef>::create(&SIR_MALLOC_ALLOCATOR);
    ctx->block_stack = SIRArray<SIRInstRef>::create(&SIR_MALLOC_ALLOCATOR);
    return ctx;
}

void SIRInterpContextDestroy(SIRInterpContext *ctx)
{
    ctx->value_addrs.destroy();
    ctx->func_stack.destroy();
    ctx->block_stack.destroy();
    SIRFree(&SIR_MALLOC_ALLOCATOR, ctx->stack_memory);
    SIRFree(&SIR_MALLOC_ALLOCATOR, ctx);
}

char *
SIRInterpAllocStackVal(SIRInterpContext *ctx, size_t size, size_t alignment)
{
    ctx->stack_memory_used = SIR_ROUND_UP(alignment, ctx->stack_memory_used);
    if (ctx->stack_memory_used + size > ctx->stack_memory_size) {
        // TODO: gracefully handle stack overflow
        SIR_ASSERT(!"stack overflow in interpreted code");
        return NULL;
    }

    char *addr = ctx->stack_memory + ctx->stack_memory_used;
    ctx->stack_memory_used += size;
    return addr;
}

bool SIRInterpInst(SIRInterpContext *ctx, SIRInstRef inst_ref)
{
    SIRModule *mod = ctx->mod;

    SIRInst inst = SIRModuleGetInst(mod, inst_ref);
    char *value_addr = NULL;
    bool returned = false;

    switch (inst.kind) {
    case SIRInstKind_Unknown: SIR_ASSERT(0); break;
    case SIRInstKind_Alias: {
        returned = SIRInterpInst(ctx, inst.alias.inst_ref);
        value_addr = ctx->value_addrs[inst.alias.inst_ref.id];
        break;
    }
    case SIRInstKind_Function:
    case SIRInstKind_FunctionParameter:
    case SIRInstKind_Block: {
        break;
    }
    case SIRInstKind_Global: {
        SIR_ASSERT(!"global values not supported in interpreter");
        break;
    }
    case SIRInstKind_StackSlot: {
        SIRType *type = inst.type;
        char *addr = SIRInterpAllocStackVal(
            ctx, SIRTypeSizeOf(ctx->mod, type), SIRTypeAlignOf(ctx->mod, type));

        value_addr =
            SIRInterpAllocStackVal(ctx, sizeof(char *), alignof(char *));
        *(char **)value_addr = addr;
        break;
    }
    case SIRInstKind_ImmediateBool: {
        value_addr = SIRInterpAllocStackVal(ctx, sizeof(bool), alignof(bool));
        *(bool *)value_addr = inst.imm_bool.value;
        break;
    }
    case SIRInstKind_ImmediateInt: {
        SIRType *type = inst.type;
        value_addr = SIRInterpAllocStackVal(
            ctx, SIRTypeSizeOf(ctx->mod, type), SIRTypeAlignOf(ctx->mod, type));
        if (type->int_.is_signed) {
            switch (type->int_.bits) {
            case 8: *((int8_t *)value_addr) = inst.imm_int.u64; break;
            case 16: *((int16_t *)value_addr) = inst.imm_int.u64; break;
            case 32: *((int32_t *)value_addr) = inst.imm_int.u64; break;
            case 64: *((int64_t *)value_addr) = inst.imm_int.u64; break;
            default: SIR_ASSERT(0); break;
            }
        } else {
            switch (type->int_.bits) {
            case 8: *((uint8_t *)value_addr) = inst.imm_int.u64; break;
            case 16: *((uint16_t *)value_addr) = inst.imm_int.u64; break;
            case 32: *((uint32_t *)value_addr) = inst.imm_int.u64; break;
            case 64: *((uint64_t *)value_addr) = inst.imm_int.u64; break;
            default: SIR_ASSERT(0); break;
            }
        }
        break;
    }
    case SIRInstKind_ImmediateFloat: {
        SIRType *type = inst.type;
        value_addr = SIRInterpAllocStackVal(
            ctx, SIRTypeSizeOf(ctx->mod, type), SIRTypeAlignOf(ctx->mod, type));
        switch (type->float_.bits) {
        case 32: *((float *)value_addr) = inst.imm_float.f64; break;
        case 64: *((double *)value_addr) = inst.imm_float.f64; break;
        default: SIR_ASSERT(0); break;
        }
        break;
    }
    case SIRInstKind_ReturnVoid: {
        returned = true;
        break;
    }
    case SIRInstKind_ReturnValue: {
        value_addr = ctx->value_addrs[inst.return_value.inst_ref.id];
        returned = true;
        break;
    }
    case SIRInstKind_Load: {
        SIRType *type = inst.type;
        size_t size = SIRTypeSizeOf(ctx->mod, type);
        value_addr =
            SIRInterpAllocStackVal(ctx, size, SIRTypeAlignOf(ctx->mod, type));

        SIRInstRef loaded_inst = inst.load.ptr_ref;
        SIR_ASSERT(ctx->value_addrs[loaded_inst.id]);
        char *loaded_addr = *(char **)ctx->value_addrs[loaded_inst.id];
        memcpy(value_addr, loaded_addr, size);
        break;
    }
    case SIRInstKind_Store: {
        SIRType *type = SIRModuleGetInstType(ctx->mod, inst.store.value_ref);
        size_t size = SIRTypeSizeOf(ctx->mod, type);
        SIRInstRef stored_inst = inst.store.value_ref;
        SIRInstRef ptr_inst = inst.store.ptr_ref;
        char *addr_to_store = *(char **)ctx->value_addrs[ptr_inst.id];
        memcpy(addr_to_store, ctx->value_addrs[stored_inst.id], size);
        break;
    }
    case SIRInstKind_Jump: {
        SIRInstRef current_block_ref =
            ctx->block_stack[ctx->block_stack.len - 1];

        SIRInstRef next_block_ref = inst.jump.block_ref;
        SIRInst next_block = SIRModuleGetInst(ctx->mod, next_block_ref);

        if (next_block.block.inst_refs.len > 0) {
            SIRInstRef first_inst_ref = next_block.block.inst_refs[0];
            SIRInst first_inst = SIRModuleGetInst(ctx->mod, first_inst_ref);

            if (first_inst.kind == SIRInstKind_Phi) {
                for (size_t i = 0; i < first_inst.phi.pairs.len; ++i) {
                    SIRPhiPair pair = first_inst.phi.pairs[i];
                    if (pair.block_ref.id == current_block_ref.id) {
                        ctx->value_addrs[first_inst_ref.id] =
                            ctx->value_addrs[pair.value_ref.id];
                        break;
                    }
                }
            }
        }

        ctx->block_stack[ctx->block_stack.len - 1] = next_block_ref;
        break;
    }
    case SIRInstKind_Branch: {
        value_addr = ctx->value_addrs[inst.branch.cond_inst_ref.id];

        SIRInstRef current_block_ref =
            ctx->block_stack[ctx->block_stack.len - 1];

        SIRInstRef next_block_ref = (*(bool *)value_addr)
                                        ? inst.branch.true_block_ref
                                        : inst.branch.false_block_ref;
        SIRInst next_block = SIRModuleGetInst(ctx->mod, next_block_ref);

        if (next_block.block.inst_refs.len > 0) {
            SIRInstRef first_inst_ref = next_block.block.inst_refs[0];
            SIRInst first_inst = SIRModuleGetInst(ctx->mod, first_inst_ref);

            if (first_inst.kind == SIRInstKind_Phi) {
                for (size_t i = 0; i < first_inst.phi.pairs.len; ++i) {
                    SIRPhiPair pair = first_inst.phi.pairs[i];
                    if (pair.block_ref.id == current_block_ref.id) {
                        ctx->value_addrs[first_inst_ref.id] =
                            ctx->value_addrs[pair.value_ref.id];
                        break;
                    }
                }
            }
        }

        ctx->block_stack[ctx->block_stack.len - 1] = next_block_ref;
        break;
    }
    case SIRInstKind_Phi: {
        // Phi is generated in jump/branch instructions
        break;
    }
    case SIRInstKind_FuncCall: {
        SIRInstRef called_func_ref = inst.func_call.func_ref;

        SIRInst called_func = SIRModuleGetInst(ctx->mod, called_func_ref);

        SIR_ASSERT(
            called_func.func->param_insts.len == inst.func_call.params_len);

        for (size_t i = 0; i < called_func.func->param_insts.len; ++i) {
            SIRInstRef passed_param_ref = inst.func_call.params[i];
            SIRInstRef param_ref = called_func.func->param_insts[i];

            ctx->value_addrs[param_ref.id] =
                ctx->value_addrs[passed_param_ref.id];
        }

        SIRInstRef starting_block_ref = called_func.func->blocks[0];

        ctx->func_stack.push_back(called_func_ref);
        ctx->block_stack.push_back(starting_block_ref);

        for (size_t i = 0; i < called_func.func->stack_slots.len; ++i) {
            SIRInstRef stack_slot = called_func.func->stack_slots[i];
            SIRInterpInst(ctx, stack_slot);
        }

        bool returned = false;
        while (!returned) {
            SIRInstRef block_ref = ctx->block_stack[ctx->block_stack.len - 1];
            SIRInst block = SIRModuleGetInst(mod, block_ref);
            for (size_t i = 0; i < block.block.inst_refs.len; ++i) {
                SIRInstRef inst_ref = block.block.inst_refs[i];
                returned = SIRInterpInst(ctx, inst_ref);
                if (returned) {
                    ctx->ret_addr = ctx->value_addrs[inst_ref.id];
                    break;
                }
            }
        }

        ctx->block_stack.pop();
        ctx->func_stack.pop();

        value_addr = ctx->ret_addr;

        break;
    }
    case SIRInstKind_BitCast: {
        value_addr = ctx->value_addrs[inst.bit_cast.inst_ref.id];
        break;
    }
    case SIRInstKind_ZExt: {
        SIRType *source_type =
            SIRModuleGetInstType(ctx->mod, inst.zext.inst_ref);
        size_t source_size = SIRTypeSizeOf(ctx->mod, source_type);
        SIRType *dest_type = SIRModuleGetInstType(ctx->mod, inst_ref);
        size_t dest_size = SIRTypeSizeOf(ctx->mod, dest_type);

        SIR_ASSERT(dest_size >= source_size);

        char *source_addr = ctx->value_addrs[inst.zext.inst_ref.id];

        value_addr = SIRInterpAllocStackVal(
            ctx, dest_size, SIRTypeAlignOf(ctx->mod, dest_type));

        uint64_t value = 0;
        switch (source_size) {
        case 1: value = *(uint8_t *)source_addr; break;
        case 2: value = *(uint16_t *)source_addr; break;
        case 4: value = *(uint32_t *)source_addr; break;
        case 8: value = *(uint64_t *)source_addr; break;
        default: SIR_ASSERT(0); break;
        }

        switch (dest_size) {
        case 1: *(uint8_t *)value_addr = value; break;
        case 2: *(uint16_t *)value_addr = value; break;
        case 4: *(uint32_t *)value_addr = value; break;
        case 8: *(uint64_t *)value_addr = value; break;
        default: SIR_ASSERT(0); break;
        }
        break;
    }
    case SIRInstKind_SExt: {
        SIRType *source_type =
            SIRModuleGetInstType(ctx->mod, inst.sext.inst_ref);
        size_t source_size = SIRTypeSizeOf(ctx->mod, source_type);
        SIRType *dest_type = SIRModuleGetInstType(ctx->mod, inst_ref);
        size_t dest_size = SIRTypeSizeOf(ctx->mod, dest_type);

        SIR_ASSERT(dest_size >= source_size);

        char *source_addr = ctx->value_addrs[inst.sext.inst_ref.id];

        value_addr = SIRInterpAllocStackVal(
            ctx, dest_size, SIRTypeAlignOf(ctx->mod, dest_type));

        int64_t value = 0;
        switch (source_size) {
        case 1: value = *(int8_t *)source_addr; break;
        case 2: value = *(int16_t *)source_addr; break;
        case 4: value = *(int32_t *)source_addr; break;
        case 8: value = *(int64_t *)source_addr; break;
        default: SIR_ASSERT(0); break;
        }

        switch (dest_size) {
        case 1: *(int8_t *)value_addr = value; break;
        case 2: *(int16_t *)value_addr = value; break;
        case 4: *(int32_t *)value_addr = value; break;
        case 8: *(int64_t *)value_addr = value; break;
        default: SIR_ASSERT(0); break;
        }
        break;
    }
    case SIRInstKind_Trunc: {
        SIRType *source_type =
            SIRModuleGetInstType(ctx->mod, inst.trunc.inst_ref);
        size_t source_size = SIRTypeSizeOf(ctx->mod, source_type);
        SIRType *dest_type = SIRModuleGetInstType(ctx->mod, inst_ref);
        size_t dest_size = SIRTypeSizeOf(ctx->mod, dest_type);

        SIR_ASSERT(dest_size <= source_size);

        char *source_addr = ctx->value_addrs[inst.trunc.inst_ref.id];

        value_addr = SIRInterpAllocStackVal(
            ctx, dest_size, SIRTypeAlignOf(ctx->mod, dest_type));

        uint64_t value = 0;
        switch (source_size) {
        case 1: value = *(uint8_t *)source_addr; break;
        case 2: value = *(uint16_t *)source_addr; break;
        case 4: value = *(uint32_t *)source_addr; break;
        case 8: value = *(uint64_t *)source_addr; break;
        default: SIR_ASSERT(0);
        }

        switch (dest_size) {
        case 1: *(uint8_t *)value_addr = value; break;
        case 2: *(uint16_t *)value_addr = value; break;
        case 4: *(uint32_t *)value_addr = value; break;
        case 8: *(uint64_t *)value_addr = value; break;
        default: SIR_ASSERT(0);
        }

        break;
    }
    case SIRInstKind_ArrayElemPtr: {
        SIRInstRef index_ref = inst.array_elem_ptr.index_ref;
        SIRType *index_type =
            SIRModuleGetInstType(ctx->mod, inst.array_elem_ptr.index_ref);
        SIRType *array_ptr_type =
            SIRModuleGetInstType(ctx->mod, inst.array_elem_ptr.accessed_ref);
        SIRType *array_type = array_ptr_type->pointer.sub;

        SIR_ASSERT(array_ptr_type->kind == SIRTypeKind_Pointer);
        SIR_ASSERT(array_type->kind == SIRTypeKind_Array);
        SIR_ASSERT(index_type->kind == SIRTypeKind_Int);

        char *array_addr =
            *(char **)ctx->value_addrs[inst.array_elem_ptr.accessed_ref.id];

        uint64_t index = 0;

        switch (index_type->int_.bits) {
        case 8: index = *(uint8_t *)ctx->value_addrs[index_ref.id]; break;
        case 16: index = *(uint16_t *)ctx->value_addrs[index_ref.id]; break;
        case 32: index = *(uint32_t *)ctx->value_addrs[index_ref.id]; break;
        case 64: index = *(uint64_t *)ctx->value_addrs[index_ref.id]; break;
        }

        char *elem_addr =
            array_addr + index * SIRTypeSizeOf(ctx->mod, array_type->array.sub);

        value_addr =
            SIRInterpAllocStackVal(ctx, sizeof(uint64_t), alignof(uint64_t));
        *(char **)value_addr = elem_addr;
        break;
    }
    case SIRInstKind_StructElemPtr: {
        SIRType *struct_ptr_type =
            SIRModuleGetInstType(ctx->mod, inst.struct_elem_ptr.accessed_ref);
        SIRType *struct_type = struct_ptr_type->pointer.sub;

        SIR_ASSERT(struct_ptr_type->kind == SIRTypeKind_Pointer);
        SIR_ASSERT(struct_type->kind == SIRTypeKind_Struct);

        char *struct_addr =
            *(char **)ctx->value_addrs[inst.struct_elem_ptr.accessed_ref.id];

        uint32_t field_offset = SIRTypeStructOffsetOf(
            ctx->mod, struct_type, inst.struct_elem_ptr.field_index);

        char *field_addr = struct_addr + field_offset;

        value_addr =
            SIRInterpAllocStackVal(ctx, sizeof(uint64_t), alignof(uint64_t));
        *(char **)value_addr = field_addr;
        break;
    }
    case SIRInstKind_ExtractArrayElem: {
        SIRType *array_type = SIRModuleGetInstType(
            ctx->mod, inst.extract_array_elem.accessed_ref);

        SIR_ASSERT(array_type->kind == SIRTypeKind_Array);

        char *array_addr =
            ctx->value_addrs[inst.extract_array_elem.accessed_ref.id];
        uint64_t index = inst.extract_array_elem.elem_index;

        size_t elem_size = SIRTypeSizeOf(ctx->mod, array_type->array.sub);

        char *elem_addr = array_addr + index * elem_size;

        value_addr = SIRInterpAllocStackVal(
            ctx, elem_size, SIRTypeAlignOf(ctx->mod, array_type->array.sub));
        memcpy(value_addr, elem_addr, elem_size);
        break;
    }
    case SIRInstKind_ExtractStructElem: {
        SIRType *struct_type = SIRModuleGetInstType(
            ctx->mod, inst.extract_struct_elem.accessed_ref);

        SIR_ASSERT(struct_type->kind == SIRTypeKind_Struct);

        uint32_t field_index = inst.struct_elem_ptr.field_index;

        uint32_t field_offset =
            SIRTypeStructOffsetOf(ctx->mod, struct_type, field_index);

        size_t field_size =
            SIRTypeSizeOf(ctx->mod, struct_type->struct_.fields[field_index]);
        size_t field_align =
            SIRTypeAlignOf(ctx->mod, struct_type->struct_.fields[field_index]);

        char *struct_addr =
            ctx->value_addrs[inst.extract_struct_elem.accessed_ref.id];

        char *field_addr = struct_addr + field_offset;
        value_addr = SIRInterpAllocStackVal(ctx, field_size, field_align);
        memcpy(value_addr, field_addr, field_size);

        break;
    }
    case SIRInstKind_Binop: {
        SIRType *type = inst.type;
        size_t value_size = SIRTypeSizeOf(ctx->mod, type);
        value_addr = SIRInterpAllocStackVal(
            ctx, value_size, SIRTypeAlignOf(ctx->mod, type));

        char *left_addr = ctx->value_addrs[inst.binop.left_ref.id];
        char *right_addr = ctx->value_addrs[inst.binop.right_ref.id];

#define SIR_INTERP_BINOP(RESULT_TYPE, OP_TYPE, OP)                             \
    (*(RESULT_TYPE *)value_addr) =                                             \
        (*(OP_TYPE *)left_addr)OP(*(OP_TYPE *)right_addr)

#define SIR_INTERP_UINT_CMP_BINOP(OP)                                          \
    switch (value_size) {                                                      \
    case 1: SIR_INTERP_BINOP(bool, uint8_t, OP); break;                        \
    case 2: SIR_INTERP_BINOP(bool, uint16_t, OP); break;                       \
    case 4: SIR_INTERP_BINOP(bool, uint32_t, OP); break;                       \
    case 8: SIR_INTERP_BINOP(bool, uint64_t, OP); break;                       \
    }

#define SIR_INTERP_INT_CMP_BINOP(OP)                                           \
    switch (value_size) {                                                      \
    case 1: SIR_INTERP_BINOP(bool, int8_t, OP); break;                         \
    case 2: SIR_INTERP_BINOP(bool, int16_t, OP); break;                        \
    case 4: SIR_INTERP_BINOP(bool, int32_t, OP); break;                        \
    case 8: SIR_INTERP_BINOP(bool, int64_t, OP); break;                        \
    }

#define SIR_INTERP_FLOAT_CMP_BINOP(OP)                                         \
    switch (value_size) {                                                      \
    case 4: SIR_INTERP_BINOP(bool, float, OP); break;                          \
    case 8: SIR_INTERP_BINOP(bool, double, OP); break;                         \
    }

#define SIR_INTERP_UINT_BINOP(OP)                                              \
    switch (value_size) {                                                      \
    case 1: SIR_INTERP_BINOP(uint8_t, uint8_t, OP); break;                     \
    case 2: SIR_INTERP_BINOP(uint16_t, uint16_t, OP); break;                   \
    case 4: SIR_INTERP_BINOP(uint32_t, uint32_t, OP); break;                   \
    case 8: SIR_INTERP_BINOP(uint64_t, uint64_t, OP); break;                   \
    }

#define SIR_INTERP_INT_BINOP(OP)                                               \
    switch (value_size) {                                                      \
    case 1: SIR_INTERP_BINOP(int8_t, int8_t, OP); break;                       \
    case 2: SIR_INTERP_BINOP(int16_t, int16_t, OP); break;                     \
    case 4: SIR_INTERP_BINOP(int32_t, int32_t, OP); break;                     \
    case 8: SIR_INTERP_BINOP(int64_t, int64_t, OP); break;                     \
    }

#define SIR_INTERP_FLOAT_BINOP(OP)                                             \
    switch (value_size) {                                                      \
    case 4: SIR_INTERP_BINOP(float, float, OP); break;                         \
    case 8: SIR_INTERP_BINOP(double, double, OP); break;                       \
    }

        switch (inst.binop.op) {
        case SIRBinaryOperation_Unknown:
        case SIRBinaryOperation_MAX: SIR_ASSERT(0); break;

        case SIRBinaryOperation_IAdd: {
            SIR_INTERP_UINT_BINOP(+);
            break;
        }
        case SIRBinaryOperation_ISub: {
            SIR_INTERP_UINT_BINOP(-);
            break;
        }
        case SIRBinaryOperation_IMul: {
            SIR_INTERP_UINT_BINOP(*);
            break;
        }
        case SIRBinaryOperation_UDiv: {
            SIR_INTERP_UINT_BINOP(/);
            break;
        }
        case SIRBinaryOperation_SDiv: {
            SIR_INTERP_INT_BINOP(/);
            break;
        }
        case SIRBinaryOperation_URem: {
            SIR_INTERP_UINT_BINOP(%);
            break;
        }
        case SIRBinaryOperation_SRem: {
            SIR_INTERP_INT_BINOP(%);
            break;
        }
        case SIRBinaryOperation_FAdd: {
            SIR_INTERP_FLOAT_BINOP(+);
            break;
        }
        case SIRBinaryOperation_FSub: {
            SIR_INTERP_FLOAT_BINOP(-);
            break;
        }
        case SIRBinaryOperation_FMul: {
            SIR_INTERP_FLOAT_BINOP(*);
            break;
        }
        case SIRBinaryOperation_FDiv: {
            SIR_INTERP_FLOAT_BINOP(/);
            break;
        }
        case SIRBinaryOperation_FRem: {
            switch (value_size) {
            case 4:
                *(float *)value_addr =
                    fmodf(*(float *)left_addr, *(float *)right_addr);
                break;
            case 8:
                *(double *)value_addr =
                    fmod(*(double *)left_addr, *(double *)right_addr);
                break;
            }
            break;
        }
        case SIRBinaryOperation_BEQ: {
            SIR_INTERP_BINOP(bool, bool, ==);
            break;
        }
        case SIRBinaryOperation_BNE: {
            SIR_INTERP_BINOP(bool, bool, !=);
            break;
        }
        case SIRBinaryOperation_IEQ: SIR_INTERP_UINT_CMP_BINOP(==); break;
        case SIRBinaryOperation_INE: SIR_INTERP_UINT_CMP_BINOP(!=); break;
        case SIRBinaryOperation_UGT: SIR_INTERP_UINT_CMP_BINOP(>); break;
        case SIRBinaryOperation_UGE: SIR_INTERP_UINT_CMP_BINOP(>=); break;
        case SIRBinaryOperation_ULT: SIR_INTERP_UINT_CMP_BINOP(<); break;
        case SIRBinaryOperation_ULE: SIR_INTERP_UINT_CMP_BINOP(<=); break;
        case SIRBinaryOperation_SGT: SIR_INTERP_INT_CMP_BINOP(>); break;
        case SIRBinaryOperation_SGE: SIR_INTERP_INT_CMP_BINOP(>=); break;
        case SIRBinaryOperation_SLT: SIR_INTERP_INT_CMP_BINOP(<); break;
        case SIRBinaryOperation_SLE: SIR_INTERP_INT_CMP_BINOP(<=); break;

        case SIRBinaryOperation_FEQ: SIR_INTERP_FLOAT_CMP_BINOP(==); break;
        case SIRBinaryOperation_FNE: SIR_INTERP_FLOAT_CMP_BINOP(!=); break;
        case SIRBinaryOperation_FGT: SIR_INTERP_FLOAT_CMP_BINOP(>); break;
        case SIRBinaryOperation_FGE: SIR_INTERP_FLOAT_CMP_BINOP(>=); break;
        case SIRBinaryOperation_FLT: SIR_INTERP_FLOAT_CMP_BINOP(<); break;
        case SIRBinaryOperation_FLE: SIR_INTERP_FLOAT_CMP_BINOP(<=); break;

        case SIRBinaryOperation_Shl: SIR_INTERP_UINT_BINOP(<<); break;
        case SIRBinaryOperation_LShr: SIR_INTERP_UINT_BINOP(>>); break;
        case SIRBinaryOperation_AShr: SIR_INTERP_INT_BINOP(>>); break;

        case SIRBinaryOperation_And: SIR_INTERP_UINT_BINOP(&); break;
        case SIRBinaryOperation_Or: SIR_INTERP_UINT_BINOP(|); break;
        case SIRBinaryOperation_Xor: SIR_INTERP_UINT_BINOP(^); break;
        }
        break;
    }
    }

    ctx->value_addrs[inst_ref.id] = value_addr;

    return returned;
}

void SIRInterpFunction(SIRInterpContext *ctx, SIRInstRef func_ref, void *result)
{
    SIRModule *mod = ctx->mod;

    ctx->stack_memory_used = 0;
    ctx->value_addrs.resize(ctx->mod->insts.len);
    ctx->func_stack.resize(0);
    ctx->block_stack.resize(0);

    SIRInst func = SIRModuleGetInst(mod, func_ref);
    SIRType *func_ret_type = func.func->return_type;
    SIR_ASSERT(func.kind == SIRInstKind_Function);

    SIRInstRef starting_block_ref = func.func->blocks[0];

    ctx->func_stack.push_back(func_ref);
    ctx->block_stack.push_back(starting_block_ref);

    for (size_t i = 0; i < func.func->stack_slots.len; ++i) {
        SIRInstRef stack_slot = func.func->stack_slots[i];
        SIRInterpInst(ctx, stack_slot);
    }

    bool returned = false;
    while (!returned) {
        SIRInstRef block_ref = ctx->block_stack[ctx->block_stack.len - 1];
        SIRInst block = SIRModuleGetInst(mod, block_ref);
        for (size_t i = 0; i < block.block.inst_refs.len; ++i) {
            SIRInstRef inst_ref = block.block.inst_refs[i];
            returned = SIRInterpInst(ctx, inst_ref);
            if (returned) {
                ctx->ret_addr = ctx->value_addrs[inst_ref.id];
                break;
            }
        }
    }

    ctx->block_stack.pop();
    ctx->func_stack.pop();

    memcpy(result, ctx->ret_addr, SIRTypeSizeOf(ctx->mod, func_ret_type));
}