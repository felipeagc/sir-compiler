#include "sir_interp.h"
#include "sir_base.hpp"
#include "sir_ir.hpp"
#include <math.h>

struct SIRInterpContext {
    SIRModule *mod;
    uint32_t ret_addr;
    SIRArray<SIRInstRef> func_stack;
    SIRArray<SIRInstRef> block_stack;
    SIRArray<uint32_t> value_addrs;
    char *memory;
    size_t memory_size;
    size_t memory_used;
};

SIRInterpContext *SIRInterpContextCreate(SIRModule *mod)
{
    SIRInterpContext *ctx =
        SIRAllocInit(&SIR_MALLOC_ALLOCATOR, SIRInterpContext);
    ctx->mod = mod;
    ctx->memory_used = 0;
    ctx->memory_size = 1 << 24;
    ctx->memory = SIRAllocSlice(&SIR_MALLOC_ALLOCATOR, char, ctx->memory_size);
    ctx->value_addrs = SIRArray<uint32_t>::create(&SIR_MALLOC_ALLOCATOR);
    ctx->func_stack = SIRArray<SIRInstRef>::create(&SIR_MALLOC_ALLOCATOR);
    ctx->block_stack = SIRArray<SIRInstRef>::create(&SIR_MALLOC_ALLOCATOR);
    return ctx;
}

void SIRInterpContextDestroy(SIRInterpContext *ctx)
{
    ctx->value_addrs.destroy();
    ctx->func_stack.destroy();
    ctx->block_stack.destroy();
    SIRFree(&SIR_MALLOC_ALLOCATOR, ctx->memory);
    SIRFree(&SIR_MALLOC_ALLOCATOR, ctx);
}

uint32_t SIRInterpAllocVal(SIRInterpContext *ctx, size_t size, size_t alignment)
{
    // TODO: check for out-of-memory
    ctx->memory_used = SIR_ROUND_UP(alignment, ctx->memory_used);
    uint32_t addr = ctx->memory_used;
    ctx->memory_used += size;
    return addr;
}

bool SIRInterpInst(SIRInterpContext *ctx, SIRInstRef inst_ref)
{
    SIRModule *mod = ctx->mod;

    SIRInst inst = SIRModuleGetInst(mod, inst_ref);
    uint32_t value_addr = {};
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
        uint64_t addr = SIRInterpAllocVal(
            ctx, SIRTypeSizeOf(ctx->mod, type), SIRTypeAlignOf(ctx->mod, type));

        value_addr =
            SIRInterpAllocVal(ctx, sizeof(uint64_t), alignof(uint64_t));
        uint64_t *ptr = (uint64_t *)&ctx->memory[value_addr];
        *ptr = addr;
        break;
    }
    case SIRInstKind_ImmediateBool: {
        value_addr = SIRInterpAllocVal(ctx, sizeof(bool), alignof(bool));
        bool *ptr = (bool *)&ctx->memory[value_addr];
        *ptr = inst.imm_bool.value;
        break;
    }
    case SIRInstKind_ImmediateInt: {
        SIRType *type = inst.type;
        value_addr = SIRInterpAllocVal(
            ctx, SIRTypeSizeOf(ctx->mod, type), SIRTypeAlignOf(ctx->mod, type));
        char *ptr = &ctx->memory[value_addr];
        if (type->int_.is_signed) {
            switch (type->int_.bits) {
            case 8: *((int8_t *)ptr) = inst.imm_int.u64; break;
            case 16: *((int16_t *)ptr) = inst.imm_int.u64; break;
            case 32: *((int32_t *)ptr) = inst.imm_int.u64; break;
            case 64: *((int64_t *)ptr) = inst.imm_int.u64; break;
            default: SIR_ASSERT(0); break;
            }
        } else {
            switch (type->int_.bits) {
            case 8: *((uint8_t *)ptr) = inst.imm_int.u64; break;
            case 16: *((uint16_t *)ptr) = inst.imm_int.u64; break;
            case 32: *((uint32_t *)ptr) = inst.imm_int.u64; break;
            case 64: *((uint64_t *)ptr) = inst.imm_int.u64; break;
            default: SIR_ASSERT(0); break;
            }
        }
        break;
    }
    case SIRInstKind_ImmediateFloat: {
        SIRType *type = inst.type;
        value_addr = SIRInterpAllocVal(
            ctx, SIRTypeSizeOf(ctx->mod, type), SIRTypeAlignOf(ctx->mod, type));
        char *ptr = &ctx->memory[value_addr];
        switch (type->float_.bits) {
        case 32: *((float *)ptr) = inst.imm_float.f64; break;
        case 64: *((double *)ptr) = inst.imm_float.f64; break;
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
            SIRInterpAllocVal(ctx, size, SIRTypeAlignOf(ctx->mod, type));
        char *ptr = &ctx->memory[value_addr];

        SIRInstRef loaded_inst = inst.load.ptr_ref;
        uint64_t loaded_addr =
            *(uint64_t *)&ctx->memory[ctx->value_addrs[loaded_inst.id]];
        memcpy(ptr, &ctx->memory[loaded_addr], size);
        break;
    }
    case SIRInstKind_Store: {
        SIRType *type = SIRModuleGetInstType(ctx->mod, inst.store.value_ref);
        size_t size = SIRTypeSizeOf(ctx->mod, type);
        SIRInstRef stored_inst = inst.store.value_ref;
        SIRInstRef ptr_inst = inst.store.ptr_ref;
        uint64_t addr_to_store =
            *(uint64_t *)&ctx->memory[ctx->value_addrs[ptr_inst.id]];
        memcpy(
            &ctx->memory[addr_to_store],
            &ctx->memory[ctx->value_addrs[stored_inst.id]],
            size);
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
        bool *ptr = (bool *)&ctx->memory[value_addr];

        SIRInstRef current_block_ref =
            ctx->block_stack[ctx->block_stack.len - 1];

        SIRInstRef next_block_ref =
            (*ptr) ? inst.branch.true_block_ref : inst.branch.false_block_ref;
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
        SIR_ASSERT(!"unimplemented func_call");
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

        uint64_t source_addr = ctx->value_addrs[inst.zext.inst_ref.id];

        value_addr = SIRInterpAllocVal(
            ctx, dest_size, SIRTypeAlignOf(ctx->mod, dest_type));

        uint64_t value = 0;
        switch (source_size) {
        case 1: value = *(uint8_t *)&ctx->memory[source_addr];
        case 2: value = *(uint16_t *)&ctx->memory[source_addr];
        case 4: value = *(uint32_t *)&ctx->memory[source_addr];
        case 8: value = *(uint64_t *)&ctx->memory[source_addr];
        default: SIR_ASSERT(0);
        }

        switch (dest_size) {
        case 1: *(uint8_t *)&ctx->memory[value_addr] = value;
        case 2: *(uint16_t *)&ctx->memory[value_addr] = value;
        case 4: *(uint32_t *)&ctx->memory[value_addr] = value;
        case 8: *(uint64_t *)&ctx->memory[value_addr] = value;
        default: SIR_ASSERT(0);
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

        uint64_t source_addr = ctx->value_addrs[inst.sext.inst_ref.id];

        value_addr = SIRInterpAllocVal(
            ctx, dest_size, SIRTypeAlignOf(ctx->mod, dest_type));

        int64_t value = 0;
        switch (source_size) {
        case 1: value = *(int8_t *)&ctx->memory[source_addr];
        case 2: value = *(int16_t *)&ctx->memory[source_addr];
        case 4: value = *(int32_t *)&ctx->memory[source_addr];
        case 8: value = *(int64_t *)&ctx->memory[source_addr];
        default: SIR_ASSERT(0);
        }

        switch (dest_size) {
        case 1: *(int8_t *)&ctx->memory[value_addr] = value;
        case 2: *(int16_t *)&ctx->memory[value_addr] = value;
        case 4: *(int32_t *)&ctx->memory[value_addr] = value;
        case 8: *(int64_t *)&ctx->memory[value_addr] = value;
        default: SIR_ASSERT(0);
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

        uint64_t source_addr = ctx->value_addrs[inst.trunc.inst_ref.id];

        value_addr = SIRInterpAllocVal(
            ctx, dest_size, SIRTypeAlignOf(ctx->mod, dest_type));

        uint64_t value = 0;
        switch (source_size) {
        case 1: value = *(uint8_t *)&ctx->memory[source_addr];
        case 2: value = *(uint16_t *)&ctx->memory[source_addr];
        case 4: value = *(uint32_t *)&ctx->memory[source_addr];
        case 8: value = *(uint64_t *)&ctx->memory[source_addr];
        default: SIR_ASSERT(0);
        }

        switch (dest_size) {
        case 1: *(uint8_t *)&ctx->memory[value_addr] = value;
        case 2: *(uint16_t *)&ctx->memory[value_addr] = value;
        case 4: *(uint32_t *)&ctx->memory[value_addr] = value;
        case 8: *(uint64_t *)&ctx->memory[value_addr] = value;
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

        uint64_t array_addr =
            *(uint64_t *)&ctx->memory
                 [ctx->value_addrs[inst.array_elem_ptr.accessed_ref.id]];

        uint64_t index = 0;

        switch (index_type->int_.bits) {
        case 8:
            index = *(uint8_t *)&ctx->memory[ctx->value_addrs[index_ref.id]];
            break;
        case 16:
            index = *(uint16_t *)&ctx->memory[ctx->value_addrs[index_ref.id]];
            break;
        case 32:
            index = *(uint32_t *)&ctx->memory[ctx->value_addrs[index_ref.id]];
            break;
        case 64:
            index = *(uint64_t *)&ctx->memory[ctx->value_addrs[index_ref.id]];
            break;
        }

        uint64_t elem_addr =
            array_addr + index * SIRTypeSizeOf(ctx->mod, array_type->array.sub);

        value_addr =
            SIRInterpAllocVal(ctx, sizeof(uint64_t), alignof(uint64_t));
        *(uint64_t *)&ctx->memory[value_addr] = elem_addr;
        break;
    }
    case SIRInstKind_StructElemPtr: {
        SIRType *struct_ptr_type =
            SIRModuleGetInstType(ctx->mod, inst.struct_elem_ptr.accessed_ref);
        SIRType *struct_type = struct_ptr_type->pointer.sub;

        SIR_ASSERT(struct_ptr_type->kind == SIRTypeKind_Pointer);
        SIR_ASSERT(struct_type->kind == SIRTypeKind_Struct);

        uint64_t struct_addr =
            *(uint64_t *)&ctx->memory
                 [ctx->value_addrs[inst.struct_elem_ptr.accessed_ref.id]];

        uint32_t field_offset = SIRTypeStructOffsetOf(
            ctx->mod, struct_type, inst.struct_elem_ptr.field_index);

        uint64_t field_addr = struct_addr + field_offset;

        value_addr =
            SIRInterpAllocVal(ctx, sizeof(uint64_t), alignof(uint64_t));
        *(uint64_t *)&ctx->memory[value_addr] = field_addr;
        break;
    }
    case SIRInstKind_ExtractArrayElem: {
        SIRType *array_type = SIRModuleGetInstType(
            ctx->mod, inst.extract_array_elem.accessed_ref);

        SIR_ASSERT(array_type->kind == SIRTypeKind_Array);

        uint64_t array_addr =
            ctx->value_addrs[inst.extract_array_elem.accessed_ref.id];
        uint64_t index = inst.extract_array_elem.elem_index;

        size_t elem_size = SIRTypeSizeOf(ctx->mod, array_type->array.sub);

        uint64_t elem_addr = array_addr + index * elem_size;

        value_addr = SIRInterpAllocVal(
            ctx, elem_size, SIRTypeAlignOf(ctx->mod, array_type->array.sub));
        memcpy(&ctx->memory[value_addr], &ctx->memory[elem_addr], elem_size);
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

        uint64_t struct_addr =
            ctx->value_addrs[inst.extract_struct_elem.accessed_ref.id];

        uint64_t field_addr = struct_addr + field_offset;
        value_addr = SIRInterpAllocVal(ctx, field_size, field_align);
        memcpy(&ctx->memory[value_addr], &ctx->memory[field_addr], field_size);

        break;
    }
    case SIRInstKind_Binop: {
        SIRType *type = inst.type;
        size_t value_size = SIRTypeSizeOf(ctx->mod, type);
        value_addr =
            SIRInterpAllocVal(ctx, value_size, SIRTypeAlignOf(ctx->mod, type));

        uint32_t left_addr = ctx->value_addrs[inst.binop.left_ref.id];
        uint32_t right_addr = ctx->value_addrs[inst.binop.right_ref.id];

#define SIR_INTERP_BINOP(RESULT_TYPE, OP_TYPE, OP)                             \
    (*(RESULT_TYPE *)&ctx->memory[value_addr]) =                               \
        (*(OP_TYPE *)&ctx->memory[left_addr])OP(                               \
            *(OP_TYPE *)&ctx->memory[right_addr])

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
                *(float *)&ctx->memory[value_addr] = fmodf(
                    *(float *)&ctx->memory[left_addr],
                    *(float *)&ctx->memory[right_addr]);
                break;
            case 8:
                *(double *)&ctx->memory[value_addr] = fmod(
                    *(double *)&ctx->memory[left_addr],
                    *(double *)&ctx->memory[right_addr]);
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

    ctx->memory_used = 1; // Must be 1 so 0 is the null pointer
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

    while (ctx->func_stack.len > 0) {
        SIRInstRef block_ref = ctx->block_stack[ctx->block_stack.len - 1];
        SIRInst block = SIRModuleGetInst(mod, block_ref);
        for (size_t i = 0; i < block.block.inst_refs.len; ++i) {
            SIRInstRef inst_ref = block.block.inst_refs[i];
            if (SIRInterpInst(ctx, inst_ref)) {
                ctx->ret_addr = ctx->value_addrs[inst_ref.id];
                break;
            }
        }
    }

    ctx->block_stack.pop();
    ctx->func_stack.pop();

    memcpy(
        result,
        &ctx->memory[ctx->ret_addr],
        SIRTypeSizeOf(ctx->mod, func_ret_type));
}
