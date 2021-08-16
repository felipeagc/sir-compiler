#include "sir_interp.h"
#include "sir_base.hpp"
#include "sir_ir.hpp"

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

bool SIRInterpInst(
    SIRInterpContext *ctx, SIRInstRef func_ref, SIRInstRef inst_ref)
{
    SIRModule *mod = ctx->mod;

    SIRInst inst = SIRModuleGetInst(mod, inst_ref);
    uint32_t value_addr = {};
    bool returned = false;

    switch (inst.kind) {
    case SIRInstKind_Unknown: SIR_ASSERT(0); break;
    case SIRInstKind_Alias: {
        returned = SIRInterpInst(ctx, func_ref, inst.alias.inst_ref);
        value_addr = ctx->value_addrs[inst.alias.inst_ref.id];
        break;
    }
    case SIRInstKind_Function:
    case SIRInstKind_FunctionParameter:
    case SIRInstKind_Block: {
        break;
    }
    case SIRInstKind_Global:
    case SIRInstKind_StackSlot: {
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
        ctx->func_stack.pop();
        ctx->block_stack.pop();
        returned = true;
        break;
    }
    case SIRInstKind_ReturnValue: {
        ctx->func_stack.pop();
        ctx->block_stack.pop();
        value_addr = ctx->value_addrs[inst.return_value.inst_ref.id];
        returned = true;
        break;
    }
    case SIRInstKind_Load: {
        SIR_ASSERT(!"unimplemented");
        break;
    }
    case SIRInstKind_Store: {
        SIR_ASSERT(!"unimplemented");
        break;
    }
    case SIRInstKind_Jump: {
        ctx->block_stack[ctx->block_stack.len - 1] = inst.jump.block_ref;
        break;
    }
    case SIRInstKind_Branch: {
        value_addr = ctx->value_addrs[inst.branch.cond_inst_ref.id];
        bool *ptr = (bool *)&ctx->memory[value_addr];
        if (*ptr) {
            ctx->block_stack[ctx->block_stack.len - 1] =
                inst.branch.true_block_ref;
        } else {
            ctx->block_stack[ctx->block_stack.len - 1] =
                inst.branch.false_block_ref;
        }
        break;
    }
    case SIRInstKind_Phi: {
        SIR_ASSERT(!"unimplemented phi");
        break;
    }
    case SIRInstKind_FuncCall: {
        SIR_ASSERT(!"unimplemented func_call");
        break;
    }
    case SIRInstKind_PtrCast: {
        SIR_ASSERT(!"unimplemented ptr_cast");
        break;
    }
    case SIRInstKind_ZExt: {
        SIR_ASSERT(!"unimplemented zext");
        break;
    }
    case SIRInstKind_SExt: {
        SIR_ASSERT(!"unimplemented sext");
        break;
    }
    case SIRInstKind_Trunc: {
        SIR_ASSERT(!"unimplemented trunc");
        break;
    }
    case SIRInstKind_ArrayElemPtr: {
        SIR_ASSERT(!"unimplemented array_elem_ptr");
        break;
    }
    case SIRInstKind_StructElemPtr: {
        SIR_ASSERT(!"unimplemented struct_elem_ptr");
        break;
    }
    case SIRInstKind_ExtractArrayElem: {
        SIR_ASSERT(!"unimplemented extract_array_elem");
        break;
    }
    case SIRInstKind_ExtractStructElem: {
        SIR_ASSERT(!"unimplemented extract_struct_elem");
        break;
    }
    case SIRInstKind_Binop: {
        SIR_ASSERT(!"unimplemented binop");
        break;
    }
    }

    ctx->value_addrs[inst_ref.id] = value_addr;

    return returned;
}

void SIRInterpFunction(SIRInterpContext *ctx, SIRInstRef func_ref, void *result)
{
    SIRModule *mod = ctx->mod;

    ctx->memory_used = 0;
    ctx->value_addrs.resize(ctx->mod->insts.len);
    ctx->func_stack.resize(0);
    ctx->block_stack.resize(0);

    SIRInst func = SIRModuleGetInst(mod, func_ref);
    SIRType *func_ret_type = func.func->return_type;
    SIR_ASSERT(func.kind == SIRInstKind_Function);

    SIRInstRef starting_block_ref = func.func->blocks[0];

    ctx->func_stack.push_back(func_ref);
    ctx->block_stack.push_back(starting_block_ref);

    while (ctx->func_stack.len > 0) {
        SIRInstRef block_ref = ctx->block_stack[ctx->block_stack.len - 1];
        SIRInst block = SIRModuleGetInst(mod, block_ref);
        for (size_t i = 0; i < block.block.inst_refs.len; ++i) {
            SIRInstRef inst_ref = block.block.inst_refs[i];
            if (SIRInterpInst(ctx, func_ref, inst_ref)) {
                ctx->ret_addr = ctx->value_addrs[inst_ref.id];
                break;
            }
        }
    }

    memcpy(
        result,
        &ctx->memory[ctx->ret_addr],
        SIRTypeSizeOf(ctx->mod, func_ret_type));
}
