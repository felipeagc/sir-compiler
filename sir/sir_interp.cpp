#include "sir_interp.h"
#include "sir_base.hpp"
#include "sir_ir.hpp"
#include <math.h>
#include <setjmp.h>

struct SIRInterpContext {
    SIRInterpResult err_code;
    SIRModule *mod;
    SIRArray<SIRInstRef> func_stack;
    SIRArray<SIRInstRef> block_stack;
    SIRArray<size_t> stack_usage_stack;
    SIRArray<char *> value_addrs;
    char *stack_memory;
    size_t stack_memory_size;
    size_t stack_memory_used;
    jmp_buf jump_buf;
};

SIRInterpContext *SIRInterpContextCreate(SIRModule *mod)
{
    ZoneScoped;

    SIRInterpContext *ctx =
        SIRAllocInit(&SIR_MALLOC_ALLOCATOR, SIRInterpContext);
    ctx->mod = mod;
    ctx->stack_memory_used = 0;
    ctx->stack_memory_size = 1 << 23;
    ctx->stack_memory =
        SIRAllocSlice(&SIR_MALLOC_ALLOCATOR, char, ctx->stack_memory_size);
    ctx->stack_usage_stack = SIRArray<size_t>::create(&SIR_MALLOC_ALLOCATOR);
    ctx->value_addrs = SIRArray<char *>::create(&SIR_MALLOC_ALLOCATOR);
    ctx->func_stack = SIRArray<SIRInstRef>::create(&SIR_MALLOC_ALLOCATOR);
    ctx->block_stack = SIRArray<SIRInstRef>::create(&SIR_MALLOC_ALLOCATOR);
    return ctx;
}

void SIRInterpContextDestroy(SIRInterpContext *ctx)
{
    ZoneScoped;

    ctx->stack_usage_stack.destroy();
    ctx->value_addrs.destroy();
    ctx->func_stack.destroy();
    ctx->block_stack.destroy();
    SIRFree(&SIR_MALLOC_ALLOCATOR, ctx->stack_memory);
    SIRFree(&SIR_MALLOC_ALLOCATOR, ctx);
}

static void
SIRInterpContextAbort(SIRInterpContext *ctx, SIRInterpResult err_code)
{
    ctx->err_code = err_code;
    longjmp(ctx->jump_buf, 1);
}

static char *
SIRInterpAllocStackVal(SIRInterpContext *ctx, size_t size, size_t alignment)
{
    ZoneScoped;

    ctx->stack_memory_used = SIR_ROUND_UP(alignment, ctx->stack_memory_used);
    if (ctx->stack_memory_used + size > ctx->stack_memory_size) {
        SIRInterpContextAbort(ctx, SIRInterpResult_StackOverflow);
        return NULL;
    }

    char *addr = ctx->stack_memory + ctx->stack_memory_used;
    ctx->stack_memory_used += size;
    return addr;
}

static char *SIRInterpGetInstAddr(SIRInterpContext *ctx, SIRInstRef inst_ref)
{
    ZoneScoped;

    char *value_addr = NULL;
    SIRInst inst = SIRModuleGetInst(ctx->mod, inst_ref);

    switch (inst.kind) {
    case SIRInstKind_Unknown: SIR_ASSERT(0); break;

    case SIRInstKind_Alias:
    case SIRInstKind_Global:
    case SIRInstKind_StackSlot:
    case SIRInstKind_Block:
    case SIRInstKind_Function:
    case SIRInstKind_FunctionParameter:
    case SIRInstKind_ReturnVoid:
    case SIRInstKind_ReturnValue:
    case SIRInstKind_Load:
    case SIRInstKind_Store:
    case SIRInstKind_Jump:
    case SIRInstKind_Branch:
    case SIRInstKind_Phi:
    case SIRInstKind_FuncCall:
    case SIRInstKind_BitCast:
    case SIRInstKind_ZExt:
    case SIRInstKind_SExt:
    case SIRInstKind_Trunc:
    case SIRInstKind_FPTrunc:
    case SIRInstKind_FPExt:
    case SIRInstKind_SIToFP:
    case SIRInstKind_UIToFP:
    case SIRInstKind_FPToSI:
    case SIRInstKind_FPToUI:
    case SIRInstKind_ArrayElemPtr:
    case SIRInstKind_StructElemPtr:
    case SIRInstKind_ExtractArrayElem:
    case SIRInstKind_ExtractStructElem:
    case SIRInstKind_Binop: {
        value_addr = ctx->value_addrs[inst_ref.id];
        break;
    }

    case SIRInstKind_ConstBool: {
        value_addr = SIRInterpAllocStackVal(ctx, sizeof(bool), alignof(bool));
        *(bool *)value_addr = inst.const_bool.value;
        break;
    }
    case SIRInstKind_ConstInt: {
        SIRType *type = inst.type;
        value_addr = SIRInterpAllocStackVal(
            ctx, SIRTypeSizeOf(ctx->mod, type), SIRTypeAlignOf(ctx->mod, type));
        if (type->int_.is_signed) {
            switch (type->int_.bits) {
            case 8: *((int8_t *)value_addr) = inst.const_int.u64; break;
            case 16: *((int16_t *)value_addr) = inst.const_int.u64; break;
            case 32: *((int32_t *)value_addr) = inst.const_int.u64; break;
            case 64: *((int64_t *)value_addr) = inst.const_int.u64; break;
            default: SIR_ASSERT(0); break;
            }
        } else {
            switch (type->int_.bits) {
            case 8: *((uint8_t *)value_addr) = inst.const_int.u64; break;
            case 16: *((uint16_t *)value_addr) = inst.const_int.u64; break;
            case 32: *((uint32_t *)value_addr) = inst.const_int.u64; break;
            case 64: *((uint64_t *)value_addr) = inst.const_int.u64; break;
            default: SIR_ASSERT(0); break;
            }
        }
        break;
    }
    case SIRInstKind_ConstFloat: {
        SIRType *type = inst.type;
        value_addr = SIRInterpAllocStackVal(
            ctx, SIRTypeSizeOf(ctx->mod, type), SIRTypeAlignOf(ctx->mod, type));
        switch (type->float_.bits) {
        case 32: *((float *)value_addr) = inst.const_float.f64; break;
        case 64: *((double *)value_addr) = inst.const_float.f64; break;
        default: SIR_ASSERT(0); break;
        }
        break;
    }
    }

    return value_addr;
}

bool SIRInterpInst(SIRInterpContext *ctx, SIRInstRef inst_ref)
{
    ZoneScoped;

    SIRModule *mod = ctx->mod;

    SIRInst inst = SIRModuleGetInst(mod, inst_ref);
    char *value_addr = NULL;
    bool returned = false;

    switch (inst.kind) {
    case SIRInstKind_Unknown:
    case SIRInstKind_ConstBool:
    case SIRInstKind_ConstInt:
    case SIRInstKind_ConstFloat: SIR_ASSERT(0); break;

    case SIRInstKind_Function:
    case SIRInstKind_FunctionParameter:
    case SIRInstKind_Block: {
        break;
    }

    case SIRInstKind_Alias: {
        returned = SIRInterpInst(ctx, inst.alias.inst_ref);
        value_addr = SIRInterpGetInstAddr(ctx, inst.alias.inst_ref);
        break;
    }
    case SIRInstKind_Global: {
        // Globals cannot be used in interpreter
        SIRInterpContextAbort(ctx, SIRInterpResult_CannotBeInterpreted);
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
    case SIRInstKind_ReturnVoid: {
        returned = true;
        break;
    }
    case SIRInstKind_ReturnValue: {
        value_addr = SIRInterpGetInstAddr(ctx, inst.return_value.inst_ref);
        returned = true;
        break;
    }
    case SIRInstKind_Load: {
        SIRType *type = inst.type;
        size_t size = SIRTypeSizeOf(ctx->mod, type);
        value_addr =
            SIRInterpAllocStackVal(ctx, size, SIRTypeAlignOf(ctx->mod, type));

        SIRInstRef loaded_inst = inst.load.ptr_ref;
        char *loaded_addr = *(char **)SIRInterpGetInstAddr(ctx, loaded_inst);
        memcpy(value_addr, loaded_addr, size);
        break;
    }
    case SIRInstKind_Store: {
        SIRType *type = SIRModuleGetInstType(ctx->mod, inst.store.value_ref);
        size_t size = SIRTypeSizeOf(ctx->mod, type);
        SIRInstRef stored_inst = inst.store.value_ref;
        SIRInstRef ptr_inst = inst.store.ptr_ref;
        char *addr_to_store = *(char **)SIRInterpGetInstAddr(ctx, ptr_inst);
        memcpy(addr_to_store, SIRInterpGetInstAddr(ctx, stored_inst), size);
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
                            SIRInterpGetInstAddr(ctx, pair.value_ref);
                        break;
                    }
                }
            }
        }

        ctx->block_stack[ctx->block_stack.len - 1] = next_block_ref;
        break;
    }
    case SIRInstKind_Branch: {
        value_addr = SIRInterpGetInstAddr(ctx, inst.branch.cond_inst_ref);

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
                            SIRInterpGetInstAddr(ctx, pair.value_ref);
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
        SIRType *func_ret_type = called_func.func->return_type;

        if (called_func.func->blocks.len == 0) {
            SIRInterpContextAbort(ctx, SIRInterpResult_CannotBeInterpreted);
            break;
        }

        if (called_func.func->variadic) {
            SIRInterpContextAbort(ctx, SIRInterpResult_CannotBeInterpreted);
            break;
        }

        SIR_ASSERT(
            called_func.func->param_insts.len == inst.func_call.params_len);

        for (size_t i = 0; i < called_func.func->param_insts.len; ++i) {
            SIRInstRef passed_param_ref = inst.func_call.params[i];
            SIRInstRef param_ref = called_func.func->param_insts[i];

            ctx->value_addrs[param_ref.id] =
                SIRInterpGetInstAddr(ctx, passed_param_ref);
        }

        SIRInstRef starting_block_ref = called_func.func->blocks[0];

        size_t ret_type_size = SIRTypeSizeOf(ctx->mod, func_ret_type);
        value_addr = SIRInterpAllocStackVal(
            ctx, ret_type_size, SIRTypeAlignOf(ctx->mod, func_ret_type));

        ctx->func_stack.push_back(called_func_ref);
        ctx->block_stack.push_back(starting_block_ref);
        ctx->stack_usage_stack.push_back(ctx->stack_memory_used);

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
                    memcpy(
                        value_addr,
                        ctx->value_addrs[inst_ref.id],
                        ret_type_size);
                    break;
                }
            }
        }

        ctx->stack_memory_used =
            ctx->stack_usage_stack[ctx->stack_usage_stack.len - 1];
        ctx->stack_usage_stack.pop();
        ctx->block_stack.pop();
        ctx->func_stack.pop();

        break;
    }
    case SIRInstKind_BitCast: {
        value_addr = SIRInterpGetInstAddr(ctx, inst.cast.inst_ref);
        break;
    }
    case SIRInstKind_ZExt: {
        SIRType *source_type =
            SIRModuleGetInstType(ctx->mod, inst.cast.inst_ref);
        size_t source_size = SIRTypeSizeOf(ctx->mod, source_type);
        SIRType *dest_type = SIRModuleGetInstType(ctx->mod, inst_ref);
        size_t dest_size = SIRTypeSizeOf(ctx->mod, dest_type);

        SIR_ASSERT(dest_size >= source_size);

        char *source_addr = SIRInterpGetInstAddr(ctx, inst.cast.inst_ref);

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
            SIRModuleGetInstType(ctx->mod, inst.cast.inst_ref);
        size_t source_size = SIRTypeSizeOf(ctx->mod, source_type);
        SIRType *dest_type = SIRModuleGetInstType(ctx->mod, inst_ref);
        size_t dest_size = SIRTypeSizeOf(ctx->mod, dest_type);

        SIR_ASSERT(dest_size >= source_size);

        char *source_addr = SIRInterpGetInstAddr(ctx, inst.cast.inst_ref);

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
            SIRModuleGetInstType(ctx->mod, inst.cast.inst_ref);
        size_t source_size = SIRTypeSizeOf(ctx->mod, source_type);
        SIRType *dest_type = SIRModuleGetInstType(ctx->mod, inst_ref);
        size_t dest_size = SIRTypeSizeOf(ctx->mod, dest_type);

        SIR_ASSERT(dest_size <= source_size);

        char *source_addr = SIRInterpGetInstAddr(ctx, inst.cast.inst_ref);

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
    case SIRInstKind_FPTrunc: {
        SIRType *source_type =
            SIRModuleGetInstType(ctx->mod, inst.cast.inst_ref);
        size_t source_size = SIRTypeSizeOf(ctx->mod, source_type);
        SIRType *dest_type = SIRModuleGetInstType(ctx->mod, inst_ref);
        size_t dest_size = SIRTypeSizeOf(ctx->mod, dest_type);

        SIR_ASSERT(dest_size <= source_size);

        char *source_addr = SIRInterpGetInstAddr(ctx, inst.cast.inst_ref);

        value_addr = SIRInterpAllocStackVal(
            ctx, dest_size, SIRTypeAlignOf(ctx->mod, dest_type));

        *(float *)value_addr = *(float *)source_addr;

        break;
    }
    case SIRInstKind_FPExt: {
        SIRType *source_type =
            SIRModuleGetInstType(ctx->mod, inst.cast.inst_ref);
        size_t source_size = SIRTypeSizeOf(ctx->mod, source_type);
        SIRType *dest_type = SIRModuleGetInstType(ctx->mod, inst_ref);
        size_t dest_size = SIRTypeSizeOf(ctx->mod, dest_type);

        SIR_ASSERT(dest_size >= source_size);

        char *source_addr = SIRInterpGetInstAddr(ctx, inst.cast.inst_ref);

        value_addr = SIRInterpAllocStackVal(
            ctx, dest_size, SIRTypeAlignOf(ctx->mod, dest_type));

        *(double *)value_addr = *(double *)source_addr;

        break;
    }
    case SIRInstKind_SIToFP: {
        SIRType *source_type =
            SIRModuleGetInstType(ctx->mod, inst.cast.inst_ref);
        size_t source_size = SIRTypeSizeOf(ctx->mod, source_type);
        SIRType *dest_type = SIRModuleGetInstType(ctx->mod, inst_ref);
        size_t dest_size = SIRTypeSizeOf(ctx->mod, dest_type);

        SIR_ASSERT(dest_size <= source_size);

        char *source_addr = SIRInterpGetInstAddr(ctx, inst.cast.inst_ref);

        value_addr = SIRInterpAllocStackVal(
            ctx, dest_size, SIRTypeAlignOf(ctx->mod, dest_type));

        int64_t value = 0;
        switch (source_size) {
        case 1: value = *(int8_t *)source_addr; break;
        case 2: value = *(int16_t *)source_addr; break;
        case 4: value = *(int32_t *)source_addr; break;
        case 8: value = *(int64_t *)source_addr; break;
        default: SIR_ASSERT(0);
        }

        switch (dest_size) {
        case 4: *(float *)value_addr = value; break;
        case 8: *(double *)value_addr = value; break;
        default: SIR_ASSERT(0);
        }
        break;
    }
    case SIRInstKind_UIToFP: {
        SIRType *source_type =
            SIRModuleGetInstType(ctx->mod, inst.cast.inst_ref);
        size_t source_size = SIRTypeSizeOf(ctx->mod, source_type);
        SIRType *dest_type = SIRModuleGetInstType(ctx->mod, inst_ref);
        size_t dest_size = SIRTypeSizeOf(ctx->mod, dest_type);

        char *source_addr = SIRInterpGetInstAddr(ctx, inst.cast.inst_ref);

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
        case 4: *(float *)value_addr = value; break;
        case 8: *(double *)value_addr = value; break;
        default: SIR_ASSERT(0);
        }
        break;
    }
    case SIRInstKind_FPToSI: {
        SIRType *source_type =
            SIRModuleGetInstType(ctx->mod, inst.cast.inst_ref);
        size_t source_size = SIRTypeSizeOf(ctx->mod, source_type);
        SIRType *dest_type = SIRModuleGetInstType(ctx->mod, inst_ref);
        size_t dest_size = SIRTypeSizeOf(ctx->mod, dest_type);

        char *source_addr = SIRInterpGetInstAddr(ctx, inst.cast.inst_ref);

        value_addr = SIRInterpAllocStackVal(
            ctx, dest_size, SIRTypeAlignOf(ctx->mod, dest_type));

        double value = 0;
        switch (source_size) {
        case 4: value = *(float *)source_addr; break;
        case 8: value = *(double *)source_addr; break;
        default: SIR_ASSERT(0);
        }

        switch (dest_size) {
        case 1: *(int8_t *)value_addr = value; break;
        case 2: *(int16_t *)value_addr = value; break;
        case 4: *(int32_t *)value_addr = value; break;
        case 8: *(int64_t *)value_addr = value; break;
        default: SIR_ASSERT(0);
        }
        break;
    }
    case SIRInstKind_FPToUI: {
        SIRType *source_type =
            SIRModuleGetInstType(ctx->mod, inst.cast.inst_ref);
        size_t source_size = SIRTypeSizeOf(ctx->mod, source_type);
        SIRType *dest_type = SIRModuleGetInstType(ctx->mod, inst_ref);
        size_t dest_size = SIRTypeSizeOf(ctx->mod, dest_type);

        char *source_addr = SIRInterpGetInstAddr(ctx, inst.cast.inst_ref);

        value_addr = SIRInterpAllocStackVal(
            ctx, dest_size, SIRTypeAlignOf(ctx->mod, dest_type));

        double value = 0;
        switch (source_size) {
        case 4: value = *(float *)source_addr; break;
        case 8: value = *(double *)source_addr; break;
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
        SIRType *index_type =
            SIRModuleGetInstType(ctx->mod, inst.array_elem_ptr.index_ref);
        SIRType *array_ptr_type =
            SIRModuleGetInstType(ctx->mod, inst.array_elem_ptr.accessed_ref);
        SIRType *array_type = array_ptr_type->pointer.sub;

        SIR_ASSERT(array_ptr_type->kind == SIRTypeKind_Pointer);
        SIR_ASSERT(array_type->kind == SIRTypeKind_Array);
        SIR_ASSERT(index_type->kind == SIRTypeKind_Int);

        char *array_addr = *(char **)SIRInterpGetInstAddr(
            ctx, inst.array_elem_ptr.accessed_ref);

        char *index_addr =
            *(char **)SIRInterpGetInstAddr(ctx, inst.array_elem_ptr.index_ref);

        uint64_t index = 0;

        switch (index_type->int_.bits) {
        case 8: index = *(uint8_t *)index_addr; break;
        case 16: index = *(uint16_t *)index_addr; break;
        case 32: index = *(uint32_t *)index_addr; break;
        case 64: index = *(uint64_t *)index_addr; break;
        }

        char *elem_addr =
            array_addr + index * SIRTypeSizeOf(ctx->mod, array_type->array.sub);

        value_addr =
            SIRInterpAllocStackVal(ctx, sizeof(char *), alignof(char *));
        *(char **)value_addr = elem_addr;
        break;
    }
    case SIRInstKind_StructElemPtr: {
        SIRType *struct_ptr_type =
            SIRModuleGetInstType(ctx->mod, inst.struct_elem_ptr.accessed_ref);
        SIRType *struct_type = struct_ptr_type->pointer.sub;

        SIR_ASSERT(struct_ptr_type->kind == SIRTypeKind_Pointer);
        SIR_ASSERT(struct_type->kind == SIRTypeKind_Struct);

        char *struct_addr = *(char **)SIRInterpGetInstAddr(
            ctx, inst.struct_elem_ptr.accessed_ref);

        uint32_t field_offset = SIRTypeStructOffsetOf(
            ctx->mod, struct_type, inst.struct_elem_ptr.field_index);

        char *field_addr = struct_addr + field_offset;

        value_addr =
            SIRInterpAllocStackVal(ctx, sizeof(char *), alignof(char *));
        *(char **)value_addr = field_addr;
        break;
    }
    case SIRInstKind_ExtractArrayElem: {
        SIRType *array_type = SIRModuleGetInstType(
            ctx->mod, inst.extract_array_elem.accessed_ref);

        SIR_ASSERT(array_type->kind == SIRTypeKind_Array);

        char *array_addr =
            SIRInterpGetInstAddr(ctx, inst.extract_array_elem.accessed_ref);
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
            SIRInterpGetInstAddr(ctx, inst.extract_struct_elem.accessed_ref);

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

        char *left_addr = SIRInterpGetInstAddr(ctx, inst.binop.left_ref);
        char *right_addr = SIRInterpGetInstAddr(ctx, inst.binop.right_ref);

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

        case SIRBinaryOperation_IAdd: SIR_INTERP_UINT_BINOP(+); break;
        case SIRBinaryOperation_ISub: SIR_INTERP_UINT_BINOP(-); break;
        case SIRBinaryOperation_IMul: SIR_INTERP_UINT_BINOP(*); break;
        case SIRBinaryOperation_UDiv: SIR_INTERP_UINT_BINOP(/); break;
        case SIRBinaryOperation_SDiv: SIR_INTERP_INT_BINOP(/); break;
        case SIRBinaryOperation_URem: SIR_INTERP_UINT_BINOP(%); break;
        case SIRBinaryOperation_SRem: SIR_INTERP_INT_BINOP(%); break;
        case SIRBinaryOperation_FAdd: SIR_INTERP_FLOAT_BINOP(+); break;
        case SIRBinaryOperation_FSub: SIR_INTERP_FLOAT_BINOP(-); break;
        case SIRBinaryOperation_FMul: SIR_INTERP_FLOAT_BINOP(*); break;
        case SIRBinaryOperation_FDiv: SIR_INTERP_FLOAT_BINOP(/); break;
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

SIRInterpResult
SIRInterpFunction(SIRInterpContext *ctx, SIRInstRef func_ref, void *result)
{
    ZoneScoped;

    SIRModule *mod = ctx->mod;

    ctx->stack_memory_used = 0;
    ctx->value_addrs.resize(ctx->mod->insts.len);
    ctx->func_stack.resize(0);
    ctx->block_stack.resize(0);

    if (setjmp(ctx->jump_buf)) {
        return ctx->err_code;
    }

    SIRInst func = SIRModuleGetInst(mod, func_ref);
    SIRType *func_ret_type = func.func->return_type;
    SIR_ASSERT(func.kind == SIRInstKind_Function);

    SIRInstRef starting_block_ref = func.func->blocks[0];

    size_t ret_type_size = SIRTypeSizeOf(ctx->mod, func_ret_type);
    char *ret_addr = SIRInterpAllocStackVal(
        ctx, ret_type_size, SIRTypeAlignOf(ctx->mod, func_ret_type));

    ctx->func_stack.push_back(func_ref);
    ctx->block_stack.push_back(starting_block_ref);
    ctx->stack_usage_stack.push_back(ctx->stack_memory_used);

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
                memcpy(ret_addr, ctx->value_addrs[inst_ref.id], ret_type_size);
                break;
            }
        }
    }

    ctx->stack_memory_used =
        ctx->stack_usage_stack[ctx->stack_usage_stack.len - 1];
    ctx->stack_usage_stack.pop();
    ctx->block_stack.pop();
    ctx->func_stack.pop();

    memcpy(result, ret_addr, SIRTypeSizeOf(ctx->mod, func_ret_type));

    return SIRInterpResult_Success;
}
