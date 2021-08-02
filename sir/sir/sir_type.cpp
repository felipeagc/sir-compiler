#include "sir_type.hpp"
#include "sir_ir.hpp"

SIRString SIRTypeToString(SIRModule *module, SIRType *type)
{
    if (type->str.len > 0) {
        return type->str;
    }

    switch (type->kind) {
    case SIRTypeKind_Void: {
        type->str = SIR_STR("@void");
        break;
    }
    case SIRTypeKind_Bool: {
        type->str = SIR_STR("@bool");
        break;
    }
    case SIRTypeKind_Pointer: {
        SIRString sub_string = SIRTypeToString(module, type->pointer.sub);
        type->str = SIRAllocSprintf(
            module->arena, "@ptr(%.*s)", (int)sub_string.len, sub_string.ptr);
        break;
    }
    case SIRTypeKind_Int: {
        type->str = SIRAllocSprintf(module->arena, "@int(%u)", type->int_.bits);
        break;
    }
    case SIRTypeKind_Float: {
        type->str =
            SIRAllocSprintf(module->arena, "@float(%u)", type->float_.bits);
        break;
    }
    case SIRTypeKind_Array: {
        SIRString sub_string = SIRTypeToString(module, type->array.sub);
        type->str = SIRAllocSprintf(
            module->arena,
            "@array(%lu, %.*s)",
            type->array.count,
            (int)sub_string.len,
            sub_string.ptr);
        break;
    }
    case SIRTypeKind_Struct: {
        SIRStringBuilder sb = SIRStringBuilder::create(&SIR_MALLOC_ALLOCATOR);

        if (type->struct_.packed) {
            sb.append(SIR_STR("@packed_struct("));
        } else {
            sb.append(SIR_STR("@struct("));
        }

        for (size_t i = 0; i < type->struct_.fields_len; ++i) {
            SIRType *field_type = type->struct_.fields[i];
            if (i == 0) {
                sb.append(SIR_STR(", "));
            }
            SIRString field_str = SIRTypeToString(module, field_type);
            sb.append(field_str);
        }

        sb.append(SIR_STR(")"));

        type->str = sb.build_null_terminated((SIRAllocator *)module->arena);

        sb.destroy();
        break;
    }
    }

    return type->str;
}

uint32_t SIRTypeSizeOf(SIRModule *module, SIRType *type)
{
    if (type->size > 0) return type->size;

    switch (type->kind) {
    case SIRTypeKind_Void: type->size = 1; break;
    case SIRTypeKind_Bool: type->size = 1; break;
    case SIRTypeKind_Int: type->size = type->int_.bits / 8; break;
    case SIRTypeKind_Float: type->size = type->float_.bits / 8; break;
    case SIRTypeKind_Pointer: {
        switch (module->target_arch) {
        case SIRTargetArch_X86_64: type->size = 8; break;
        }
        break;
    }
    case SIRTypeKind_Array: {
        uint32_t elem_size = SIRTypeSizeOf(module, type->array.sub);
        uint32_t elem_alignment = SIRTypeAlignOf(module, type->array.sub);
        uint32_t stride = SIR_ROUND_UP(elem_alignment, elem_size);
        type->size = stride * type->array.count;
        break;
    }
    case SIRTypeKind_Struct: {
        type->size = 0;

        for (size_t i = 0; i < type->struct_.fields_len; ++i) {
            SIRType *field_type = type->struct_.fields[i];
            uint32_t field_align = SIRTypeAlignOf(module, field_type);
            type->size = SIR_ROUND_UP(field_align, type->size); // Add padding

            uint32_t field_size = SIRTypeSizeOf(module, field_type);
            type->size += field_size;
        }

        break;
    }
    }

    uint32_t self_alignment = SIRTypeAlignOf(module, type);
    type->size =
        SIR_ROUND_UP(self_alignment, type->size); // Round size up for alignment

    return type->size;
}

uint32_t SIRTypeAlignOf(SIRModule *module, SIRType *type)
{
    if (type->alignment > 0) return type->alignment;

    switch (type->kind) {
    case SIRTypeKind_Void: type->alignment = 1; break;
    case SIRTypeKind_Bool: type->alignment = 1; break;
    case SIRTypeKind_Int: type->alignment = type->int_.bits / 8; break;
    case SIRTypeKind_Float: type->alignment = type->float_.bits / 8; break;
    case SIRTypeKind_Pointer: {
        switch (module->target_arch) {
        case SIRTargetArch_X86_64: type->alignment = 8; break;
        }
        break;
    }
    case SIRTypeKind_Array: {
        type->alignment = SIRTypeAlignOf(module, type->array.sub);
        break;
    }
    case SIRTypeKind_Struct: {
        type->alignment = 0;

        for (size_t i = 0; i < type->struct_.fields_len; ++i) {
            SIRType *field_type = type->struct_.fields[i];
            uint32_t field_align = SIRTypeAlignOf(module, field_type);
            if (field_align > type->alignment) {
                type->alignment = field_align;
            }
        }

        break;
    }
    }

    return type->alignment;
}
