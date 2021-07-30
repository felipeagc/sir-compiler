#include "sir_type.hpp"
#include "sir_ir.hpp"

SIRString SIRType::to_string(SIRModule *module)
{
    if (this->str.len > 0) {
        return this->str;
    }

    switch (this->kind) {
    case SIRTypeKind_Void: {
        this->str = "@void";
        break;
    }
    case SIRTypeKind_Bool: {
        this->str = "@bool";
        break;
    }
    case SIRTypeKind_Pointer: {
        SIRString sub_string = this->pointer.sub->to_string(module);
        this->str = module->arena->sprintf(
            "@ptr(%.*s)", (int)sub_string.len, sub_string.ptr);
        break;
    }
    case SIRTypeKind_Int: {
        this->str = module->arena->sprintf("@int(%u)", this->int_.bits);
        break;
    }
    case SIRTypeKind_Float: {
        this->str = module->arena->sprintf("@float(%u)", this->float_.bits);
        break;
    }
    case SIRTypeKind_Array: {
        SIRString sub_string = this->array.sub->to_string(module);
        this->str = module->arena->sprintf(
            "@array(%lu, %.*s)",
            this->array.count,
            (int)sub_string.len,
            sub_string.ptr);
        break;
    }
    case SIRTypeKind_Struct: {
        SIRStringBuilder sb =
            SIRStringBuilder::create(SIRMallocAllocator::get_instance());

        if (this->struct_.packed) {
            sb.append("@packed_struct(");
        } else {
            sb.append("@struct(");
        }

        bool first = true;
        for (auto &field_type : this->struct_.fields) {
            if (!first) {
                sb.append(", ");
            }
            SIRString field_str = field_type->to_string(module);
            sb.append(field_str);
            first = false;
        }

        sb.append(")");

        this->str = sb.build_null_terminated(module->arena);

        sb.destroy();
        break;
    }
    }

    return this->str;
}

uint32_t SIRType::size_of(SIRModule *module)
{
    if (this->size > 0) return this->size;

    switch (this->kind) {
    case SIRTypeKind_Void: this->size = 1; break;
    case SIRTypeKind_Bool: this->size = 1; break;
    case SIRTypeKind_Int: this->size = this->int_.bits / 8; break;
    case SIRTypeKind_Float: this->size = this->float_.bits / 8; break;
    case SIRTypeKind_Pointer: {
        switch (module->target_arch) {
        case SIRTargetArch_X86_64: this->size = 8; break;
        }
        break;
    }
    case SIRTypeKind_Array: {
        uint32_t elem_size = this->array.sub->size_of(module);
        uint32_t elem_alignment = this->array.sub->align_of(module);
        uint32_t stride = SIR_ROUND_UP(elem_alignment, elem_size);
        this->size = stride * this->array.count;
        break;
    }
    case SIRTypeKind_Struct: {
        this->size = 0;

        for (auto &field_type : this->struct_.fields) {
            uint32_t field_align = field_type->align_of(module);
            this->size = SIR_ROUND_UP(field_align, this->size); // Add padding

            uint32_t field_size = field_type->size_of(module);
            this->size += field_size;
        }

        break;
    }
    }

    uint32_t self_alignment = this->align_of(module);
    this->size =
        SIR_ROUND_UP(self_alignment, this->size); // Round size up for alignment

    return this->size;
}

uint32_t SIRType::align_of(SIRModule *module)
{
    if (this->alignment > 0) return this->alignment;

    switch (this->kind) {
    case SIRTypeKind_Void: this->alignment = 1; break;
    case SIRTypeKind_Bool: this->alignment = 1; break;
    case SIRTypeKind_Int: this->alignment = this->int_.bits / 8; break;
    case SIRTypeKind_Float: this->alignment = this->float_.bits / 8; break;
    case SIRTypeKind_Pointer: {
        switch (module->target_arch) {
        case SIRTargetArch_X86_64: this->alignment = 8; break;
        }
        break;
    }
    case SIRTypeKind_Array: {
        this->alignment = this->array.sub->align_of(module);
        break;
    }
    case SIRTypeKind_Struct: {
        this->alignment = 0;

        for (auto &field_type : this->struct_.fields) {
            uint32_t field_align = field_type->align_of(module);
            if (field_align > this->alignment) {
                this->alignment = field_align;
            }
        }

        break;
    }
    }

    return this->alignment;
}