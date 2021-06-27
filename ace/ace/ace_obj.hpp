#pragma once

#include "ace_base.hpp"
#include "ace_ir.hpp"

namespace ace {

enum SymbolType {
    SymbolType_None,
    SymbolType_Function,
    SymbolType_Section,
};

enum SectionType {
    SectionType_None,
    SectionType_Text,
    SectionType_Data,
    SectionType_BSS,
    SectionType_ROData,
};

struct SymbolRef {
    uint32_t id;
};

struct ObjectBuilder {
    virtual void add_to_section(SectionType type, Slice<uint8_t> data) = 0;
    virtual void set_section_data(SectionType type, size_t offset, Slice<uint8_t> data) = 0;
    virtual size_t get_section_size(SectionType type) = 0;

    virtual void add_data_relocation(
        SectionType source_section,
        int64_t source_data_offset,
        uint64_t destination_offset,
        size_t dest_addr_size) = 0;

    virtual void add_procedure_relocation(
        SymbolRef function_symbol,
        uint64_t destination_offset,
        size_t dest_addr_size) = 0;

    virtual SymbolRef add_symbol(
        String name,
        SectionType section_type,
        SymbolType type,
        Linkage linkage) = 0;
    virtual void
    set_symbol_region(SymbolRef symbol_ref, size_t offset, size_t size) = 0;

    virtual bool output_to_file(String path) = 0;
    virtual void destroy() = 0;
};

struct AsmBuilder {
    virtual void generate() = 0;
    virtual void destroy() = 0;
};

ObjectBuilder *create_elf64_builder(Module *module);

AsmBuilder *create_x86_64_builder(Module *module, ObjectBuilder *obj_builder);

} // namespace ace
