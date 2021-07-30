#pragma once

#include "sir_base.hpp"
#include "sir_ir.hpp"

enum SIRSymbolType : uint8_t {
    SIRSymbolType_None,
    SIRSymbolType_Function,
    SIRSymbolType_Section,
};

enum SIRSectionType : uint8_t {
    SIRSectionType_None,
    SIRSectionType_Text,
    SIRSectionType_Data,
    SIRSectionType_BSS,
    SIRSectionType_ROData,
};

struct SIRSymbolRef {
    uint32_t id;
};

struct SIRObjectBuilder {
    void (*add_to_section)(
        SIRObjectBuilder *builder, SIRSectionType type, SIRSlice<uint8_t> data);
    void (*set_section_data)(
        SIRObjectBuilder *builder,
        SIRSectionType type,
        size_t offset,
        SIRSlice<uint8_t> data);
    size_t (*get_section_size)(SIRObjectBuilder *builder, SIRSectionType type);

    void (*add_data_relocation)(
        SIRObjectBuilder *builder,
        SIRSectionType source_section,
        int64_t source_data_offset,
        uint64_t destination_offset,
        size_t dest_addr_size);

    void (*add_procedure_relocation)(
        SIRObjectBuilder *builder,
        SIRSymbolRef function_symbol,
        uint64_t destination_offset,
        size_t dest_addr_size);

    SIRSymbolRef (*add_symbol)(
        SIRObjectBuilder *builder,
        SIRString name,
        SIRSectionType section_type,
        SIRSymbolType type,
        SIRLinkage linkage);
    void (*set_symbol_region)(
        SIRObjectBuilder *builder,
        SIRSymbolRef symbol_ref,
        size_t offset,
        size_t size);

    bool (*output_to_file)(SIRObjectBuilder *builder, SIRString path);
    void (*destroy)(SIRObjectBuilder *builder);
};

struct SIRAsmBuilder {
    void (*generate)(SIRAsmBuilder *asm_builder);
    void (*destroy)(SIRAsmBuilder *asm_builder);
};

SIRObjectBuilder *SIRCreateELF64Bbuilder(SIRModule *module);

SIRAsmBuilder *
SIRCreateX86_64Builder(SIRModule *module, SIRObjectBuilder *obj_builder);
