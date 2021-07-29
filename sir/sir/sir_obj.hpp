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
    virtual void add_to_section(SIRSectionType type, SIRSlice<uint8_t> data) = 0;
    virtual void set_section_data(SIRSectionType type, size_t offset, SIRSlice<uint8_t> data) = 0;
    virtual size_t get_section_size(SIRSectionType type) = 0;

    virtual void add_data_relocation(
        SIRSectionType source_section,
        int64_t source_data_offset,
        uint64_t destination_offset,
        size_t dest_addr_size) = 0;

    virtual void add_procedure_relocation(
        SIRSymbolRef function_symbol,
        uint64_t destination_offset,
        size_t dest_addr_size) = 0;

    virtual SIRSymbolRef add_symbol(
        SIRString name,
        SIRSectionType section_type,
        SIRSymbolType type,
        SIRLinkage linkage) = 0;
    virtual void
    set_symbol_region(SIRSymbolRef symbol_ref, size_t offset, size_t size) = 0;

    virtual bool output_to_file(SIRString path) = 0;
    virtual void destroy() = 0;
};

struct SIRAsmBuilder {
    virtual void generate() = 0;
    virtual void destroy() = 0;
};

SIRObjectBuilder *SIRCreateELF64Bbuilder(SIRModule *module);

SIRAsmBuilder *SIRCreateX86_64Builder(SIRModule *module, SIRObjectBuilder *obj_builder);
