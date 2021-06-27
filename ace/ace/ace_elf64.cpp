#include "ace_obj.hpp"
#include <Tracy.hpp>

namespace ace {

#define ACE_ELF64_ST_BIND(info) ((info) >> 4)
#define ACE_ELF64_ST_TYPE(info) ((info)&0xf)
#define ACE_ELF64_ST_INFO(bind, type) (((bind) << 4) + ((type)&0xf))

#define ACE_ELF64_R_SYM(i) ((i) >> 32)
#define ACE_ELF64_R_TYPE(i) ((i)&0xffffffffL)
#define ACE_ELF64_R_INFO(s, t) (((s) << 32) + ((t)&0xffffffffL))

enum Elf64SectionType : uint32_t {
    Elf64SectionType_Null = 0x0,
    Elf64SectionType_ProgBits = 0x1,
    Elf64SectionType_SymTab = 0x2,
    Elf64SectionType_StrTab = 0x3,
    Elf64SectionType_Rela = 0x4,
    Elf64SectionType_Hash = 0x5,
    Elf64SectionType_Dynamic = 0x6,
    Elf64SectionType_Note = 0x7,
    Elf64SectionType_NoBits = 0x8,
    Elf64SectionType_Rel = 0x9,
    Elf64SectionType_ShLib = 0x0A,
    Elf64SectionType_DynSym = 0x0B,
    Elf64SectionType_InitArray = 0x0E,
    Elf64SectionType_FiniArray = 0x0F,
    Elf64SectionType_PreinitArray = 0x10,
    Elf64SectionType_Group = 0x11,
    Elf64SectionType_SymTabShndx = 0x12,
    Elf64SectionType_Num = 0x13,
};

enum Elf64SectionFlags : uint32_t {
    Elf64SectionFlags_Write = 0x1,
    Elf64SectionFlags_Alloc = 0x2,
    Elf64SectionFlags_ExecInstr = 0x4,
    Elf64SectionFlags_Merge = 0x10,
    Elf64SectionFlags_Strings = 0x20,
    Elf64SectionFlags_InfoLink = 0x40,
    Elf64SectionFlags_LinkOrder = 0x40,
    Elf64SectionFlags_TLS = 0x400,
};

struct Elf64Header {
    uint8_t e_ident[16];
    uint16_t e_type;
    uint16_t e_machine;
    uint32_t e_version;
    uint64_t e_entry;
    uint64_t e_phoff;
    uint64_t e_shoff;
    uint32_t e_flags;
    uint16_t e_ehsize;
    uint16_t e_phentsize;
    uint16_t e_phnum;
    uint16_t e_shentsize;
    uint16_t e_shnum;
    uint16_t e_shstrndx;
};

static_assert(sizeof(Elf64Header) == 0x40, "Elf64Header size invalid");

struct Elf64SectionHeader {
    uint32_t sh_name = 0;
    uint32_t sh_type = 0;
    uint64_t sh_flags = 0;
    uint64_t sh_addr = 0;
    uint64_t sh_offset = 0;
    uint64_t sh_size = 0;
    uint32_t sh_link = 0;
    uint32_t sh_info = 0;
    uint64_t sh_addralign = 0;
    uint64_t sh_entsize = 0;
};

static_assert(
    sizeof(Elf64SectionHeader) == 0x40, "Elf64SectionHeader size invalid");

struct Elf64Symbol {
    uint32_t st_name = 0;
    uint8_t st_info = 0;
    uint8_t st_other = 0;
    uint16_t st_shndx = 0;
    uint64_t st_value = 0;
    uint64_t st_size = 0;
};

static_assert(sizeof(Elf64Symbol) == 24, "incorrect size of Elf64Symbol");

struct Elf64Rela {
    uint64_t r_offset;
    uint64_t r_info;
    int64_t r_addend;
};

static_assert(sizeof(Elf64Rela) == 24, "incorrect size of Elf64Rela");

enum Elf64SymbolBinding : uint8_t {
    Elf64SymbolBinding_Local = 0,
    Elf64SymbolBinding_Global = 1,
    Elf64SymbolBinding_Weak = 2,
};

enum Elf64SymbolType : uint8_t {
    Elf64SymbolType_NoType = 0,
    Elf64SymbolType_Object = 1,
    Elf64SymbolType_Func = 2,
    Elf64SymbolType_Section = 3,
    Elf64SymbolType_File = 4,
    Elf64SymbolType_Common = 5,
    Elf64SymbolType_TLS = 6,
};

enum Elf64RelaTypeX86_64 : uint32_t {
    Elf64RelaTypeX86_64_PC32 = 2,
    Elf64RelaTypeX86_64_PLT32 = 4,
    Elf64RelaTypeX86_64_PC64 = 24,
};

struct Section {
    Elf64SectionHeader header;
    Array<uint8_t> data;
};

struct Symbol {
    SymbolType type;
    SectionType section_type;
    Linkage linkage;
    String name;
    size_t offset = 0;
    size_t size = 0;
    size_t final_index = 0;
};

struct Elf64Builder : ObjectBuilder {
    Module *module;
    Elf64Header header;
    Array<Section> sections;
    Array<Symbol> symbols;
    uint16_t null_index = 0;     // Null section
    uint16_t shstrtab_index = 0; // String table
    uint16_t strtab_index = 0;   // String table
    uint16_t symtab_index = 0;
    uint16_t rela_text_index = 0; // Code
    uint16_t text_index = 0;      // Code
    uint16_t rodata_index = 0;    // Initialized data RO
    uint16_t data_index = 0;      // Initalizad data RW
    uint16_t bss_index = 0;       // Uninitialized data RW

    SymbolRef text_symbol_ref = {0};
    SymbolRef data_symbol_ref = {0};
    SymbolRef bss_symbol_ref = {0};
    SymbolRef rodata_symbol_ref = {0};

    uint64_t output_symbol_count = 0;

    virtual void add_to_section(SectionType, Slice<uint8_t> data) override;
    virtual void set_section_data(
        SectionType type, size_t offset, Slice<uint8_t> data) override;
    virtual size_t get_section_size(SectionType type) override;
    virtual void add_data_relocation(
        SectionType source_section,
        int64_t source_data_offset,
        uint64_t destination_offset,
        size_t dest_addr_size) override;
    virtual void add_procedure_relocation(
        SymbolRef function_symbol,
        uint64_t destination_offset,
        size_t dest_addr_size) override;
    virtual SymbolRef add_symbol(
        String name,
        SectionType section_type,
        SymbolType type,
        Linkage linkage) override;
    virtual void set_symbol_region(
        SymbolRef symbol_ref, size_t offset, size_t size) override;

    virtual bool output_to_file(String path) override;
    virtual void destroy() override;
};

static uint32_t
elf_add_string(Elf64Builder *builder, uint16_t string_section_index, String str)
{
    ZoneScoped;

    ACE_ASSERT(string_section_index < builder->sections.len);
    Section *str_section = &builder->sections[string_section_index];
    ACE_ASSERT(str_section->header.sh_type == Elf64SectionType_StrTab);

    uint32_t string_offset = (uint32_t)str_section->data.len;
    str_section->data.push_many({(uint8_t *)str.ptr, str.len});
    str_section->data.push_back(0); // Null terminate

    return string_offset;
}

static void elf_init_section(
    Elf64Builder *builder,
    uint16_t section_index,
    String name,
    Elf64SectionHeader header_template)
{
    ZoneScoped;

    ACE_ASSERT(section_index < builder->sections.len);
    Section *section = &builder->sections[section_index];
    *section = {
        header_template,
        Array<uint8_t>::create(MallocAllocator::get_instance()),
    };

    if (section->header.sh_type == Elf64SectionType_StrTab) {
        section->data.push_back(0);
    }

    if (section->header.sh_type != Elf64SectionType_Null) {
        section->data.reserve(1 << 20);
        section->header.sh_name =
            elf_add_string(builder, builder->shstrtab_index, name);
    }
}

static void elf_output_symbol(Elf64Builder *builder, size_t symbol_index)
{
    ZoneScoped;

    Symbol *symbol = &builder->symbols[symbol_index];
    uint16_t section_index = 0;
    switch (symbol->section_type) {
    case SectionType_None: section_index = builder->null_index; break;
    case SectionType_Text: section_index = builder->text_index; break;
    case SectionType_Data: section_index = builder->data_index; break;
    case SectionType_BSS: section_index = builder->bss_index; break;
    case SectionType_ROData: section_index = builder->rodata_index; break;
    }

    Elf64SymbolBinding binding;
    Elf64SymbolType type;

    switch (symbol->linkage) {
    case Linkage_Internal: binding = Elf64SymbolBinding_Local; break;
    case Linkage_External: binding = Elf64SymbolBinding_Global; break;
    }

    switch (symbol->type) {
    case SymbolType_None: type = Elf64SymbolType_NoType; break;
    case SymbolType_Section: type = Elf64SymbolType_Section; break;
    case SymbolType_Function: type = Elf64SymbolType_Func; break;
    }

    Section *symtab = &builder->sections[builder->symtab_index];

    Elf64Symbol elf_symbol{};
    if (symtab->data.len > 0) {
        if (symbol->name.len > 0) {
            elf_symbol.st_name =
                elf_add_string(builder, builder->strtab_index, symbol->name);
        }
        elf_symbol.st_value = symbol->offset;
        elf_symbol.st_size = symbol->size;
        elf_symbol.st_shndx = section_index;
        elf_symbol.st_info = ACE_ELF64_ST_INFO(binding, type);
    }

    symtab->data.push_many({(uint8_t *)&elf_symbol, sizeof(Elf64Symbol)});

    symbol->final_index = builder->output_symbol_count;
    builder->output_symbol_count++;
}

ObjectBuilder *create_elf64_builder(Module *module)
{
    ZoneScoped;

    Elf64Builder *builder = module->arena->alloc_init<Elf64Builder>();
    builder->module = module;
    builder->header = Elf64Header{};
    builder->sections = Array<Section>::create(MallocAllocator::get_instance());
    builder->symbols = Array<Symbol>::create(MallocAllocator::get_instance());

    // Zero out the header
    memset(&builder->header, 0, sizeof(builder->header));

    // Magic number
    builder->header.e_ident[0] = 0x7f;
    builder->header.e_ident[1] = 0x45;
    builder->header.e_ident[2] = 0x4c;
    builder->header.e_ident[3] = 0x46;

    builder->header.e_ident[4] = 2; // Little endian

    switch (module->endianness) {
    case Endianness_LittleEndian: {
        builder->header.e_ident[5] = 1;
        break;
    }
    case Endianness_BigEndian: {
        builder->header.e_ident[5] = 2;
        break;
    }
    }

    builder->header.e_ident[6] = 1;   // ELF version
    builder->header.e_ident[7] = 0x0; // ABI: System V
    builder->header.e_ident[8] = 0;   // ABI version

    builder->header.e_type = 0x01; // Type: relocatable

    switch (module->target_arch) {
    case TargetArch_X86_64: {
        builder->header.e_machine = 0x3E; // Arch: x86-64
        break;
    }
    }

    {
        uint16_t section_count = 0;
        builder->null_index = section_count++;
        builder->text_index = section_count++;
        builder->rela_text_index = section_count++;
        builder->data_index = section_count++;
        builder->bss_index = section_count++;
        builder->rodata_index = section_count++;
        builder->symtab_index = section_count++;
        builder->strtab_index = section_count++;
        builder->shstrtab_index = section_count++;

        builder->sections.resize(section_count);
    }

    builder->header.e_version = 1; // Version
    builder->header.e_entry = 0;
    builder->header.e_phoff = 0;
    builder->header.e_shoff = 0; // To be filled later
    builder->header.e_flags = 0;
    builder->header.e_ehsize = sizeof(Elf64Header);
    builder->header.e_phentsize = 0;
    builder->header.e_phnum = 0;
    builder->header.e_shentsize = sizeof(Elf64SectionHeader);
    builder->header.e_shnum = 0; // To be filled later
    builder->header.e_shstrndx = builder->shstrtab_index;

    elf_init_section(
        builder, builder->null_index, "", {.sh_type = Elf64SectionType_Null});

    elf_init_section(
        builder,
        builder->shstrtab_index,
        ".shstrtab",
        {
            .sh_type = Elf64SectionType_StrTab,
            .sh_addralign = 1,
        });

    elf_init_section(
        builder,
        builder->strtab_index,
        ".strtab",
        {
            .sh_type = Elf64SectionType_StrTab,
            .sh_addralign = 1,
        });

    elf_init_section(
        builder,
        builder->symtab_index,
        ".symtab",
        {
            .sh_type = Elf64SectionType_SymTab,
            .sh_link = builder->strtab_index, // associated string table
            .sh_info = 0, // the index of the first non-local symbol
            .sh_addralign = 8,
            .sh_entsize = sizeof(Elf64Symbol),
        });

    switch (module->target_arch) {
    case TargetArch_X86_64: {
        elf_init_section(
            builder,
            builder->text_index,
            ".text",
            {
                .sh_type = Elf64SectionType_ProgBits,
                .sh_flags =
                    Elf64SectionFlags_Alloc | Elf64SectionFlags_ExecInstr,
                .sh_addralign = 1,
            });

        elf_init_section(
            builder,
            builder->rela_text_index,
            ".rela.text",
            {
                .sh_type = Elf64SectionType_Rela,
                .sh_flags = Elf64SectionFlags_InfoLink,
                .sh_link = builder->symtab_index, // associated symbol table
                .sh_info = builder->text_index,   // the section to which the
                                                  // relocations are applied
                .sh_addralign = 8,
                .sh_entsize = sizeof(Elf64Rela),
            });
        break;
    }
    }

    elf_init_section(
        builder,
        builder->data_index,
        ".data",
        {
            .sh_type = Elf64SectionType_ProgBits,
            .sh_flags = Elf64SectionFlags_Alloc | Elf64SectionFlags_Write,
            .sh_addralign = 1,
        });

    elf_init_section(
        builder,
        builder->bss_index,
        ".bss",
        {
            .sh_type = Elf64SectionType_NoBits,
            .sh_flags = Elf64SectionFlags_Alloc | Elf64SectionFlags_Write,
            .sh_addralign = 1,
        });

    elf_init_section(
        builder,
        builder->rodata_index,
        ".rodata",
        {
            .sh_type = Elf64SectionType_ProgBits,
            .sh_flags = Elf64SectionFlags_Alloc,
            .sh_addralign = 1,
        });

    builder->symbols.push_back(Symbol{}); // Null symbol

    builder->text_symbol_ref = builder->add_symbol(
        "", SectionType_Text, SymbolType_Section, Linkage_Internal);
    builder->data_symbol_ref = builder->add_symbol(
        "", SectionType_Data, SymbolType_Section, Linkage_Internal);
    builder->bss_symbol_ref = builder->add_symbol(
        "", SectionType_BSS, SymbolType_Section, Linkage_Internal);
    builder->rodata_symbol_ref = builder->add_symbol(
        "", SectionType_ROData, SymbolType_Section, Linkage_Internal);

    return builder;
}

bool Elf64Builder::output_to_file(String path)
{
    ZoneScoped;

    for (size_t i = 0; i < this->symbols.len; ++i) {
        Symbol *symbol = &this->symbols[i];
        if (symbol->linkage == Linkage_Internal) {
            elf_output_symbol(this, i);
        }
    }

    {
        // Set symtab->header.sh_info, which is the index of the first non-local
        // symbol
        Section *symtab = &this->sections[this->symtab_index];
        symtab->header.sh_info = this->output_symbol_count;
    }

    for (size_t i = 0; i < this->symbols.len; ++i) {
        Symbol *symbol = &this->symbols[i];
        if (symbol->linkage != Linkage_Internal) {
            elf_output_symbol(this, i);
        }
    }

    Section *rela_text_section = &this->sections[this->rela_text_index];
    Elf64Rela *relas = (Elf64Rela *)rela_text_section->data.ptr;
    size_t rela_entry_count = rela_text_section->data.len / sizeof(Elf64Rela);
    for (size_t i = 0; i < rela_entry_count; ++i) {
        Elf64Rela *rela = &relas[i];
        uint64_t type = ACE_ELF64_R_TYPE(rela->r_info);
        uint64_t old_sym_index = ACE_ELF64_R_SYM(rela->r_info);
        uint64_t new_sym_index = this->symbols[old_sym_index].final_index;
        rela->r_info = ACE_ELF64_R_INFO(new_sym_index, type);
    }

    FILE *f = fopen(this->module->arena->null_terminate(path), "wb");
    if (!f) {
        fprintf(
            stderr, "Failed to open file: '%.*s'\n", (int)path.len, path.ptr);
        return false;
    }

    size_t total_section_data_size = 0;

    for (auto &section : this->sections) {
        if (section.header.sh_addralign > 0) {
            size_t section_data_padding =
                section.data.len % section.header.sh_addralign;
            for (size_t i = 0; i < section_data_padding; ++i) {
                section.data.push_back(0);
            }
        }

        section.header.sh_offset =
            sizeof(Elf64Header) + total_section_data_size;
        section.header.sh_size = section.data.len;

        total_section_data_size += section.data.len;
    }

    this->header.e_shoff = sizeof(Elf64Header) + total_section_data_size;
    this->header.e_shnum = this->sections.len;

    fwrite(&this->header, 1, sizeof(this->header), f);
    for (auto &section : this->sections) {
        if (section.data.len > 0) {
            fwrite(section.data.ptr, 1, section.data.len, f);
        }
    }

    for (auto &section : this->sections) {
        fwrite(&section.header, 1, sizeof(Elf64SectionHeader), f);
    }

    fclose(f);

    return true;
}

void Elf64Builder::destroy()
{
    for (auto &section : this->sections) {
        section.data.destroy();
    }
    this->sections.destroy();
}

void Elf64Builder::add_to_section(SectionType type, Slice<uint8_t> data)
{
    ZoneScoped;

    size_t section_index = 0;
    switch (type) {
    case SectionType_None: ACE_ASSERT(0); break;
    case SectionType_Text: section_index = this->text_index; break;
    case SectionType_Data: section_index = this->data_index; break;
    case SectionType_BSS: section_index = this->bss_index; break;
    case SectionType_ROData: section_index = this->rodata_index; break;
    }

    Section *section = &this->sections[section_index];
    section->data.push_many(data);
}

void Elf64Builder::set_section_data(
    SectionType type, size_t offset, Slice<uint8_t> data)
{
    ZoneScoped;

    size_t section_index = 0;
    switch (type) {
    case SectionType_None: ACE_ASSERT(0); break;
    case SectionType_Text: section_index = this->text_index; break;
    case SectionType_Data: section_index = this->data_index; break;
    case SectionType_BSS: section_index = this->bss_index; break;
    case SectionType_ROData: section_index = this->rodata_index; break;
    }

    Section *section = &this->sections[section_index];

    ACE_ASSERT(offset + data.len <= section->data.len);

    for (size_t i = 0; i < data.len; ++i) {
        section->data.ptr[offset + i] = data.ptr[i];
    }
}

size_t Elf64Builder::get_section_size(SectionType type)
{
    ZoneScoped;

    size_t section_index = 0;
    switch (type) {
    case SectionType_None: ACE_ASSERT(0); break;
    case SectionType_Text: section_index = this->text_index; break;
    case SectionType_Data: section_index = this->data_index; break;
    case SectionType_BSS: section_index = this->bss_index; break;
    case SectionType_ROData: section_index = this->rodata_index; break;
    }

    Section *section = &this->sections[section_index];
    return section->data.len;
}

void Elf64Builder::add_data_relocation(
    SectionType source_section,
    int64_t source_data_offset,
    uint64_t destination_offset,
    size_t dest_addr_size)
{
    ZoneScoped;

    uint64_t section_sym_index = 0;
    switch (source_section) {
    case SectionType_None: ACE_ASSERT(0); break;
    case SectionType_Text: section_sym_index = this->text_symbol_ref.id; break;
    case SectionType_Data: section_sym_index = this->data_symbol_ref.id; break;
    case SectionType_BSS: section_sym_index = this->bss_symbol_ref.id; break;
    case SectionType_ROData:
        section_sym_index = this->rodata_symbol_ref.id;
        break;
    }

    Elf64Rela rela{};

    switch (this->module->target_arch) {
    case TargetArch_X86_64: {
        switch (dest_addr_size) {
        case 4: {
            rela.r_offset = destination_offset;
            rela.r_addend = source_data_offset - 4;
            rela.r_info =
                ACE_ELF64_R_INFO(section_sym_index, Elf64RelaTypeX86_64_PC32);
            break;
        }
        default: ACE_ASSERT(0);
        }
        break;
    }
    }

    Section *rela_text_section = &this->sections[this->rela_text_index];
    rela_text_section->data.push_many({(uint8_t *)&rela, sizeof(rela)});
}

void Elf64Builder::add_procedure_relocation(
    SymbolRef function_symbol,
    uint64_t destination_offset,
    size_t dest_addr_size)
{
    ZoneScoped;

    Elf64Rela rela{};

    switch (this->module->target_arch) {
    case TargetArch_X86_64: {
        switch (dest_addr_size) {
        case 4: {
            rela.r_offset = destination_offset;
            rela.r_addend = -4;
            rela.r_info = ACE_ELF64_R_INFO(
                (uint64_t)function_symbol.id, Elf64RelaTypeX86_64_PLT32);
            break;
        }
        default: ACE_ASSERT(0);
        }
        break;
    }
    }

    Section *rela_text_section = &this->sections[this->rela_text_index];
    rela_text_section->data.push_many({(uint8_t *)&rela, sizeof(rela)});
}

SymbolRef Elf64Builder::add_symbol(
    String name, SectionType section_type, SymbolType type, Linkage linkage)
{
    ZoneScoped;

    Symbol symbol{
        .type = type,
        .section_type = section_type,
        .linkage = linkage,
        .name = name,
    };

    SymbolRef ref = {(uint32_t)this->symbols.len};
    this->symbols.push_back(symbol);
    return ref;
}

void Elf64Builder::set_symbol_region(
    SymbolRef symbol_ref, size_t offset, size_t size)
{
    ZoneScoped;
    Symbol *symbol = &this->symbols[symbol_ref.id];
    symbol->offset = offset;
    symbol->size = size;
}

} // namespace ace
