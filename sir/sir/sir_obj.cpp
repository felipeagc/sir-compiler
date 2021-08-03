#include "sir.h"
#include "sir_obj.hpp"

void SIRObjectBuilderDestroy(SIRObjectBuilder *obj_builder)
{
    obj_builder->destroy(obj_builder);
}

void SIRObjectBuilderOutputToFile(
    SIRObjectBuilder *obj_builder, const char *path, size_t path_len)
{
    obj_builder->output_to_file(obj_builder, (SIRString){path, path_len});
}

void SIRAsmBuilderGenerate(SIRAsmBuilder *asm_builder)
{
    asm_builder->generate(asm_builder);
}

void SIRAsmBuilderDestroy(SIRAsmBuilder *asm_builder)
{
    asm_builder->destroy(asm_builder);
}
