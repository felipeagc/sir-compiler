#include <sir.h>
#include <sir_interp.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

int main()
{
    SIRModule *mod =
        SIRModuleCreate(SIRTargetArch_X86_64, SIREndianness_LittleEndian);

    const char *func_name = "interp_func";

    SIRInstRef my_func = SIRModuleAddFunction(
        mod,
        func_name,
        strlen(func_name),
        SIRCallingConvention_SystemV,
        SIRLinkage_External,
        false,
        NULL,
        0,
        SIRModuleGetI64Type(mod));

    SIRBuilder *builder = SIRBuilderCreate(mod);

    SIRInstRef start_block = SIRModuleInsertBlockAtEnd(mod, my_func);
    SIRInstRef other_block1 = SIRModuleInsertBlockAtEnd(mod, my_func);
    SIRInstRef other_block2 = SIRModuleInsertBlockAtEnd(mod, my_func);
    SIRBuilderSetFunction(builder, my_func);
    SIRBuilderPositionAtEnd(builder, start_block);

    SIRInstRef cond_ref = SIRBuilderInsertImmBool(builder, true);
    SIRBuilderInsertBranch(builder, cond_ref, other_block1, other_block2);

    {
        SIRBuilderPositionAtEnd(builder, other_block1);

        SIRInstRef ret_value =
            SIRBuilderInsertImmInt(builder, SIRModuleGetI64Type(mod), 123);
        SIRBuilderInsertReturnValue(builder, ret_value);
    }

    {
        SIRBuilderPositionAtEnd(builder, other_block2);

        SIRInstRef ret_value =
            SIRBuilderInsertImmInt(builder, SIRModuleGetI64Type(mod), 321);
        SIRBuilderInsertReturnValue(builder, ret_value);
    }

    {
        size_t mod_str_len = 0;
        const char *mod_str = SIRModulePrintToString(mod, &mod_str_len);

        fprintf(stderr, "%.*s", (int)mod_str_len, mod_str);

        free((char *)mod_str);
    }

    SIRInterpContext *ctx = SIRInterpContextCreate(mod);

    int64_t res = 0;
    SIRInterpFunction(ctx, my_func, &res);
    printf("returned = %ld\n", res);
    assert(res == 123);

    SIRInterpContextDestroy(ctx);
    SIRModuleDestroy(mod);
    return 0;
}
