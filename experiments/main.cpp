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

    SIRBuilder *builder = SIRBuilderCreate(mod);

    const char *sum_func_name = "sum";

    SIRType *sum_param_types[2] = {
        SIRModuleGetI64Type(mod),
        SIRModuleGetI64Type(mod),
    };

    SIRInstRef sum_func = SIRModuleAddFunction(
        mod,
        sum_func_name,
        strlen(sum_func_name),
        SIRCallingConvention_SystemV,
        SIRLinkage_Internal,
        false,
        sum_param_types,
        2,
        SIRModuleGetI64Type(mod));

    {
        SIRBuilderSetFunction(builder, sum_func);

        SIRInstRef start_block = SIRModuleInsertBlockAtEnd(mod, sum_func);

        SIRInstRef param0 = SIRModuleGetFuncParam(mod, sum_func, 0);
        SIRInstRef param1 = SIRModuleGetFuncParam(mod, sum_func, 1);

        SIRBuilderPositionAtEnd(builder, start_block);

        SIRInstRef result = SIRBuilderInsertBinop(
            builder, SIRBinaryOperation_IAdd, param0, param1);
        SIRBuilderInsertReturnValue(builder, result);
    }

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

    SIRInstRef ss0 =
        SIRModuleAddStackSlot(mod, my_func, SIRModuleGetI64Type(mod));

    SIRInstRef start_block = SIRModuleInsertBlockAtEnd(mod, my_func);
    SIRInstRef other_block1 = SIRModuleInsertBlockAtEnd(mod, my_func);
    SIRInstRef other_block2 = SIRModuleInsertBlockAtEnd(mod, my_func);
    SIRBuilderSetFunction(builder, my_func);

    {
        SIRBuilderPositionAtEnd(builder, start_block);

        SIRInstRef params[2] = {
            SIRModuleAddConstInt(mod, SIRModuleGetI64Type(mod), 100),
            SIRModuleAddConstInt(mod, SIRModuleGetI64Type(mod), 23),
        };
        SIRInstRef sum = SIRBuilderInsertFuncCall(builder, sum_func, params, 2);

        SIRBuilderInsertStore(builder, ss0, sum);

        SIRInstRef cond_ref = SIRModuleAddConstBool(mod, true);
        SIRBuilderInsertBranch(builder, cond_ref, other_block1, other_block2);
    }

    {
        SIRBuilderPositionAtEnd(builder, other_block1);

        SIRInstRef ret_value = SIRBuilderInsertLoad(builder, ss0);
        SIRBuilderInsertReturnValue(builder, ret_value);
    }

    {
        SIRBuilderPositionAtEnd(builder, other_block2);

        SIRInstRef ret_value =
            SIRModuleAddConstInt(mod, SIRModuleGetI64Type(mod), 321);
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
