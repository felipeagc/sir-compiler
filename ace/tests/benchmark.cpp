#include <stdio.h>
#include <ace_base.hpp>
#include <ace_ir.hpp>
#include <ace_obj.hpp>

using namespace ace;

int main()
{
#ifdef NDEBUG
    printf("Running in release mode\n");
#else
    printf("Running in debug mode\n");
#endif

    Module *module =
        Module::create(TargetArch_X86_64, ace::Endianness_LittleEndian);
    Builder builder = Builder::create(module);

    auto global_str_1 = module->add_global_string("hello, world!");

    auto printf_func = module->add_function(
        "printf",
        ace::CallingConvention_SystemV,
        ace::Linkage_External,
        true,
        {module->create_pointer_type(module->i8_type)},
        module->i32_type);

    for (size_t i = 0; i < 1000; ++i) {
        auto function_ref = module->add_function(
            module->arena->sprintf("func%zu", i),
            ace::CallingConvention_SystemV,
            ace::Linkage_Internal,
            false,
            {},
            module->void_type);

        builder.set_function(function_ref);

        auto block = module->insert_block_at_end(function_ref);
        builder.position_at_end(block);

        for (size_t j = 0; j < 100; ++j) {
            builder.insert_func_call(
                printf_func, {builder.insert_global_addr(global_str_1)});
        }

        builder.insert_return_void();
    }

    {
        auto function_ref = module->add_function(
            "main",
            ace::CallingConvention_SystemV,
            ace::Linkage_Internal,
            false,
            {},
            module->i32_type);

        builder.set_function(function_ref);

        auto block = module->insert_block_at_end(function_ref);
        builder.position_at_end(block);

        builder.insert_return_value(builder.insert_get_const(
            module->create_int_const(module->i32_type, 0)));
    }

    ObjectBuilder *obj_builder = ace::create_elf64_builder(module);
    AsmBuilder *asm_builder = ace::create_x86_64_builder(module, obj_builder);

    asm_builder->generate();

    obj_builder->output_to_file("./main.o");

    asm_builder->destroy();
    obj_builder->destroy();
    module->destroy();

    return 0;
}
