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
    auto global_str_2 = module->add_global_string("this is another string!");
    auto int_fmt_str = module->add_global_string("%ld\n");
    auto f32_fmt_str = module->add_global_string("%lf\n");

    auto global_int = module->add_global(
        module->i64_type, GlobalFlags_Initialized, {127, 0, 0, 0, 0, 0, 0, 0});
    auto global_int2 = module->add_global(
        module->i64_type, GlobalFlags_Initialized, {0, 0, 0, 0, 0, 0, 0, 0});
    auto global_float1 = module->add_global(
        module->f64_type, GlobalFlags_Initialized, {0, 0, 0, 0, 0, 0, 0, 0});

    auto puts_func = module->add_function(
        "puts",
        ace::CallingConvention_SystemV,
        ace::Linkage_External,
        false,
        {module->create_pointer_type(module->i8_type)},
        module->void_type);

    auto printf_func = module->add_function(
        "printf",
        ace::CallingConvention_SystemV,
        ace::Linkage_External,
        true,
        {module->create_pointer_type(module->i8_type)},
        module->i32_type);

    {
        auto function_ref = module->add_function(
            "return_param",
            ace::CallingConvention_SystemV,
            ace::Linkage_Internal,
            false,
            {module->i64_type},
            module->i64_type);

        auto param1 = module->get_func_param(function_ref, 0);

        builder.set_function(function_ref);

        auto block = module->insert_block_at_end(function_ref);
        builder.position_at_end(block);

        builder.insert_return_value(param1);
    }

    FunctionRef change_global_float_func;
    {
        auto function_ref = module->add_function(
            "change_global_float",
            ace::CallingConvention_SystemV,
            ace::Linkage_Internal,
            false,
            {},
            module->void_type);
        change_global_float_func = function_ref;

        builder.set_function(function_ref);

        auto block = module->insert_block_at_end(function_ref);
        builder.position_at_end(block);

        builder.insert_global_store(
            global_float1,
            builder.insert_get_const(
                module->create_float_const(module->f64_type, 1.0)));

        builder.insert_return_void();
    }

    FunctionRef other_func;
    {
        auto function_ref = module->add_function(
            "other",
            ace::CallingConvention_SystemV,
            ace::Linkage_External,
            false,
            {},
            module->void_type);
        other_func = function_ref;

        StackSlotRef ss0 =
            module->add_stack_slot(function_ref, module->i64_type);
        StackSlotRef ss1 =
            module->add_stack_slot(function_ref, module->i64_type);
        StackSlotRef ss2 =
            module->add_stack_slot(function_ref, module->i64_type);
        StackSlotRef ss3 =
            module->add_stack_slot(function_ref, module->i64_type);
        StackSlotRef ss4 =
            module->add_stack_slot(function_ref, module->i64_type);
        StackSlotRef ss5 = module->add_stack_slot(
            function_ref, module->create_pointer_type(module->i64_type));

        builder.set_function(function_ref);

        auto block = module->insert_block_at_end(function_ref);
        builder.position_at_end(block);

        auto int_const = builder.insert_get_const(
            module->create_int_const(module->i64_type, 123));
        builder.insert_stack_store(ss0, int_const);
        auto loaded_val = builder.insert_stack_load(ss0);
        builder.insert_stack_store(ss1, loaded_val);
        builder.insert_stack_store(ss2, loaded_val);
        builder.insert_stack_store(ss3, loaded_val);
        builder.insert_stack_store(ss4, loaded_val);

        auto ss4_addr = builder.insert_stack_addr(ss4);
        builder.insert_stack_store(ss5, ss4_addr);

        auto block2 = module->insert_block_at_end(function_ref);

        builder.insert_jump(block2);
        builder.position_at_end(block2);
        builder.insert_return_void();
    }

    {
        auto function_ref = module->add_function(
            "main",
            ace::CallingConvention_SystemV,
            ace::Linkage_External,
            false,
            {},
            module->i64_type);

        builder.set_function(function_ref);

        auto block = module->insert_block_at_end(function_ref);
        builder.position_at_end(block);

        builder.insert_func_call(
            puts_func, {builder.insert_global_addr(global_str_1)});
        builder.insert_func_call(
            puts_func, {builder.insert_global_addr(global_str_2)});

        builder.insert_func_call(
            printf_func,
            {
                builder.insert_global_addr(int_fmt_str),
                builder.insert_global_load(global_int),
            });
        builder.insert_global_store(
            global_int,
            builder.insert_get_const(
                module->create_int_const(module->i64_type, 123)));
        auto loaded_int =
            builder.insert_global_load(global_int2);
        builder.insert_global_store(global_int, loaded_int);
        builder.insert_func_call(
            printf_func,
            {
                builder.insert_global_addr(int_fmt_str),
                builder.insert_global_load(global_int),
            });

        builder.insert_func_call(
            printf_func,
            {
                builder.insert_global_addr(f32_fmt_str),
                builder.insert_global_load(global_float1),
            });
        builder.insert_func_call(change_global_float_func, {});
        builder.insert_func_call(
            printf_func,
            {
                builder.insert_global_addr(f32_fmt_str),
                builder.insert_get_const(
                    module->create_float_const(module->f64_type, 123.0)),
            });

        auto const_return = builder.insert_get_const(
            module->create_int_const(module->i64_type, 123));
        builder.insert_return_value(const_return);
    }

    {
        auto allocator = MallocAllocator::get_instance();
        String str = module->print_alloc(allocator);
        printf("%.*s", (int)str.len, str.ptr);
        allocator->free(str);
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
