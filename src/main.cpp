#include "compiler.hpp"
#include <stdio.h>

int main(int argc, const char *argv[])
{
    if (argc < 2)  {
        fprintf(stderr, "error: expected command syntax: %s <filename>\n", argv[0]);
        exit(1);
    }

    Compiler compiler = Compiler::create();

    compiler.compile(SIR_CSTR(argv[1]));

    compiler.destroy();
    return 0;
}
