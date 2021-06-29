#!/bin/bash
set -x

ninja -C build &&\
./build/compiler test.lang &&\
readelf -a ./main.o &&\
objdump -d -M intel-mnemonic ./main.o &&\
gcc ./main.o -o a.out &&\
./a.out
