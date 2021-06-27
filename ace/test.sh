#!/bin/bash

ninja -C build &&\
./build/ace_tests &&\
readelf -a ./main.o &&\
objdump -d -M intel-mnemonic ./main.o &&\
gcc ./main.o -o a.out &&\
./a.out
