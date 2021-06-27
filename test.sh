#!/bin/bash

ninja -C build &&\
./build/ace/ace_tests &&\
readelf -a ./main.o &&\
objdump -d -M intel-mnemonic ./main.o &&\
gcc ./main.o -o a.out &&\
./a.out
