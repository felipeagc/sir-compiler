#!/usr/bin/env bash
set -x

[[ -d build ]] || cmake -Bbuild -GNinja -DCMAKE_BUILD_TYPE=Debug .
ninja -C build &&\
./build/compiler tests/basic.lang &&\
# readelf -a ./main.o &&\
objdump -d -M intel-mnemonic ./main.o &&\
gcc ./main.o -o a.out &&\
./a.out
