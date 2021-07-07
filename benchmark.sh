#!/usr/bin/env bash
set -x

[[ -d build-release ]] || cmake -Bbuild-release -GNinja -DCMAKE_BUILD_TYPE=RelWithDebInfo .
pushd benchmark &&\
lua generate_test.lua &&\
popd &&\
ninja -C build-release &&\
./build-release/compiler benchmark/test_benchmark.lang
