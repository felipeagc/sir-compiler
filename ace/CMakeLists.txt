add_library(
  ace

  ace/ace.h
  ace/ace_base.hpp
  ace/ace_base.cpp
  ace/ace_ir.hpp
  ace/ace_ir.cpp
  ace/ace_type.hpp
  ace/ace_type.cpp

  ace/ace_obj.hpp
  ace/ace_elf64.cpp

  ace/x86_64/builder.cpp
  ace/x86_64/encoder.h
  ace/x86_64/encoder.c

  tracy/Tracy.hpp
  tracy/TracyClient.cpp
)
target_include_directories(ace PUBLIC ace)
target_include_directories(ace PUBLIC tracy)

target_link_libraries(ace PRIVATE pthread dl rt)

add_executable(ace_tests tests/main.cpp)
target_link_libraries(ace_tests PRIVATE ace)

add_executable(ace_benchmark tests/benchmark.cpp)
target_link_libraries(ace_benchmark PRIVATE ace)
