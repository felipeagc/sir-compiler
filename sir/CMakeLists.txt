add_library(
  sir

  sir/sir_base.hpp
  sir/sir_base.cpp
  sir/sir_ir.hpp
  sir/sir_ir.cpp
  sir/sir_type.hpp
  sir/sir_type.cpp
  sir/stb_sprintf.c

  sir/sir_obj.cpp
  sir/sir_obj.hpp
  sir/sir_elf64.cpp

  sir/x86_64/builder.cpp
  sir/x86_64/encoder.h
  sir/x86_64/encoder.c

  tracy/Tracy.hpp
  tracy/TracyClient.cpp
)
target_include_directories(sir PUBLIC sir)
target_include_directories(sir PUBLIC tracy)

target_link_libraries(sir PRIVATE pthread dl rt)
