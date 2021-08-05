cmake_minimum_required(VERSION 3.1)
project(lang VERSION 1.0 LANGUAGES C CXX)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
	add_compile_options (-fdiagnostics-color=always)
elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
	add_compile_options (-fcolor-diagnostics)
endif ()

if (NOT MSVC)
	add_compile_options(-Wall -Wextra -Wno-unused-function)
	# set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -fsanitize=address,undefined")
	# set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -fsanitize=memory -fsanitize-memory-track-origins")
endif()

set(TRACY_ENABLE OFF)
if (TRACY_ENABLE)
	add_definitions(-DTRACY_ENABLE)
endif()

if (TRACY_ENABLE)
	add_library(
		tracy_client

		thirdparty/tracy/TracyClient.cpp
	)
	target_link_libraries(tracy_client PUBLIC pthread dl rt)
	target_include_directories(tracy_client PUBLIC thirdparty/tracy)
endif()

add_library(
  sir

  sir/sir_base.hpp
  sir/sir_base.cpp
  sir/sir_ir.hpp
  sir/sir_ir.cpp
  sir/stb_sprintf.c

  sir/sir_obj.cpp
  sir/sir_obj.hpp
  sir/sir_elf64.cpp

  sir/sir_x64.cpp
  sir/sir_x64_encoder.c
  sir/sir_x64_encoder.h
  sir/sir_x64_enc_cases.h
  sir/sir_x64_enc_mnems.h
)
target_include_directories(sir PUBLIC sir)
if (TRACY_ENABLE)
	target_link_libraries(sir PRIVATE tracy_client)
endif()

add_executable(
	compiler
	src/main.cpp
	src/base.cpp
	src/compiler.cpp
	src/parser.cpp
	src/analysis.cpp
	src/codegen.cpp
)
target_link_libraries(compiler PRIVATE sir)
if (TRACY_ENABLE)
	target_link_libraries(compiler PRIVATE tracy_client)
endif()

