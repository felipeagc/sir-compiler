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

# add_definitions(-DTRACY_ENABLE)

add_subdirectory(sir)

add_executable(
	compiler
	src/main.cpp
	src/compiler.cpp
	src/parser.cpp
	src/analysis.cpp
	src/codegen.cpp
)
target_link_libraries(compiler PRIVATE sir)

