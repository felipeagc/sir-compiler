cmake_minimum_required(VERSION 3.1)
project(lang VERSION 1.0 LANGUAGES C CXX)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
# add_definitions(-DTRACY_ENABLE)

add_subdirectory(ace)

add_executable(
	compiler
	src/main.cpp
)
target_link_libraries(compiler PRIVATE ace)

if (NOT MSVC)
	set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -fsanitize=address,undefined")
endif()
