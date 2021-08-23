cmake_minimum_required(VERSION 3.19.0)

project(ctest_shocktube Fortran)

enable_language (Fortran)

set(LIB_MOD_DIR ${CMAKE_CURRENT_BINARY_DIR}/mod_files/)

enable_testing()

add_subdirectory(src)
add_subdirectory(tests)
