cmake_minimum_required (VERSION 3.0)

project(quarry VERSION 0.5 LANGUAGES C CXX)

include_directories (${PROJECT_SOURCE_DIR}/inc ${PROJECT_BINARY_DIR}/src)
include(GenerateExportHeader)

add_subdirectory(src)
add_subdirectory(tests)
