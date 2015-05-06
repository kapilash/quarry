cmake_minimum_required (VERSION 3.0)

project(quarry VERSION 0.5 LANGUAGES C)

include_directories (${PROJECT_SOURCE_DIR}/inc ${PROJECT_SOURCE_DIR}/src ${PROJECT_BINARY_DIR}/src)
include(GenerateExportHeader)
add_subdirectory(src)
add_subdirectory(tests)
