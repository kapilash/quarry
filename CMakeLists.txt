cmake_minimum_required (VERSION 3.0)

project(quarry VERSION 0.5 LANGUAGES C)

include_directories (${PROJECT_SOURCE_DIR}/inc ${PROJECT_SOURCE_DIR}/src)

add_subdirectory(src)
if (MSVC)
 message ("not running tests for now")
else()
add_subdirectory(tests)
endif()
