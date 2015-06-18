cmake_minimum_required (VERSION 3.0)
set(CMAKE_INCLUDE_CURRENT_DIR ON)


FIND_LIBRARY(QUARRY_LIBRARY quarry ${QU_LIB_DIR})
message(${QU_DIR})
include_directories(${QU_BIN_DIR} ${QU_SRC_DIR})
add_executable(qexample "example.c")

target_link_libraries(qexample ${QUARRY_LIBRARY})

