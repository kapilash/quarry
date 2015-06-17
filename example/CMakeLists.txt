cmake_minimum_required (VERSION 3.0)
set(CMAKE_INCLUDE_CURRENT_DIR ON)


FIND_LIBRARY(QUARRY_LIBRARY quarry ${QU_DIR}/build/src)
message(${QU_DIR})
include_directories(${QU_DIR}/src ${QU_DIR}/build/src)
add_executable(qexample "example.c")

target_link_libraries(qexample ${QUARRY_LIBRARY})

