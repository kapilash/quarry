cmake_minimum_required (VERSION 3.0)
set(CMAKE_INCLUDE_CURRENT_DIR ON)


FIND_LIBRARY(QUARRY_LIBRARY quarry ${QU_LIB_DIR})
message(${QU_LIB_DIR})
message(${QU_INC_DIR})
include_directories(${QU_INC_DIR})
add_executable(qexample "Example.cpp")

target_link_libraries(qexample ${QUARRY_LIBRARY})

