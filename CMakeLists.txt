cmake_minimum_required (VERSION 3.0)

project(quarry VERSION 0.5 LANGUAGES C CXX)

Set(Boost_USE_MULTITHREADED ON)
set(Boost_USE_STATIC_LIBS ON)
find_package(Boost 1.56 REQUIRED COMPONENTS system filesystem unit_test_framework iostreams python)

include_directories (${PROJECT_SOURCE_DIR}/inc ${PROJECT_BINARY_DIR}/src ${Boost_INCLUDE_DIR})
include(GenerateExportHeader)
#ADD_DEFINITIONS(-DBOOST_TEST_DYN_LINK)
link_directories(${Boost_LIBRARY_DIR})

message ("Compiler id is ${CMAKE_CXX_COMPILER_ID}")
if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  add_compile_options(-std=c++11 -stdlib=libc++)
elseif(CMAKE_CXX_COMPILER_ID MATCHES "GNU")
  add_compile_options(-std=c++11)
endif()

add_subdirectory(gen)
add_subdirectory(src)
add_subdirectory(tests)
