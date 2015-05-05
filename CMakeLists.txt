cmake_minimum_required (VERSION 3.0)

project(quarry VERSION 0.5 LANGUAGES C CXX)
find_package(Boost
  1.57.0
   COMPONENTS
  unit_test_framework
   REQUIRED
 )

if (MSVC)
 message("Visual Studio Compiler")
else()
add_compile_options(-std=c++11)
add_compile_options(-stdlib=libc++)
endif()


include_directories (${Boost_INCLUDE_DIRS} ${PROJECT_SOURCE_DIR}/inc ${PROJECT_SOURCE_DIR}/src)

add_subdirectory(gen)
add_subdirectory(src)
add_subdirectory(tests)
