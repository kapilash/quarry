cmake_minimum_required (VERSION 3.0)
set(CMAKE_INCLUDE_CURRENT_DIR ON)

find_package(Boost
  1.57.0
   COMPONENTS
  unit_test_framework
  test_exec_monitor
   REQUIRED
 )

include_directories ( ${Boost_INCLUDE_DIRS})

if (MSVC)
 message("Visual Studio Compiler")
else()
add_compile_options(-std=c++11)
add_compile_options(-stdlib=libc++)
endif()
set(SOURCES
  kw.cpp
)


configure_file(CKeywords CKeywords COPYONLY)
configure_file(COperators COperators COPYONLY)
configure_file(JavaKeywords JavaKeywords COPYONLY)
configure_file(JavaOperators JavaOperators COPYONLY)
add_executable(kw
  ${SOURCES}
)

add_executable(kwverify "verify.cpp")
target_link_libraries(kwverify ${Boost_LIBRARIES})
