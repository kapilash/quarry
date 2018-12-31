cmake_minimum_required (VERSION 3.13)
set(CMAKE_INCLUDE_CURRENT_DIR ON)




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

target_include_directories(kwverify PUBLIC ${PROJECT_SOURCE_DIR}/inc ${PROJECT_BINARY_DIR}/src ${Boost_INCLUDE_DIR})
target_link_libraries(kwverify PRIVATE ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY} )

