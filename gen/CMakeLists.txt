cmake_minimum_required (VERSION 3.0)
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
target_link_libraries(kwverify ${Boost_LIBRARIES})
