cmake_minimum_required (VERSION 3.0)
set(CMAKE_INCLUDE_CURRENT_DIR ON)


set (SOURCES
    QReader.cpp
    Lexers.cpp
    QContext.cpp
    QInternal.h
    QReader.h
    QuarryMaps.cpp
    Characters.cpp
    Numbers.cpp
    QToken.h
    QToken.cpp
    Quarry.h
    )


add_library(quarry SHARED   ${SOURCES})


GENERATE_EXPORT_HEADER(quarry)
target_link_libraries(quarry ${Boost_LIBRARIES})

message ("Compiler id is ${CMAKE_CXX_COMPILER_ID}")

add_executable(qexample "Example.cpp")

target_link_libraries(qexample quarry)

#install (TARGETS quarry  DESTINATION lib)
#install (FILES quarry.h DESTINATION include)
