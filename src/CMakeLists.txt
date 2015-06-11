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
    )
    
add_library(quarry SHARED   ${SOURCES})

GENERATE_EXPORT_HEADER(quarry)
target_link_libraries(quarry ${Boost_LIBRARIES})

#add_executable(qexample "exquarry.c")

#target_link_libraries(qexample quarry)

#install (TARGETS quarry  DESTINATION lib)
#install (FILES quarry.h DESTINATION include)
