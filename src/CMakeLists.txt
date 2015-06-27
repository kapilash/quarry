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


find_package(Doxygen)
if(DOXYGEN_FOUND)
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)
    add_custom_target(doc
	${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
	WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
	COMMENT "Generating API documentation with Doxygen" VERBATIM
	)
endif(DOXYGEN_FOUND)

add_library(quarry SHARED   ${SOURCES})


GENERATE_EXPORT_HEADER(quarry)
target_link_libraries(quarry ${Boost_LIBRARIES})

install (TARGETS quarry  DESTINATION lib)
install (FILES Quarry.h DESTINATION include)
install (FILES QToken.h DESTINATION include)
configure_file(Quarry.h Quarry.h COPYONLY)
configure_file(QToken.h QToken.h COPYONLY)
