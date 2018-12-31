cmake_minimum_required (VERSION 3.13)
set(CMAKE_INCLUDE_CURRENT_DIR ON)


find_package(Doxygen)
if(DOXYGEN_FOUND)
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)
    add_custom_target(doc
	${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
	WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
	COMMENT "Generating API documentation with Doxygen" VERBATIM
	)
endif(DOXYGEN_FOUND)

add_library(quarry SHARED
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
    Quarry.h)


GENERATE_EXPORT_HEADER(quarry)
target_link_libraries(quarry PUBLIC Boost::filesystem Boost::iostreams Boost::system)
target_include_directories(quarry PUBLIC ${PROJECT_SOURCE_DIR}/inc ${PROJECT_BINARY_DIR}/src ${Boost_INCLUDE_DIR})

install (TARGETS quarry  DESTINATION lib)
install (FILES Quarry.h DESTINATION include)
install (FILES QToken.h DESTINATION include)
configure_file(Quarry.h Quarry.h COPYONLY)
configure_file(QToken.h QToken.h COPYONLY)
