cmake_minimum_required (VERSION 3.0)


configure_file(HugeJavaIn HugeJavaIn COPYONLY)
configure_file(ReaderTest1 ReaderTest1 COPYONLY)
include_directories(${PROJECT_SOURCE_DIR}/src)
add_executable(rtests "rtests.cpp")
target_link_libraries(rtests quarry ${Boost_LIBRARIES})
