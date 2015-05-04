cmake_minimum_required (VERSION 3.0)

configure_file(ReaderTest1 ReaderTest1 COPYONLY)
add_executable(rtests "rtests.cpp")
target_link_libraries(rtests ${Boost_LIBRARIES} quarry)
