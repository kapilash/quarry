cmake_minimum_required (VERSION 3.0)


configure_file(HugeJavaIn HugeJavaIn COPYONLY)
configure_file(ReaderTest1 ReaderTest1 COPYONLY)
configure_file(ManualComments.txt ManualComments.txt COPYONLY)
configure_file(Numbers.txt Numbers.txt COPYONLY)
configure_file(Chars.txt Chars.txt COPYONLY)
configure_file(Strings.txt Strings.txt COPYONLY)
configure_file(Idents.txt Idents.txt COPYONLY)

include_directories(${PROJECT_SOURCE_DIR}/src)
add_executable(rtests "rtests.cpp")
add_executable(comments "CommentsTest.cpp")
add_executable(misc "TokenTests.cpp")

target_link_libraries(rtests quarry ${Boost_LIBRARIES})

target_link_libraries(comments quarry ${Boost_LIBRARIES})

target_link_libraries(misc quarry ${Boost_LIBRARIES})
