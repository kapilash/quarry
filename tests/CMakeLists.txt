cmake_minimum_required (VERSION 3.0)

set(SOURCES quarry_testrunner.c
            comments_tests.c
            identifier_tests.c
            quotes_tests.c
            numbers_tests.c
            reader_tests.c
            metaid_tests.c
            CuTest.c
            CuTest.h)


configure_file(HugeJavaIn HugeJavaIn COPYONLY)

add_executable(qtests ${SOURCES})
target_link_libraries(qtests quarry)
