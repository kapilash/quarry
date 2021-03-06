#define BOOST_TEST_MODULE reader_tests test
#include <boost/test/unit_test.hpp>
#define QUARRY_EXPORT
#include "QReader.h"
#include <cstring>
#include <iostream>
BOOST_AUTO_TEST_CASE (simple_string)
{
    const char *str = "hello world";
    int len = std::strlen(str);
    unsigned char *uc = new unsigned char[len];

    for(int i=0; i< len; i++){
	uc[i] = str[i];
    }
    int initLine = 120;
    int initCol = 250;
    Quarry::QReader qr(uc, len, initLine , initCol );
    BOOST_CHECK(qr.hasMore());
    for (int i=0; i<100; i++) {
	BOOST_CHECK(qr.peekNext() == 'h' );
    }
    int count = 0;
    while (qr.hasMore()) {
	BOOST_CHECK(qr.next() == str[count]);
	count++;
    }
    BOOST_CHECK(count == len);
    BOOST_CHECK(initLine == qr.getLine());
    BOOST_CHECK( initCol == qr.getCol());
    delete []uc;
}

BOOST_AUTO_TEST_CASE (simple_file)
{
    Quarry::QReader qr("ReaderTest1");
    BOOST_CHECK(qr.hasMore());
    for (int i=0; i<100; i++) {
	BOOST_CHECK(qr.peekNext() == 'a' );
    }
    int aCount = 0;
    int bCount = 0;
    int cCount = 0;
    int lineCount = 0;
    while (qr.hasMore()) {
	char c = qr.next();
	if (c == 'a')
	    aCount++;
	if ('b' == c)
	    bCount++;
	if (c == 'c')
	    cCount++;
	if (c == '\n')
	    lineCount++;
    }
    BOOST_CHECK(lineCount == 2);
    BOOST_CHECK(3 == qr.getLine());
    BOOST_CHECK(1 == qr.getCol());
    BOOST_CHECK(aCount == 10);
    BOOST_CHECK(bCount == 10);
    BOOST_CHECK(cCount == 10);
}

BOOST_AUTO_TEST_CASE (oneMBfile)
{
    Quarry::QReader qr("HugeJavaIn");
    int count = 0;
    while(qr.hasMore()) {
      if(qr.next() != '\r')
	count++;
    }

    BOOST_CHECK(count == 127722);
    BOOST_CHECK(qr.getLine() == 3808);
}
