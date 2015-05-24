#define BOOST_TEST_MODULE reader_tests test
#include <boost/test/unit_test.hpp>

#include "QReader.h"
#include <cstring>
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
	qr.incrementLine();
    }
    int count = 0;
    while (qr.hasMore()) {
	BOOST_CHECK(qr.next() == str[count]);
	count++;
    }
    BOOST_CHECK(count == len);
    BOOST_CHECK((initLine + 100) == qr.getLine());
    BOOST_CHECK((len + 1) == qr.getCol());
}

BOOST_AUTO_TEST_CASE (simple_file)
{
    Quarry::QReader qr("ReaderTest1");
    BOOST_CHECK(!qr.hasMore());
    BOOST_CHECK(qr.read());
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
    BOOST_CHECK(aCount == 10);
    BOOST_CHECK(bCount == 10);
    BOOST_CHECK(cCount == 10);
}
