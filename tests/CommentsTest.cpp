#define BOOST_TEST_MODULE comments_test test
#include <boost/test/unit_test.hpp>
#define QUARRY_EXPORT
#include "QReader.h"
#include "QInternal.h"
#include <string>
#include <fstream>
#include <iostream>
#include <set>
#include <algorithm>
#include <cstdlib>
#include <locale>

static void createFile(const char *fileName, const std::string &str, char begin, char second, char end) {
    std::ofstream commentsFile(fileName);
    commentsFile << begin << second;
    int line = 0;
    for(auto it=str.begin(); it != str.end(); ++it){
	for(int i=0; i<100; i++) {
	    commentsFile << (*it);
	}
	if (line % 7 == 0) {
	    commentsFile << '\r';
	}
	if(((*it) != begin) && ((*it) != second) && ((*it) != end))
	    commentsFile << (std::endl);
	line++;
    }
    commentsFile << second << end ;
    commentsFile << "XXXXXXXXXX";
}
BOOST_AUTO_TEST_CASE (simpleBlockComment)
{
    std::string str = std::string("abcdefghijklmnopqrstuvwxyz1234567890A");
    const char *testFile = "comments1.txt";
    createFile(testFile, str, '/','*','/');

    Quarry::QReader qr(testFile);
    BOOST_CHECK(qr.hasMore());

    Quarry::Lexer comments = Quarry::csComments;
    Quarry::QContext context(Quarry::C);
    auto slab = comments(qr, context);
    BOOST_CHECK(slab != nullptr);
    BOOST_CHECK(slab->tokenType == Quarry::COMMENT);
    std::string leftOver;
    while(qr.hasMore()) {
      leftOver.append(1, qr.next());
    }
    BOOST_CHECK_EQUAL(leftOver.c_str(), "XXXXXXXXXX");
    delete slab;
}

BOOST_AUTO_TEST_CASE (nestedBlockComment)
{
    std::string str = std::string("abcdefghi/*jklm/*n*/oo*/pqrstuvwxyz1234567890A");
    const char *testFile = "comments3.txt";
    createFile(testFile, str, '/','*','/');

    Quarry::QReader qr(testFile);
    BOOST_CHECK(qr.hasMore());

    Quarry::Lexer comments = Quarry::csComments;
    Quarry::QContext context(Quarry::C);
    auto slab = comments(qr, context);
    BOOST_CHECK(slab != nullptr);
    //BOOST_CHECK(slab->value.length() == 4607);
    BOOST_CHECK(slab->tokenType == Quarry::COMMENT);
    std::string leftOver;
    while(qr.hasMore()) {
      leftOver.append(1, qr.next());
    }
    BOOST_CHECK_EQUAL(leftOver.c_str(), "XXXXXXXXXX");
    slab->writeTo(std::cout);
    delete slab;
}


BOOST_AUTO_TEST_CASE(java_comments)
{
    Quarry::QReader qr("ManualComments.txt");
    Quarry::QContext context(Quarry::C);
    Quarry::Lexer spaces = Quarry::spaceLexer;
    Quarry::Lexer comments = Quarry::csComments;
    int commentCount = 0;

    while(qr.hasMore()) {
	auto slab = comments(qr, context);
	BOOST_CHECK(slab != nullptr);
	BOOST_CHECK(slab->tokenType == Quarry::COMMENT);
	slab->writeTo(std::cout);
	commentCount++;
	delete spaces(qr, context);
	delete slab;
    }

    std::cout << "validated " << commentCount << " comments " << std::endl;
}

