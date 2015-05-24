#define BOOST_TEST_MODULE comments_test test
#include <boost/test/unit_test.hpp>

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
    commentsFile << "some random data";
}
BOOST_AUTO_TEST_CASE (simpleBlockComment)
{
    std::string str = std::string("abcdefghijklmnopqrstuvwxyz1234567890A");
    const char *testFile = "comments.txt";
    createFile(testFile, str, '/','*','/');

    Quarry::QReader qr(testFile);
    BOOST_CHECK(qr.hasMore());
    BOOST_CHECK('/' == qr.next());

    Quarry::BaseLexer *lexer = Quarry::getDblCharCommentLexer('/','*','/');
    BOOST_CHECK(lexer != nullptr);
    Quarry::QContext context(Quarry::C);
    auto slab = lexer->scan(qr, context);
    BOOST_CHECK(slab != nullptr);
    BOOST_CHECK((str.length()+1) == slab->line);
    BOOST_CHECK(qr.getLine() == slab->line);
    BOOST_CHECK(qr.getCol() == slab->col);
    BOOST_CHECK(slab->slabType == quarry_Comment);
    BOOST_CHECK(slab->slabLength == (str.length() * 101));
    std::cout << "{line = " << slab->line << "; column="<< slab->col << "; length=" << slab->slabLength << "; type=" << slab->slabType << std::endl;
}

BOOST_AUTO_TEST_CASE (schemeBlockComment)
{
    std::string str = std::string("abcdefghijklmnopqrstuvwxyz1234567890A");
    const char *testFile = "comments.txt";
    createFile(testFile, str, '#','|','#');

    Quarry::QReader qr(testFile);
    BOOST_CHECK(qr.hasMore());
    BOOST_CHECK('#' == qr.next());

    Quarry::BaseLexer *lexer = Quarry::getDblCharCommentLexer('#','|','#');
    BOOST_CHECK(lexer != nullptr);
    Quarry::QContext context(Quarry::C);
    auto slab = lexer->scan(qr, context);
    BOOST_CHECK(slab != nullptr);
    BOOST_CHECK((str.length()+1) == slab->line);
    BOOST_CHECK(qr.getLine() == slab->line);
    BOOST_CHECK(qr.getCol() == slab->col);
    BOOST_CHECK(slab->slabType == quarry_Comment);
    BOOST_CHECK(slab->slabLength == (str.length() * 101));
    std::cout << "{line = " << slab->line << "; column="<< slab->col << "; length=" << slab->slabLength << "; type=" << slab->slabType << std::endl;
}

BOOST_AUTO_TEST_CASE (nestedBlockComment)
{
    std::string str = std::string("abcdefghi#|jklm#|n|#o|#pqrstuvwxyz1234567890A");
    const char *testFile = "comments.txt";
    createFile(testFile, str, '#','|','#');

    Quarry::QReader qr(testFile);
    BOOST_CHECK(qr.hasMore());
    BOOST_CHECK('#' == qr.next());

    Quarry::BaseLexer *lexer = Quarry::getDblCharCommentLexer('#','|','#');
    int expLength = str.length() * 101; // 100 for each character and 1 for the new line. so 101 per character.
    expLength = expLength - 8; // 8 of the lines will not contain new-lines.
    BOOST_CHECK(lexer != nullptr);
    Quarry::QContext context(Quarry::C);
    auto slab = lexer->scan(qr, context);
    BOOST_CHECK(slab != nullptr);
    BOOST_CHECK(qr.getCol() == slab->col);
    BOOST_CHECK(slab->slabLength == expLength);
    std::cout << "{line = " << slab->line << "; column="<< slab->col << "; length=" << slab->slabLength << "; type=" << slab->slabType << std::endl;
}
