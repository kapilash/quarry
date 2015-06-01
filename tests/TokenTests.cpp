#define BOOST_TEST_MODULE misc_test
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


BOOST_AUTO_TEST_CASE(many_numbers)
{
    Quarry::QReader qr("Numbers.txt");
    Quarry::QContext context(Quarry::C);
    Quarry::SkipSpace spaceLexer;
    Quarry::NumberLexer numbers;
    int count = 0;
    while(qr.hasMore()) {
	auto slab = numbers.scan(qr, context);
	BOOST_CHECK(slab != nullptr);
	BOOST_CHECK(slab->slabType == quarry_Numbers);
	BOOST_CHECK(slab->slabLength != 0);
	BOOST_CHECK(slab->data != nullptr);
	// Quarry::printSlab(slab, count);
	//	std::cout << count << ":{line = " << slab->line << "; column="<< slab->col << "; length=" << slab->slabLength << "; type=" << slab->slabType << "; text=" << slab->data <<"}" << std::endl ;
	count++;
	auto l = spaceLexer.scan(qr, context);
	delete l;
	delete [](slab->data);
	delete slab;
    }
    BOOST_CHECK(count == 32);
}


BOOST_AUTO_TEST_CASE(many_chars)
{
    Quarry::QReader qr("Chars.txt");
    Quarry::QContext context(Quarry::C);
    Quarry::SkipSpace spaceLexer;
    Quarry::DelimitedLexer strings('\'','\\', quarry_Char);
    int count = 0;
    while(qr.hasMore()) {
	auto slab = strings.scan(qr, context);
	BOOST_CHECK(slab != nullptr);
	BOOST_CHECK(slab->slabType == quarry_Char);
	BOOST_CHECK(slab->slabLength != 0);
	BOOST_CHECK(slab->data != nullptr);
	//Quarry::printSlab(slab, count);
	//	std::cout << count << ":{line = " << slab->line << "; column="<< slab->col << "; length=" << slab->slabLength << "; type=" << slab->slabType << "; text=" << slab->data <<"}" << std::endl ;
	count++;
	delete spaceLexer.scan(qr, context);
	delete [](slab->data);
	delete slab;
    }
    BOOST_CHECK(count == 9);
}

BOOST_AUTO_TEST_CASE(idents_and_keywords)
{
    Quarry::QReader qr("Idents.txt");
    Quarry::QContext context(Quarry::C);
    Quarry::SkipSpace spaceLexer;
    Quarry::CIdentifier strings;
    int count = 0;
    while(qr.hasMore()) {
	auto kw = strings.scan(qr, context);
	BOOST_CHECK(kw != nullptr);
	BOOST_CHECK(kw->slabType == quarry_Keyword);
	BOOST_CHECK(kw->slabLength == 0);
	BOOST_CHECK(kw->data == nullptr);
	//Quarry::printSlab(kw, count);
	//	std::cout << count << ":{line = " << kw->line << "; column="<< kw->col << "; length=" << kw->kwLength << "; type=" << kw->kwType << "; text=" << kw->data <<"}" << std::endl ;
	count++;
	delete kw;
	delete spaceLexer.scan(qr, context);
	
	auto ident1 = strings.scan(qr, context);
	BOOST_CHECK(ident1 != nullptr);
	BOOST_CHECK(ident1->slabType == quarry_Identifier);
	BOOST_CHECK(ident1->slabLength != 0);
	BOOST_CHECK(ident1->data != nullptr);
	//Quarry::printSlab(ident1, count);
	//	std::cout << count << ":{line = " << ident1->line << "; column="<< ident1->col << "; length=" << ident1->ident1Length << "; type=" << ident1->ident1Type << "; text=" << ident1->data <<"}" << std::endl ;
	count++;
	delete spaceLexer.scan(qr, context);
	delete [](ident1->data);
	delete ident1;

	auto ident2 = strings.scan(qr, context);
	BOOST_CHECK(ident2 != nullptr);
	BOOST_CHECK(ident2->slabType == quarry_Identifier);
	BOOST_CHECK(ident2->slabLength != 0);
	BOOST_CHECK(ident2->data != nullptr);
	//Quarry::printSlab(ident2, count);
	//	std::cout << count << ":{line = " << ident2->line << "; column="<< ident2->col << "; length=" << ident2->ident2Length << "; type=" << ident2->ident2Type << "; text=" << ident2->data <<"}" << std::endl ;
	count++;
	delete spaceLexer.scan(qr, context);
	delete [](ident2->data);
	delete ident2;
    }
}


