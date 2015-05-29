#define BOOST_TEST_MODULE numbers_test test
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
	//	BOOST_CHECK(slab->data == nullptr);
	//	std::cout << count << ":{line = " << slab->line << "; column="<< slab->col << "; length=" << slab->slabLength << "; type=" << slab->slabType << "; text=" << slab->data <<"}" << std::endl ;
	count++;
	spaceLexer.scan(qr, context);
    }
    BOOST_CHECK(count == 28);
}
