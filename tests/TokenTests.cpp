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
#include <boost/lexical_cast.hpp>

BOOST_AUTO_TEST_CASE(many_numbers)
{
    Quarry::QReader qr("Numbers.txt");
    Quarry::QContext context(Quarry::C);
    Quarry::Lexer spaces = Quarry::spaceLexer;
    Quarry::Lexer numbers = Quarry::numberLexer;
    int count = 0;

    while(qr.hasMore()) {
	auto slab = numbers(qr, context);
	BOOST_CHECK(slab != nullptr);
	BOOST_CHECK(slab->tokenType == Quarry::NUMBER);
	if (slab->tokenType == Quarry::NUMBER) {
	    Quarry::NumericalToken *nt = dynamic_cast<Quarry::NumericalToken *>(slab);
	    BOOST_CHECK(nt != nullptr);
	    BOOST_CHECK(nt->text.length() > 0);

	    switch(nt->numberType) {
	    case Quarry::QLongDouble : 
		BOOST_CHECK(dynamic_cast<Quarry::LDblToken *>(slab) != nullptr);
		break;
	    case Quarry::QDouble : 
		BOOST_CHECK(dynamic_cast<Quarry::DblToken *>(slab) != nullptr);
		break;
	    case Quarry::QFloat :
		BOOST_CHECK(dynamic_cast<Quarry::FlToken *>(slab) != nullptr);
		break;
	    case Quarry::QUnsignedInt :
		BOOST_CHECK(dynamic_cast<Quarry::UIntToken *>(slab) != nullptr);
		break;
	    case Quarry::QUnsignedLong :
		BOOST_CHECK(dynamic_cast<Quarry::ULongToken *>(slab) != nullptr);
		break;
	    case Quarry::QUnsignedLongLong :
		BOOST_CHECK(dynamic_cast<Quarry::ULongLongToken *>(slab) != nullptr);
		break;
	    case Quarry::QLongLong :
		BOOST_CHECK(dynamic_cast<Quarry::LongLongToken *>(slab) != nullptr);
		break;
	    case Quarry::QLong :
		BOOST_CHECK(dynamic_cast<Quarry::LongToken *>(slab) != nullptr);
		break;
	    case Quarry::QInt :
		BOOST_CHECK(dynamic_cast<Quarry::IntToken *>(slab) != nullptr);
		break;

	    default:
		std::cout << "unknown numberType " << nt->numberType << std::endl;
	    }
	}
	// Quarry::printSlab(slab, count);
	//	std::cout << count << ":{line = " << slab->line << "; column="<< slab->col << "; length=" << slab->slabLength << "; type=" << slab->slabType << "; text=" << slab->data <<"}" << std::endl ;
	count++;
	delete spaces(qr, context);
	delete slab;
    }
    std::cout << "validated " << count  << " numbers " << std::endl;
}


BOOST_AUTO_TEST_CASE(many_chars)
{
    Quarry::QReader qr("Chars.txt");
    Quarry::QContext context(Quarry::C);
    Quarry::Lexer spaces = Quarry::spaceLexer;
    Quarry::Lexer nextChar = Quarry::charLexer;

    int count = 0;
    while(qr.hasMore()) {
	Quarry::CharToken *slab = dynamic_cast<Quarry::CharToken *>(nextChar(qr, context));
	//	std::cout << slab->tokenType  << " with " << slab->value << " at " << slab->line << "," << slab->column << std::endl;
	BOOST_CHECK(slab != nullptr);
	BOOST_CHECK(slab->tokenType == Quarry::CHAR);
	BOOST_CHECK(slab->value >= 0);

	count++;
	delete spaces(qr, context);
	delete slab;
    }
    std::cout << "validated " << count  << " characters " << std::endl;
}

BOOST_AUTO_TEST_CASE(str_literals)
{
    Quarry::QReader qr("Strings.txt");
    Quarry::QContext context(Quarry::C);
    Quarry::Lexer spaces = Quarry::spaceLexer;
    Quarry::Lexer nextString = Quarry::cstringLexer;

    int count = 0;
    while(qr.hasMore()) {
      	Quarry::StringToken *slab = dynamic_cast<Quarry::StringToken *>(nextString(qr, context));
	BOOST_CHECK(slab != nullptr);
	BOOST_CHECK(slab->tokenType == Quarry::STRING);
	BOOST_CHECK(slab->value.length() >= 0);

	count++;
	delete spaces(qr, context);
	delete slab;
    }
    std::cout << "validated " << count  << " string literals " << std::endl;
}

BOOST_AUTO_TEST_CASE(idents_and_keywords)
{
    Quarry::QReader qr("Idents.txt");
    Quarry::QContext context(Quarry::C);
    Quarry::Lexer spaces = Quarry::spaceLexer;
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
	delete spaces(qr, context);
	
	auto ident1 = strings.scan(qr, context);
	BOOST_CHECK(ident1 != nullptr);
	BOOST_CHECK(ident1->slabType == quarry_Identifier);
	BOOST_CHECK(ident1->slabLength != 0);
	BOOST_CHECK(ident1->data != nullptr);
	//Quarry::printSlab(ident1, count);
	//	std::cout << count << ":{line = " << ident1->line << "; column="<< ident1->col << "; length=" << ident1->ident1Length << "; type=" << ident1->ident1Type << "; text=" << ident1->data <<"}" << std::endl ;
	count++;
	delete spaces(qr, context);
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
	delete spaces(qr, context);
	delete [](ident2->data);
	delete ident2;
    }
}


