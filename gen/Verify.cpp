#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MODULE keywords_verify test
#include <boost/test/unit_test.hpp>
#include <fstream>
#include <string>
#include <iostream>
#include <vector>
#include <set>
#include <algorithm>
#include <cstdlib>
#include <locale>

void verify (const char *fileName) {
    std::ifstream inFile(fileName);
    std::set<std::string> leftSet;
    std::set<std::string> rightSet;

    std::string left;
    std::string right;
    int c = 0;
    BOOST_REQUIRE(inFile.is_open());
    while (inFile >> left >> right) {
	auto itL = leftSet.find(left);
	if (itL != leftSet.end()) {
	    BOOST_ERROR("duplicate " << left );
	}
	auto itR = rightSet.find(right);
	if (itR != rightSet.end() ) {
	    BOOST_ERROR("duplicate right " << right);
	}
	
	leftSet.insert(left);
	rightSet.insert(right);
	++c;
    }
    BOOST_CHECK(c == leftSet.size());
    BOOST_CHECK(c == rightSet.size());
}


BOOST_AUTO_TEST_CASE (ckeywords_are_unique)
{
    verify("CKeywords");
}
BOOST_AUTO_TEST_CASE (coperators_are_unique)
{
    verify("COperators");
}
BOOST_AUTO_TEST_CASE (javaoperators_are_unique)
{
    verify("JavaOperators");
}
BOOST_AUTO_TEST_CASE (javakeywords_are_unique)
{
    verify("JavaKeywords");
}
