#ifndef __TRAWCKER_TESTSUITES__
#define __TRAWCKER_TESTSUITES__
#include <stdio.h>
#include <stdlib.h>
#include <check.h>

void quarry_addCommentTests(Suite *suite);

void quarry_addQuoteTests(Suite *suite);

void quarry_addNumbersTests(Suite *suite);

void quarry_addKeywordTests(Suite *suite);

void quarry_addIdentifierTests(Suite *suite);

void quarry_addReaderTests(Suite *suite);
#endif
