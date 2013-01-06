#include "quarry_testsuites.h"
#include "lexers.h"
#include "quarry_internal.h"


static char *testFileName = "HugeJavaIn";

START_TEST(test_readerNew)
{
  QReaderPtr reader;
  int index;

  printf("\nRunning tests on reader.");
  reader = (QReaderPtr)quarry_newReader(testFileName,quarry_Java);
  fail_unless((reader != NULL),"expected valid reader");
  fail_unless((reader->lexers != NULL),"expected valid lexers");
  fail_unless((reader->lexers[0] == &quarry_errLexer),"expected error lexer at 0");
  for(index = 1;index<256;index++){
    fail_unless((reader->lexers[index]  != NULL),"expected a valid lexer");
    fail_unless((reader->lexers[index] != &quarry_errLexer),"unexpected error lexer");
  }
  fail_unless((reader->quarry->kwTable != NULL),"expected a valid keyword table for java");

  quarry_closeReader(reader);
  
}
END_TEST

START_TEST(test_readGT5KB)
{
  QReaderPtr reader;
  int index;
  quarry_SlabPtr slabPtr;
  long l = 0;

  printf(".");
  reader = (QReaderPtr)quarry_newReader(testFileName,quarry_Java);
  while(1){
    slabPtr = quarry_read(reader);
    l++;
    if(slabPtr->slabType == quarry_EOF)
      break;
    quarry_freeSlab(slabPtr);
  }
  quarry_closeReader(reader);
}
END_TEST


void quarry_addReaderTests(Suite *suite)
{
  TCase *tc_core = tcase_create("reader");
  tcase_add_test (tc_core,test_readerNew);
  tcase_add_test (tc_core,test_readGT5KB);
  suite_add_tcase(suite,tc_core);
}
