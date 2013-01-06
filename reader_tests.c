/*
Copyright (c) 2013, Hemanth Kapila
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1) Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2) Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
The name of the Hemanth Kapila may be used to endorse or promote products derived from this software without specific prior written permission.
3) THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
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
