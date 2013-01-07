/*
Copyright (c) 2013, Hemanth Kapila
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1) Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2) Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
The name of the Hemanth Kapila may NOT be used to endorse or promote products derived from this software without specific prior written permission.
3) THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
#include "quarry_testsuites.h"
#include "lexers.h"
#include "quarry_internal.h"


START_TEST(test_identifier1)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  size = 17;

  printf("\nRunning tests on identifier.");
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0] = 'x';
  for(index = 1;index<10;index++){
    input[index] = 47+index;
  }
  input[10]='A';
  input[11] = 'a';
  input[12] = 'C';
  input[13] = 'Z';
  input[14] = '_';
  input[15] = 'z';
  input[16] = '0';

  quarry = quarry_makeQuarry(input,size);
  quarry->input.index = 0;

  retval = quarry_idLexer(quarry,0);
  //quarry_printQuarry(quarry);
  fail_unless((retval != 0),"expected non-zero");
  printf(".");
  quarry->input.index = 0;
  quarry->input.data[3]=';';
  retval = quarry_idLexer(quarry,retval);
  //quarry_printQuarry(quarry);
  fail_unless((retval == 0),"expected zero");
  fail_unless((quarry->slabType == quarry_Identifier),"Expected identifier");
  fail_unless((quarry->holder.length == 20),"Expected size is 20");
  fail_unless((quarry->input.index == 3),"Expected input.index is 3");
  free(input);
}
END_TEST

START_TEST(test_identifierKW)
{
  unsigned char *input;
  int size,index,retval,expLine,currKWsize;
  quarry_QuarryPtr quarry;
  size = 20;

  printf("\nRunning tests on identifier.");
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  for(index = 0;index<size;index++){
    input[index] = ' ';
  }
  currKWsize = strlen("abstract");
  memcpy(input,"abstract",currKWsize);
  quarry = quarry_makeQuarry(input,size);
  quarry->input.index = 0;
  quarry->kwTable = quarry_util_keywordTableJava();

  retval = quarry_idLexer(quarry,0);
  //  quarry_printQuarry(quarry);
  fail_unless((retval == 0),"expected zero");
  fail_unless((quarry->slabType == quarry_Keyword),"Expected keyword");
  fail_unless((quarry->holder.length == currKWsize),"Expected size is currKWsize");
  free(input);
}
END_TEST


void quarry_addIdentifierTests(Suite *suite)
{
  TCase *tc_core = tcase_create("identifiers");
  tcase_add_test (tc_core,test_identifier1);
  tcase_add_test (tc_core,test_identifierKW);
  suite_add_tcase(suite,tc_core);
}
