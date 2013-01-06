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


START_TEST(test_DecimalsWithUnderscore)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  size = 5;

  printf("\nRunning tests on numbers.");
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0]='1';
  input[1] = '2';
  input[2] = '_';
  input[3] = '4';
  input[4] = ',';


  quarry = quarry_makeQuarry(input,size);
  retval = quarry_decimalLexer(quarry,0);
  fail_unless((quarry->line == 1),"Line # mismatch");
  fail_unless((quarry->slabType == quarry_Numbers),"Expected numbers");
  fail_unless((quarry->holder.length == 4),"Expected size is 4");
  fail_unless((quarry->holder.data[2] == '_'),"Expected _ at 3");
  fail_unless((quarry->input.index == 4),"Expected input.index is 4");

}
END_TEST

START_TEST(test_DecimalsEndBlock)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  size = 5;

  printf(".");
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0]='1';
  input[1] = '2';
  input[2] = '3';
  input[3] = '4';
  input[4] = '_';


  quarry = quarry_makeQuarry(input,size);
  retval = quarry_decimalLexer(quarry,0);
  fail_unless((quarry->line == 1),"Line # mismatch");
  fail_unless((quarry->slabType == quarry_Error),"Expected error");
  fail_unless((quarry->holder.length == 5),"Expected size is 5");
  fail_unless((quarry->holder.data[4] == '_'),"Expected _ at 4");
  fail_unless((quarry->input.index == 5),"Expected input.index is 5");

}
END_TEST


START_TEST(test_DecimalEndWithL)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  size = 5;

  printf(".");
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0]='1';
  input[1] = '2';
  input[2] = '_';
  input[3] = '4';
  input[4] = 'L';


  quarry = quarry_makeQuarry(input,size);
  retval = quarry_decimalLexer(quarry,0);
  fail_unless((quarry->line == 1),"Line # mismatch");
  fail_unless((quarry->slabType == quarry_Numbers),"Expected numbers");
  fail_unless((quarry->holder.length == 5),"Expected size is 5");
  fail_unless((quarry->holder.data[2] == '_'),"Expected _ at 3");
  fail_unless((quarry->input.index == 5),"Expected input.index is 5");
}
END_TEST

START_TEST(test_DecimalEndWithl)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  size = 5;

  printf(".");
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0]='1';
  input[1] = '2';
  input[2] = '_';
  input[3] = '4';
  input[4] = 'l';


  quarry = quarry_makeQuarry(input,size);
  retval = quarry_decimalLexer(quarry,0);
  fail_unless((quarry->line == 1),"Line # mismatch");
  fail_unless((quarry->slabType == quarry_Numbers),"Expected numbers");
  fail_unless((quarry->holder.length == 5),"Expected size is 5");
  fail_unless((quarry->holder.data[2] == '_'),"Expected _ at 3");
  fail_unless((quarry->input.index == 5),"Expected input.index is 5");
}
END_TEST

START_TEST(test_BinaryTest)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  size = 5;

  printf(".");
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0]='b';
  input[1] = '1';
  input[2] = '0';
  input[3] = '0';
  input[4] = '0';

  quarry = quarry_makeQuarry(input,size);
  quarry->input.index = 4;

  retval = quarry_hexBinOct0Lexer(quarry,0);
  //quarry_printQuarry(quarry);
  fail_unless((retval != 0),"expected non-zero");
  quarry->input.index = 0;
  retval = quarry_hexBinOct0Lexer(quarry,retval);
  //quarry_printQuarry(quarry);
  fail_unless((retval != 0),"expected non-zero");
  input[0] = '1';
  input[1] = 'L';
  quarry->input.index = 0;
  retval = quarry_hexBinOct0Lexer(quarry,retval);
  //quarry_printQuarry(quarry);
  fail_unless((retval == 0),"expected zero");
  fail_unless((quarry->slabType == quarry_Numbers),"Expected numbers");
  fail_unless((quarry->holder.length == 8),"Expected size is 8");
  fail_unless((quarry->holder.data[7] == 'L'),"Expected L at 7");
  fail_unless((quarry->input.index == 2),"Expected input.index is 2");
}
END_TEST


START_TEST(test_Octal)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  size = 5;

  printf(".");
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0]='2';
  input[1] = '3';
  input[2] = '7';
  input[3] = '1';
  input[4] = '0';

  quarry = quarry_makeQuarry(input,size);
  quarry->input.index = 4;

  retval = quarry_hexBinOct0Lexer(quarry,0);
  //quarry_printQuarry(quarry);
  fail_unless((retval != 0),"expected non-zero");
  quarry->input.index = 0;
  retval = quarry_hexBinOct0Lexer(quarry,retval);
  //quarry_printQuarry(quarry);
  fail_unless((retval != 0),"expected non-zero");
  input[0] = '_';
  input[1] = '3';
  input[2] = '_';
  input[3] = '9';
  quarry->input.index = 0;
  retval = quarry_hexBinOct0Lexer(quarry,retval);
  //quarry_printQuarry(quarry);
  fail_unless((retval == 0),"expected zero");
  fail_unless((quarry->slabType == quarry_Numbers),"Expected numbers");
  fail_unless((quarry->holder.length == 9),"Expected size is 9");
  fail_unless((quarry->holder.data[8] == '_'),"Expected L at 7");
  fail_unless((quarry->input.index == 3),"Expected input.index is 3");
}
END_TEST


START_TEST(test_Hexal)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  size = 17;

  printf(".");
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0] = 'x';
  for(index = 1;index<10;index++){
    input[index] = 47+index;
  }
  input[10]='A';
  input[11] = 'B';
  input[12] = 'C';
  input[13] = 'D';
  input[14] = 'E';
  input[15] = 'F';
  input[16] = '0';

  quarry = quarry_makeQuarry(input,size);
  quarry->input.index = (size-1);

  retval = quarry_hexBinOct0Lexer(quarry,0);
  //quarry_printQuarry(quarry);
  fail_unless((retval != 0),"expected non-zero");
  quarry->input.index = 0;
  retval = quarry_hexBinOct0Lexer(quarry,retval);
  //quarry_printQuarry(quarry);
  fail_unless((retval != 0),"expected non-zero");
  input[0] = '_';
  input[1] = 'a';
  input[2] = 'b';
  input[3] = 'c';
  input[4] = 'd';
  input[5] = 'e';
  input[6] = 'f';
  input[7] = 'G';
  quarry->input.index = 0;
  retval = quarry_hexBinOct0Lexer(quarry,retval);
  //quarry_printQuarry(quarry);
  fail_unless((retval == 0),"expected zero");
  fail_unless((quarry->slabType == quarry_Numbers),"Expected numbers");
  fail_unless((quarry->holder.length == 25),"Expected size is 25");
  fail_unless((quarry->holder.data[18] == '_'),"Expected _ at 18");
  fail_unless((quarry->input.index == 7),"Expected input.index is 7");
}
END_TEST

START_TEST(test_HexalWithL)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  size = 17;

  printf(".");
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0] = 'x';
  for(index = 1;index<10;index++){
    input[index] = 47+index;
  }
  input[10]='A';
  input[11] = 'B';
  input[12] = 'C';
  input[13] = 'D';
  input[14] = 'E';
  input[15] = 'F';
  input[16] = '0';

  quarry = quarry_makeQuarry(input,size);
  quarry->input.index = (size-1);

  retval = quarry_hexBinOct0Lexer(quarry,0);
  //quarry_printQuarry(quarry);
  fail_unless((retval != 0),"expected non-zero");
  quarry->input.index = 0;
  retval = quarry_hexBinOct0Lexer(quarry,retval);
  //quarry_printQuarry(quarry);
  fail_unless((retval != 0),"expected non-zero");
  input[0] = '_';
  input[1] = 'a';
  input[2] = 'b';
  input[3] = 'c';
  input[4] = 'd';
  input[5] = 'e';
  input[6] = 'f';
  input[7] = 'l';
  quarry->input.index = 0;
  retval = quarry_hexBinOct0Lexer(quarry,retval);
  //quarry_printQuarry(quarry);
  fail_unless((retval == 0),"expected zero");
  fail_unless((quarry->slabType == quarry_Numbers),"Expected numbers");
  fail_unless((quarry->holder.length == 26),"Expected size is 26");
  fail_unless((quarry->holder.data[18] == '_'),"Expected _ at 18");
  fail_unless((quarry->holder.data[25] == 'l'),"Expected _ at 25");
  fail_unless((quarry->input.index == 8),"Expected input.index is 8");
}
END_TEST
void quarry_addNumbersTests(Suite *suite)
{
  TCase *tc_core = tcase_create("numbers");
  tcase_add_test (tc_core,test_DecimalsWithUnderscore);
  tcase_add_test (tc_core,test_DecimalEndWithL);
  tcase_add_test (tc_core,test_DecimalEndWithl);
  tcase_add_test (tc_core,test_DecimalsEndBlock);
  tcase_add_test (tc_core,test_BinaryTest);
  tcase_add_test (tc_core,test_Octal);
  tcase_add_test (tc_core,test_Hexal);
  tcase_add_test (tc_core,test_HexalWithL);


  suite_add_tcase(suite,tc_core);
}

