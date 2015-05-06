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

START_TEST (test_In0Ret0Test)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  size = 100;
  expLine = 1;
  printf("Running tests on comments.");
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0] = '/';
  input[1] = '*';
  input[90] = '*';
  input[91] = '/';
  for(index = 92;index < 100;index++){
    input[index] = 'a';
  }
  for(index = 2; index < 90;index++){
    if((index % 10) == 0){
      input[index] = '\n';
      expLine++;
    }
    else
      input[index] = 'e';
  }
  input[2] = '/';
  input[20] = '*'; expLine--;
  quarry = quarry_makeQuarry(input,size);
  quarry->col++;
 
  retval = quarry_commentLexer(quarry,0);
//quarry_printQuarry(quarry);
  if(retval != 0){
    printf("got return value of %d\n",retval);
    fail("expected 0");
  }
  if(quarry->slabType != quarry_Comment){
    fail("Expected comment");
  }
  fail_unless((quarry->line == expLine),"Line number did not match");
  fail_unless((quarry->col == 12),"Expected to be on 12th column"); 
  fail_unless((quarry->input.index == 92),"Expected to have index of 92");
  fail_unless((quarry->holder.length == 88),"Comment size does not match");
  fail_unless((quarry->holder.md == 0), "expected block comments");
}
END_TEST

START_TEST(test_In0Ret0TestLast)
{
  unsigned char *input;
  int size,index,retval;
  quarry_QuarryPtr quarry;
  printf(".");  size = 5;
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0] = '/';
  input[1] = '*';
  input[2] = '/';
  input[3] = '*';
  input[4] = '/';
  quarry = quarry_makeQuarry(input,size);
  quarry->input.index = 0;
  retval = quarry_commentLexer(quarry,0);
  fail_unless((retval == 0), "expected 0 ");
  fail_unless((quarry->line == 1),"expected 1");
  fail_unless((quarry->input.index == 5),"expected index of 5");
  fail_unless((quarry->slabType == quarry_Comment),"expected comment");
  fail_unless((quarry->holder.md == 0), "expected block comments");
}
END_TEST

START_TEST(test_In0Ret0Test1)
{
  unsigned char *input;
  int size,index,retval;
  quarry_QuarryPtr quarry;
  printf(".");  size = 6;
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0] = '/';
  input[1] = '*';
  input[2] = '/';
  input[3] = '*';
  input[4] = '/';
  input[5] = 'a';
  quarry = quarry_makeQuarry(input,size);
  quarry->input.index = 0;
  retval = quarry_commentLexer(quarry,0);
  
  fail_unless((retval == 0), "expected 0 ");
  fail_unless((quarry->line == 1),"expected 1");
  fail_unless((quarry->input.index == 5),"expected index of 5");
  fail_unless((quarry->slabType == quarry_Comment),"expected comment");
  fail_unless((quarry->holder.md == 0), "expected block comments");
}
END_TEST

START_TEST (test_In0Ret0MaxSlabLenTest)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  size = 100; printf(".");
  expLine = 1;
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0] = '/';
  input[1] = '*';
  input[90] = '*';
  input[91] = '/';
  for(index = 92;index < 100;index++){
    input[index] = 'a';
  }
  for(index = 2; index < 90;index++){
    if((index % 10) == 0){
      input[index] = '\n';
      expLine++;
    }
    else
      input[index] = 'e';
  }
  input[2] = '/';
  input[20] = '*'; expLine--;
  quarry = quarry_makeQuarry(input,size);
  quarry->col++;

  quarry->maxSlabLength = 21;
 
  retval = quarry_commentLexer(quarry,0);
  if(retval != 0){
    printf("got return value of %d\n",retval);
    fail("expected 0");
  }
  if(quarry->slabType != quarry_Comment){
    fail("Expected comment");
  }

  fail_unless((quarry->line == expLine),"Line number did not match");
  fail_unless((quarry->col == 12),"Expected to be on 12th column"); 
  fail_unless((quarry->input.index == 92),"Expected to have index of 92");
  fail_unless((quarry->holder.length == 20),"Comment size does not match");
  fail_unless((quarry->holder.md == 0), "expected block comments");
}
END_TEST

START_TEST (test_In0Ret3LastCharStar)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  size = 100; printf(".");
  expLine = 1;
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0] = '/';
  input[1] = '*';
  for(index = 2;index < 99;index++){
    input[index] = 'a';
  }
  for(index = 2; index < 99;index++){
    if((index % 10) == 0){
      input[index] = '\n';
      expLine++;
    }
  }
  input[99] = '*';
  quarry = quarry_makeQuarry(input,size);
  quarry->col++;

  retval = quarry_commentLexer(quarry,0);
  //quarry_printQuarry(quarry);
  fail_unless((quarry->line == expLine),"Line number did not match");
  fail_unless((quarry->input.index > 99),"Expected to have index of 100");
  fail_unless((quarry->holder.length == 98),"Comment size does not match");
  fail_unless((retval == 3),"Expect a return value of 3");
  fail_unless((quarry->holder.md == 0), "expected block comments");
}
END_TEST

START_TEST (test_In3Ret0Immediate)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  printf(".");
  size = 100;
  expLine = 1;
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0] = '/';
  input[1] = '*';
  for(index = 2;index < 99;index++){
    input[index] = 'a';
  }
  for(index = 2; index < 99;index++){
    if((index % 10) == 0){
      input[index] = '\n';
      expLine++;
    }
  }
  input[99] = '*';
  quarry = quarry_makeQuarry(input,size);
  quarry->col++;
  Quarry_AppendNext(quarry,'a');
  Quarry_AppendNext(quarry,'b');
  Quarry_AppendNext(quarry,'c');
  Quarry_AppendNext(quarry,'*');
  retval = quarry_commentLexer(quarry,3);
  //quarry_printQuarry(quarry);
  fail_unless((quarry->line == 1),"Line number did not match");
  fail_unless((quarry->input.index ==  1),"Expected to have index of 1");
  fail_unless((quarry->holder.length == 3),"Comment size does not match");
  fail_unless((retval == 0),"Expect a return value of 0");
  fail_unless((quarry->holder.data[0] == 'a'),"comment data does not match at 0");
  fail_unless((quarry->holder.data[1] == 'b'),"comment data does not match at 1");
  fail_unless((quarry->holder.data[2] == 'c'),"comment data does not match at 2");
  fail_unless((quarry->slabType == quarry_Comment),"expected a comment slab");
  fail_unless((quarry->holder.md == 0), "expected block comments");
}
END_TEST

START_TEST (test_In3Ret0Late)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  printf(".");
  size = 100;
  expLine = 10;
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  for(index = 0;index < 99;index++){
    input[index] = 'a';
    if((index % 10) == 0){
      input[index] = '\n';
      expLine++;
    }
  }

  input[98] = '*';
  input[99] = '/';
  quarry = quarry_makeQuarry(input,size);
  quarry->line  = 11;
  quarry->col++;
  Quarry_AppendNext(quarry,'a');
  Quarry_AppendNext(quarry,'b');
  Quarry_AppendNext(quarry,'c');
  Quarry_AppendNext(quarry,'*');
  retval = quarry_commentLexer(quarry,3);


  fail_unless((quarry->line == expLine),"Line number did not match");
  fail_unless((quarry->input.index == 100 ),"Expected to have index of 100");
  fail_unless((quarry->holder.length == 102),"Comment size does not match");
  fail_unless((retval == 0),"Expect a return value of 0");
  fail_unless((quarry->holder.data[0] == 'a'),"comment data does not match at 0");
  fail_unless((quarry->holder.data[1] == 'b'),"comment data does not match at 1");
  fail_unless((quarry->holder.data[2] == 'c'),"comment data does not match at 2");
  fail_unless((quarry->holder.data[3] == '*'),"comment data does not match at 3");
  fail_unless((quarry->slabType == quarry_Comment),"expected a comment slab");
  fail_unless((quarry->holder.md == 0), "expected block comments");
}
END_TEST

START_TEST (test_In0Ret2LastCharSlash)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  printf(".");
  size = 100;
  expLine = 1;
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0] = '/';
  input[1] = '*';
  for(index = 2;index < 99;index++){
    input[index] = 'a';
  }
  for(index = 2; index < 99;index++){
    if((index % 10) == 0){
      input[index] = '\n';
      expLine++;
    }
  }
  input[99] = '/';
  quarry = quarry_makeQuarry(input,size);
  quarry->col++;

  retval = quarry_commentLexer(quarry,0);
  //  quarry_printQuarry(quarry);
  fail_unless((quarry->line == expLine),"Line number did not match");
  fail_unless((quarry->input.index > 99),"Expected to have index of 100");
  fail_unless((quarry->holder.length == 98),"Comment size does not match");
  fail_unless((retval == 2),"Expect a return value of 2");
  fail_unless((quarry->holder.md == 0), "expected block comments");
}
END_TEST

START_TEST (test_In2Ret0FirstCharSlash)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  printf(".");
  size = 100;
  expLine = 1;
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0] = '/';
  
  for(index = 1;index < 99;index++){
    input[index] = 'a';
  }
  for(index = 2; index < 98;index++){
    if((index % 10) == 0){
      input[index] = '\n';
      expLine++;
    }
  }
  input[98] = '*';
  input[99] = '/';
  quarry = quarry_makeQuarry(input,size);

  retval = quarry_commentLexer(quarry,2);
  //  quarry_printQuarry(quarry);
  fail_unless((quarry->line == expLine),"Line number did not match");
  fail_unless((quarry->input.index > 99),"Expected to have index of 100");
  fail_unless((quarry->holder.length == 98),"Comment size does not match");
  fail_unless((retval == 0),"Expect a return value of 0");
  fail_unless((quarry->holder.md == 0), "expected block comments");
}
END_TEST

START_TEST(test_EmptyCommentIn0)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  printf(".");
  size = 100;
  expLine = 1;
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0]='/';
  input[1] = '*';
  input[2] = '*';
  input[3] = '/';
  for(index =4 ;index<100;index++)
    input[index] = 'a';
  quarry = quarry_makeQuarry(input,size);

  retval = quarry_commentLexer(quarry,0);

  fail_unless((quarry->line == expLine),"Line number did not match");
  fail_unless((quarry->slabType == quarry_Comment),"Expected comment");
  fail_unless((quarry->input.index == 4),"Expected to have index of 4");
  fail_unless((quarry->holder.length == 0),"Comment size does not match");
  fail_unless((retval == 0),"Expect a return value of 0");
  fail_unless((quarry->holder.md == 0), "expected block comments");
}
END_TEST

START_TEST(test_EmptyCommentIn1)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  printf(".");
  size = 100;
  expLine = 1;
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0]='*';
  input[1] = '*';
  input[2] = '/';
  for(index =3 ;index<100;index++)
    input[index] = 'a';
  quarry = quarry_makeQuarry(input,size);

  retval = quarry_commentLexer(quarry,1);
  //quarry_printQuarry(quarry);
  fail_unless((quarry->line == expLine),"Line number did not match");
  fail_unless((quarry->slabType == quarry_Comment),"Expected comment");
  fail_unless((quarry->input.index == 3),"Expected to have index of 3");
  fail_unless((quarry->holder.length == 0),"Comment size does not match");
  fail_unless((retval == 0),"Expect a return value of 0");
  fail_unless((quarry->holder.md == 0), "expected block comments");
}
END_TEST

START_TEST(test_EmptyCommentIn2)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  printf(".");
  size = 100;
  expLine = 1;
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0]='*';
  input[1] = '/';
  for(index =2 ;index<100;index++)
    input[index] = 'a';
  quarry = quarry_makeQuarry(input,size);

  retval = quarry_commentLexer(quarry,2);
  //quarry_printQuarry(quarry);

  fail_unless((quarry->line == expLine),"Line number did not match");
  fail_unless((quarry->slabType == quarry_Comment),"Expected comment");
  fail_unless((quarry->input.index == 2),"Expected to have index of 2");
  fail_unless((quarry->holder.length == 0),"Comment size does not match");
  fail_unless((retval == 0),"Expect a return value of 0");
  fail_unless((quarry->holder.md == 0), "expected block comments");
}
END_TEST

START_TEST(test_EmptyCommentIn3)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  printf(".");
  size = 100;
  expLine = 1;
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0]='/';

  for(index =1 ;index<100;index++)
    input[index] = 'a';
  quarry = quarry_makeQuarry(input,size);

  
  retval = quarry_commentLexer(quarry,3);
  //quarry_printQuarry(quarry);

  fail_unless((quarry->line == expLine),"Line number did not match");
  fail_unless((quarry->slabType == quarry_Comment),"Expected comment");
  fail_unless((quarry->input.index == 1),"Expected to have index of 1");
  fail_unless((retval == 0),"Expect a return value of 0");
  fail_unless((quarry->holder.md == 0), "expected block comments");
}
END_TEST

START_TEST(test_SingleLineComment)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  printf(".");
  size = 100;
  expLine = 2;
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0]='/';
  input[1] = '/';

  for(index =2 ;index<100;index++)
    input[index] = 'a';
  input[91] = '\n';
  quarry = quarry_makeQuarry(input,size);

  
  retval = quarry_commentLexer(quarry,0);
  //quarry_printQuarry(quarry);

  fail_unless((quarry->line == expLine),"Line number did not match");
  fail_unless((quarry->slabType == quarry_Comment),"Expected comment");
  fail_unless((quarry->holder.length == 89), "Comment size mismatch");
  fail_unless((quarry->input.index == 92), "Expected input.index is 92");
  fail_unless((quarry->holder.md == 1), "expected line comments");
}
END_TEST

START_TEST(test_LineCommentSize2)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  printf(".");
  size = 5;
  expLine = 2;
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0]='a';
  input[1] = '\n';

  for(index =2 ;index<size;index++)
    input[index] = 'a';
  quarry = quarry_makeQuarry(input,size);
  Quarry_AppendSingleChar(quarry,'a');
  
  retval = quarry_commentLexer(quarry,4);
  //quarry_printQuarry(quarry);
  fail_unless((quarry->line == expLine),"Line # mismatch");
  fail_unless((quarry->slabType == quarry_Comment),"Expected comment");
  fail_unless((quarry->holder.length == 2),"Expected size is 2");
  fail_unless((quarry->input.index == 2),"Expected input.index is 2");
  fail_unless((quarry->holder.md == 1), "expected line comments");
}
END_TEST

START_TEST(test_LineCommentNLFirst)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  printf(".");
  size = 5;
  expLine = 1;
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0]='\n';


  for(index =1 ;index<size;index++)
    input[index] = 'a';
  quarry = quarry_makeQuarry(input,size);
  Quarry_AppendNext(quarry,'a');
  Quarry_AppendNext(quarry,'b');
  quarry->holder.md= 1;
  retval = quarry_commentLexer(quarry,4);
  //  quarry_printQuarry(quarry);
  fail_unless((quarry->line == expLine),"Line # mismatch");
  fail_unless((quarry->slabType == quarry_Comment),"Expected comment");
  fail_unless((quarry->holder.length == 2),"Expected size is 2");
  fail_unless((quarry->input.index == 1),"Expected input.index is 2");
  fail_unless((quarry->holder.md == 1), "expected line comments");
}
END_TEST

START_TEST(test_EmptyLineComment)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  printf(".");
  size = 5;
  expLine = 2;
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0]='/';
  input[1] = '/';
  input[2] = '\n';
  input[3] = 'a';
  input[4] = 'b';



  quarry = quarry_makeQuarry(input,size);

  
  retval = quarry_commentLexer(quarry,0);
  // quarry_printQuarry(quarry);
  fail_unless((quarry->line == expLine),"Line # mismatch");
  fail_unless((quarry->slabType == quarry_Comment),"Expected comment");
  fail_unless((quarry->holder.length == 0),"Expected size is 0");
  fail_unless((quarry->input.index == 3),"Expected input.index is 3");
  fail_unless((quarry->holder.md == 1), "expected line comments");
}
END_TEST

CuSuite* quarry_commentTests()
{
    CuSuite *suite = CuSuiteNew();
    SUITE_ADD_TEST (suite,test_In0Ret0Test);
    SUITE_ADD_TEST (suite,test_In0Ret0Test1);
    SUITE_ADD_TEST (suite,test_In0Ret0TestLast);
    SUITE_ADD_TEST (suite,test_In0Ret0MaxSlabLenTest);
    SUITE_ADD_TEST (suite,test_In0Ret3LastCharStar);
    SUITE_ADD_TEST (suite,test_In0Ret2LastCharSlash);
    SUITE_ADD_TEST (suite,test_In3Ret0Immediate);
    SUITE_ADD_TEST (suite,test_In3Ret0Late);
    SUITE_ADD_TEST (suite,test_In2Ret0FirstCharSlash);
    SUITE_ADD_TEST (suite,test_EmptyCommentIn0);
    SUITE_ADD_TEST (suite,test_EmptyCommentIn1);
    SUITE_ADD_TEST (suite,test_EmptyCommentIn2);
    SUITE_ADD_TEST (suite,test_EmptyCommentIn3);
    
    SUITE_ADD_TEST (suite,test_EmptyLineComment);
    SUITE_ADD_TEST (suite,test_SingleLineComment);
    SUITE_ADD_TEST (suite,test_LineCommentSize2);
    SUITE_ADD_TEST (suite,test_LineCommentNLFirst);
    return suite;
}

