/*
Copyright (c) 2013, Hemanth Kapila
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1) Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2) Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
The name of the Hemanth Kapila may NOT be used to endorse or promote products derived from this software without specific prior written permission.
3) THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
#include "quarry_kw.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


void fail_unless(int i,const char *str){
  if(i){
    printf("succeeded: %s\n",str);
  }else{
    printf("failed: %s\n",str);
  }
}

void  test_KeywordsPositive()
{
  unsigned char input[20];
  int currKWsize,size,index,retval,expLine;
  qu_KWTablePtr kwTable;

  printf("\nRunning tests on keywords.");
  kwTable = quarry_util_keywordTableJava();

  for(index =0;index<20;index++){
    input[index] = 'a';
  }
  currKWsize = strlen("abstract");
  memcpy(input,"abstract",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"abstract is a keyword");
  currKWsize = strlen("assert");
  memcpy(input,"assert",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"assert is a keyword");
  currKWsize = strlen("boolean");
  memcpy(input,"boolean",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"boolean is a keyword");
  currKWsize = strlen("break");
  memcpy(input,"break",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"break is a keyword");
  currKWsize = strlen("byte");
  memcpy(input,"byte",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"byte is a keyword");
  currKWsize = strlen("case");
  memcpy(input,"case",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"case is a keyword");
  currKWsize = strlen("catch");
  memcpy(input,"catch",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"catch is a keyword");
  currKWsize = strlen("char");
  memcpy(input,"char",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"char is a keyword");
  currKWsize = strlen("class");
  memcpy(input,"class",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"class is a keyword");
  currKWsize = strlen("const");
  memcpy(input,"const",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"const is a keyword");
  currKWsize = strlen("continue");
  memcpy(input,"continue",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"continue is a keyword");
  currKWsize = strlen("default");
  memcpy(input,"default",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"default is a keyword");
  currKWsize = strlen("do");
  memcpy(input,"do",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"do is a keyword");
  currKWsize = strlen("double");
  memcpy(input,"double",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"double is a keyword");
  currKWsize = strlen("else");
  memcpy(input,"else",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"else is a keyword");
  currKWsize = strlen("enum");
  memcpy(input,"enum",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"enum is a keyword");
  currKWsize = strlen("extends");
  memcpy(input,"extends",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"extends is a keyword");
  currKWsize = strlen("final");
  memcpy(input,"final",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"final is a keyword");
  currKWsize = strlen("finally");
  memcpy(input,"finally",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"finally is a keyword");
  currKWsize = strlen("float");
  memcpy(input,"float",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"float is a keyword");
  currKWsize = strlen("for");
  memcpy(input,"for",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"for is a keyword");
  currKWsize = strlen("if");
  memcpy(input,"if",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"if is a keyword");
  currKWsize = strlen("goto");
  memcpy(input,"goto",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"goto is a keyword");
  currKWsize = strlen("implements");
  memcpy(input,"implements",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"implements is a keyword");
  currKWsize = strlen("import");
  memcpy(input,"import",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"import is a keyword");
  currKWsize = strlen("instanceof");
  memcpy(input,"instanceof",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"instanceof is a keyword");
  currKWsize = strlen("int");
  memcpy(input,"int",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"int is a keyword");
  currKWsize = strlen("interface");
  memcpy(input,"interface",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"interface is a keyword");
  currKWsize = strlen("long");
  memcpy(input,"long",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"long is a keyword");
  currKWsize = strlen("native");
  memcpy(input,"native",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"native is a keyword");
  currKWsize = strlen("new");
  memcpy(input,"new",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"new is a keyword");
  currKWsize = strlen("package");
  memcpy(input,"package",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"package is a keyword");
  currKWsize = strlen("private");
  memcpy(input,"private",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"private is a keyword");
  currKWsize = strlen("protected");
  memcpy(input,"protected",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"protected is a keyword");
  currKWsize = strlen("public");
  memcpy(input,"public",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"public is a keyword");
  currKWsize = strlen("return");
  memcpy(input,"return",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"return is a keyword");
  currKWsize = strlen("short");
  memcpy(input,"short",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"short is a keyword");
  currKWsize = strlen("static");
  memcpy(input,"static",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"static is a keyword");
  currKWsize = strlen("strictfp");
  memcpy(input,"strictfp",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"strictfp is a keyword");
  currKWsize = strlen("super");
  memcpy(input,"super",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"super is a keyword");
  currKWsize = strlen("switch");
  memcpy(input,"switch",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"switch is a keyword");
  currKWsize = strlen("synchronized");
  memcpy(input,"synchronized",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"synchronized is a keyword");
  currKWsize = strlen("this");
  memcpy(input,"this",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"this is a keyword");
  currKWsize = strlen("throw");
  memcpy(input,"throw",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"throw is a keyword");
  currKWsize = strlen("throws");
  memcpy(input,"throws",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"throws is a keyword");
  currKWsize = strlen("transient");
  memcpy(input,"transient",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"transient is a keyword");
  currKWsize = strlen("try");
  memcpy(input,"try",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"try is a keyword");
  currKWsize = strlen("void");
  memcpy(input,"void",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"void is a keyword");
  currKWsize = strlen("volatile");
  memcpy(input,"volatile",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"volatile is a keyword");
  currKWsize = strlen("while");
  memcpy(input,"while",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  fail_unless((retval == 1),"while is a keyword");
  printf("testing assert\n");
  currKWsize = strlen("assert");
  memcpy(input,"assert",currKWsize);
  retval = quarry_util_isKeyword(kwTable, input, currKWsize );
  printf("%d\n",retval);
  fail_unless((retval == 1),"assert is a keyword");
}


int main(int argc, char **argv)
{
  test_KeywordsPositive();
}

