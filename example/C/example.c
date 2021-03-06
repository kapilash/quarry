/*
Copyright (c) 2015, Hemanth Kapila
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1) Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2) Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
The name of the Hemanth Kapila may NOT be used to endorse or promote products derived from this software without specific prior written permission.
3) THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <stdio.h>
#include <stdlib.h>
#define QUARRY_EXPORT
#include "Quarry.h"

    enum TokenType {
        ERROR,
        KEYWORD,
        IDENT,
        STRING,
        CHAR,
        NUMBER,
        OPERATOR,
        OPEN_BRACE,
        CLOSE_BRACE,
        OPEN_BRACKET,
        CLOSE_BRACKET,
        SQUARE_OPEN,
        SQUARE_CLOSE,
        DOT,
        COLON,
        SEMI_COLON,
        QUESTION_MARK,
        COMMA,
        COMMENT,
        WHITESPACE,
        NEWLINE,
        META_ID,
        BOOL,
        QEOF
    };

    static char* tokenTypeStrs[] = {
    "ERROR",
    "KEYWORD",
    "IDENT",
    "STRING",
    "CHAR",
    "NUMBER",
    "OPERATOR",
    "OPEN_BRACE",
    "CLOSE_BRACE",
    "OPEN_BRACKET",
    "CLOSE_BRACKET",
    "SQUARE_OPEN",
    "SQUARE_CLOSE",
    "DOT",
    "COLON",
    "SEMI_COLON",
    "QUESTION_MARK",
    "COMMA",
    "COMMENT",
    "WHITESPACE",
    "NEWLINE",
    "META_ID",
    "BOOL",
    "QEOF"
    };

void printSlab(struct quarry_Token *slab){
  int index = 0;
  if(slab == NULL){
    printf("NULL\n");
  }
  printf("slab{");
  printf("line = %d;",slab->line);
  printf("col = %d;",slab->column);
  printf("length = %d;",slab->length);
  printf("content = {");
  for(index = 0;index < slab->length;index++){
    if(slab->textPtr[index] != '\n')
      printf("%c",slab->textPtr[index]);
    else
      printf("\\n");
  }
  printf("}");
  printf("tokenType = %s", tokenTypeStrs[slab->tokenType]);
  if (slab->tokenType == NUMBER) {
      unsigned int numberType = quarry_numberType(slab);
      printf("[");
      switch(numberType) {
      case 1 : printf("Long Double");
	  break;
      case 2: printf("Double");
	  break;
      case 3: printf("Float");
	  break;
      case 4: printf("int(%d)", quarry_toInt(slab));
	  break;
      case 5: printf("Long");
	  break;
      case 6: printf("Long Long");
	  break;
      case 7: printf("unsigned int");
	  break;
      case 8: printf("unsigned long");
	  break;
      case 9: printf("unsigned long long");
	  break;
      default:
	  printf("NOt number");
      }
      printf("]");
  }
  else if(slab->tokenType == CHAR) {
      printf("[value=%d]", quarry_toChar(slab));
  }
  printf(";}\n");
}

int main(int argc, char **argv){
  void *reader;
  struct quarry_Token *slab;
  long l = 0;
  int lines = 0;
  int comments  = 0;
  int toPrintTokens = 0;
  int toContinue = 1;
  if(argc <2){
    printf("need a file\n");
    exit(1);
  }
  if(argc > 2){
    toPrintTokens = 1;
  }


  reader = quarry_fromFile(1,argv[1]);

  while(toContinue){
    
    slab = quarry_nextToken(reader);

    l++;
    if (toPrintTokens == 1)
      printSlab(slab);
     
    if(slab->tokenType == COMMENT)
      comments++;
    if(slab->tokenType == QEOF)
      toContinue = 0;
    lines = slab->line;
    quarry_freeToken(slab);

  }
  quarry_close(reader);
  printf("tokens = %ld\n",l);
  printf("lines = %d\n",lines);
  printf("comments = %d\n",comments);
  
}
