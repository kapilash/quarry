/*
Copyright (c) 2015, Hemanth Kapila
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1) Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2) Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
The name of the Hemanth Kapila may NOT be used to endorse or promote products derived from this software without specific prior written permission.
3) THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


#include <iostream>
#include <string>
#define QUARRY_EXPORT
#include "Quarry.h"
#include <chrono>

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

static std::string  tokenTypeStrs[] = {
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
      std::cout << "NULL\n";
  }
  std::cout <<"slab{";
  std::cout <<"line = " << slab->line << ";";
  std::cout <<"col =" << slab->column << ";";
  std::cout <<"length = " << slab->length << ";";
  std::cout <<"content = {";
  for(index = 0;index < slab->length;index++){
    if(slab->textPtr[index] != '\n')
	std::cout << slab->textPtr[index];
    else
      std::cout <<"\\n";
  }
  std::cout <<"}";
  std::cout <<"tokenType = "<< tokenTypeStrs[slab->tokenType];
  std::cout <<";}" << std::endl;
}

int main(int argc, char **argv){
  void *reader;
  struct quarry_Token *slab;
  long l = 0;
  int lines = 0;
  int comments  = 0;
  bool toPrint = false;
  int toContinue = 1;
  if(argc <2){
    std::cout << "need a file" << std::endl;
    exit(1);
  }
  if(argc > 2){
    toPrint = true;
  }
  auto start = std::chrono::high_resolution_clock::now();
  reader = quarry_fromFile(1,argv[1]);
  while(toContinue){
    slab = quarry_nextToken(reader);
    l++;

    if(toPrint)
      printSlab(slab);
    
     
    if(slab->tokenType == COMMENT)
      comments++;
    if(slab->tokenType == QEOF)
      toContinue = 0;
    lines = slab->line;
    quarry_freeToken(slab);
  }
  quarry_close(reader);
  auto finish = std::chrono::high_resolution_clock::now();
  std::cout << "slabs = " << l << std::endl;
  std::cout << "lines = " << lines << std::endl;
  std::cout << "comments = " << comments << std::endl;
  std::cout << std::chrono::duration_cast<std::chrono::milliseconds>(finish-start).count() << "ms\n";
  
}
