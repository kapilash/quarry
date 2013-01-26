/*
Copyright (c) 2013, Hemanth Kapila
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1) Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2) Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
The name of the Hemanth Kapila may NOT be used to endorse or promote products derived from this software without specific prior written permission.
3) THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
#include "quarry.h"
#include <stdio.h>
#include <stdlib.h>

void printSlab(quarry_SlabPtr slab){
  int index = 0;
  if(slab == NULL){
    printf("NULL\n");
  }
  printf("slab{");
  printf("line = %d;",slab->line);
  printf("col = %d;",slab->col);
  printf("length = %d;",slab->slabLength);
  printf("slabMD = %x;",slab->slabMD);
  printf("content = {");
  for(index = 0;index < slab->slabLength;index++){
    if(slab->data[index] != '\n')
      printf("%c",slab->data[index]);
    else
      printf("\\n");
  }
  printf("}");
  printf("slabType = ");
  switch(slab->slabType){
  case   quarry_Error:      { printf("ERROR");break;}
  case   quarry_Keyword:    { printf("KEYWORD");break;}
  case   quarry_Identifier: {printf("IDENTIFIER");break;}
  case   quarry_String:     {printf("STRING");break;}
  case   quarry_Char:       {printf("CHAR");break;}
  case   quarry_Numbers:    {printf("NUMBERS");break;}
  case   quarry_Operator:   {printf("OPERATOR");break;}
  case   quarry_Grouping:   {printf("GROUPING");break;}
  case   quarry_Punctuation:{printf("PUNCTUATION");break;}
  case   quarry_Comment:    {printf("COMMENT");break;}
  case   quarry_Whitespace: {printf("WHITESPACE");break;}
  case   quarry_NewLine:    {printf("NEWLINE");break;}
  case quarry_EOF:          {printf("EOF");break;}
  case quarry_MetaId:       {printf("META");break;}
  default: printf("UNKNOWN");
  }
  printf(";}\n");
}

int main(int argc, char **argv){
  quarry_Reader reader;
  quarry_SlabPtr slab;
  long l = 0;
  int lines = 0;
  int comments  = 0;
  int toPrintType = 100;
  int toContinue = 1;
  if(argc <2){
    printf("need a file\n");
    exit(1);
  }
  if(argc > 2){
    toPrintType = atoi(argv[2]);
  }
  reader = quarry_newReader(argv[1],quarry_Java);
  while(toContinue){
    slab = quarry_read(reader);
    l++;
    if(slab->slabType == toPrintType)
      printSlab(slab);
    if(slab->slabType == quarry_Comment)
      comments++;
    if(slab->slabType == quarry_EOF)
      toContinue = 0;
    lines = slab->line;
    quarry_freeSlab(slab);
  }
  quarry_closeReader(reader);
  printf("slabs = %ld\n",l);
  printf("lines = %d\n",lines);
  printf("comments = %d\n",comments);
  
}
