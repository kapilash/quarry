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
  if(argc <2){
    printf("need a file\n");
    exit(1);
  }
  reader = quarry_newReader(argv[1],quarry_Java);
  while(1){
    slab = quarry_read(reader);
    l++;
    //printSlab(slab);
    if(slab->slabType == quarry_Comment)
      comments++;
    if(slab->slabType == quarry_EOF)
      break;
    lines = slab->line;
    quarry_freeSlab(slab);
  }
  quarry_closeReader(reader);
  printf("slabs = %ld\n",l);
  printf("lines = %d\n",lines);
  printf("comments = %d\n",comments);
  
}
