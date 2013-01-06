#include "quarry_internal.h"
#include <stdlib.h>
#include <stdio.h>

quarry_QuarryPtr quarry_makeQuarry(unsigned char *buffer, int bufferSize){
  quarry_QuarryPtr quarry =  (quarry_QuarryPtr)malloc(sizeof(quarry_Quarry));
  quarry->line = 1;
  quarry->col = 0;
  quarry->maxSlabLength = MAX_SLAB_LENGTH;
  quarry->input.length = bufferSize;
  quarry->input.index = 0;
  quarry->input.data = buffer;
  quarry->slabType = quarry_Error;
  quarry->holder.data = (unsigned char *)malloc(4096*sizeof(unsigned char));
  quarry->holder.length = 0;
  quarry->kwTable = NULL;
  return quarry;
}

void quarry_printQuarry(quarry_QuarryPtr quarry){
  int index = 0;
  if(quarry == NULL){
    printf("NULL\n");
  }
  printf("quarry{\n");
  printf("\tline = %d;\n",quarry->line);
  printf("\tcol = %d;\n",quarry->col);
  printf("\tinput.length = %d\n",quarry->input.length);
  printf("\tinput.index = %d\n",quarry->input.index);
  printf("\tinput.consumed = ");
  for(index = 0;index < quarry->input.index;index++){
    printf("%c",quarry->input.data[index]);
  }
  printf(";\n");
  printf("\tinput.unconsumed = ");
  for(index = quarry->input.index;index < quarry->input.length;index++){
    printf("%c",quarry->input.data[index]);
  }
  printf(";\n");
  printf("\tholder.length=%d\n",quarry->holder.length);
  printf("\n\tslab data = ");
  for(index = 0;index < quarry->holder.length;index++){
    printf("%c",quarry->holder.data[index]);
  }
  printf(";\n\tholder.slabType = ");
  switch(quarry->slabType){
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
  printf(";\n}\n");
}

int quarry_util_isKeyword(qu_KWTablePtr kwtable, unsigned char *identifier, int len){
  qu_KWspl keywordsPerLetter;
  int index,curr,kwlen,max,j,k,isMatch;
  if(kwtable == NULL)
    return 0;
  if(len < 1)
    return 0;

  keywordsPerLetter = kwtable->kwIndices[identifier[0]];
  curr = 0;max = 0;
  for(index =0;index<(keywordsPerLetter.wordCount);index++){
    kwlen = keywordsPerLetter.indices[index];
    max += kwlen;
    if(len == kwlen){

      isMatch = 1;

      k = 0;
      for(j=curr;j<max;j++){
	if(identifier[k] != keywordsPerLetter.word[j]){
	  isMatch = 0; break;
	}
	k++;
      }
      if(isMatch){
	return 1;
      }
    }
    curr = max;
  }
  return 0;
}

void quarry_util_freeKeywordTable(qu_KWTablePtr kwtable){
  if(kwtable == NULL)
    return;
  free(kwtable->word);
  free(kwtable->indices);
  free(kwtable->kwIndices);
  free(kwtable);
}


void quarry_printSlab(quarry_SlabPtr slab){
  int index = 0;
  if(slab == NULL){
    printf("NULL\n");
  }
  printf("slab{\n");
  printf("\tline = %d;\n",slab->line);
  printf("\tcol = %d;\n",slab->col);
  printf("\tlength = %d\n",slab->slabLength);
  printf("\tcontent = {\n");
  for(index = 0;index < slab->slabLength;index++){
    printf("%c",slab->data[index]);
  }
  printf("\n}\n");
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
  printf(";\n}\n");
}

