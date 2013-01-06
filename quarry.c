#include "quarry_internal.h"
#include <stdlib.h>
#include <stdio.h>
#include "lexers.h"

const size_t QRY_BUFFER_SIZE = 4096;

static int fillQuarryInput(QReaderPtr reader,quarry_SlabPtr slab){
  if(feof(reader->file)){
    slab->slabType = quarry_EOF;
    return 0;
  }
  reader->quarry->input.length = fread(reader->quarry->input.data,sizeof(unsigned char),QRY_BUFFER_SIZE,reader->file);
  reader->quarry->input.index = 0;
  if(!ferror(reader->file)){
    return 1;
  }
  fputs("error reading file\n",stderr);
  reader->quarry->input.length = 0;
  slab->slabType = quarry_Error;
  return 0;
}

quarry_Reader quarry_newReader(const char *fileName, quarry_PL pl){
  QReaderPtr reader;
  quarry_Lexer *lexers;
  int index;
  unsigned char *buffer;

  reader = (QReaderPtr)malloc(sizeof(QReader));
  reader->file = fopen(fileName,"r");
  lexers = (quarry_Lexer*)malloc(256*sizeof(quarry_Lexer));

   for(index = 0;index<256;index++)
     lexers[index] = NULL;
  
  lexers[0] = &quarry_errLexer;
  /* lexercount = 1; */

  /* whitespace: (32) */
  for(index = 1;index<33;index++){
    lexers[index] = &quarry_wsLexer;
  }
  lexers[127] = &quarry_wsLexer; /* (1) */
  /* lexerCount:  1 + 32 + 1 = 34*/

  /* newLines */
  lexers[10] = &quarry_lfLexer;
  lexers[13] = &quarry_crlfLexer;
  /* lexerCount:  34 +2 = 36*/
  
  /* comments (1)*/
  if( (pl == quarry_C) || (pl == quarry_Java)){
    lexers['/'] = &quarry_commentLexer;
  }else{
    lexers['/'] = &quarry_operatorLexer;
  }
  /* lexerCount:   36 +1 =37*/

  /* constant values */
  lexers['"'] = &quarry_dqLexer;/* string (1)*/
  lexers['\''] = &quarry_sqLexer;/* char (1)*/
  lexers['0'] = &quarry_hexBinOct0Lexer;/* hexadecimal, octal, binary or single 0 (1)*/
  for(index = 49;index<58;index++){ /* decimal numbers (9) */
    lexers[index] = &quarry_decimalLexer;
  }
  /* lexerCount:  37 +1 + 1 + 1 + 9 = 49*/

  /* grouping (8)*/
  lexers['('] = &quarry_groupLexer;
  lexers[')'] = &quarry_groupLexer;
  lexers['{'] = &quarry_groupLexer;
  lexers['}'] = &quarry_groupLexer;
  lexers['['] = &quarry_groupLexer;
  lexers[']'] = &quarry_groupLexer;
  lexers['<'] = &quarry_groupLexer;
  lexers['>'] = &quarry_groupLexer;
  /* lexerCount:  49 + 8 = 57 */

  /* identifier (26)*/
  for(index = 65;index<91;index++){
    lexers[index]=&quarry_idLexer;
  }
  lexers[95] = &quarry_idLexer; /* (1) */
  for(index = 97;index<123;index++){ /* (26) */
    lexers[index] = &quarry_idLexer;
  }
  for(index=128;index<256;index++){ /* (128) */
    lexers[index]=&quarry_idLexer;
  }
  /* lexerCount:  57 + 26 + 1 + 26 + 128 = 238 */
  /* punctuation (7)*/
  lexers[';'] = &quarry_punctuation;
  lexers[':'] = &quarry_punctuation;
  lexers[','] = &quarry_punctuation;
  lexers['.'] = &quarry_punctuation;
  lexers['?'] = &quarry_punctuation;
  lexers['!'] = &quarry_punctuation;
  lexers['`'] = &quarry_punctuation;
  /* lexerCount:  238 + 7 = 245 */
  
  /* operators */
  lexers['#'] = &quarry_operatorLexer;
  lexers['$'] = &quarry_operatorLexer;
  lexers['%'] = &quarry_operatorLexer;
  lexers['&'] = &quarry_operatorLexer;
  lexers['*'] = &quarry_operatorLexer;
  lexers['+'] = &quarry_operatorLexer;
  lexers['-'] = &quarry_operatorLexer;
  lexers['\\'] = &quarry_operatorLexer;
  lexers['^'] = &quarry_operatorLexer;
  lexers['|'] = &quarry_operatorLexer;
  lexers['@'] = &quarry_operatorLexer;
  lexers['='] = &quarry_operatorLexer;
  lexers['~'] = &quarry_operatorLexer;
  /* lexerCount: 246 + 11 = 256 */

  reader->lexers = lexers;

  buffer = (unsigned char *)malloc(sizeof(unsigned char) * QRY_BUFFER_SIZE);
  reader->quarry = quarry_makeQuarry(buffer,QRY_BUFFER_SIZE);
  reader->quarry->input.index = 0;
  reader->quarry->input.length = 0;

  switch(pl){
  case quarry_C :{
    reader->quarry->kwTable= quarry_util_keywordTableC();
    break;
  }
  case quarry_Java:{
    reader->quarry->kwTable = quarry_util_keywordTableJava();
    break;
  }
  default:
    reader->quarry->kwTable = NULL;
  }
  return reader;
}


quarry_SlabPtr quarry_read(quarry_Reader reader){
  unsigned char nextChar;
  QReaderPtr qreader;
  quarry_SlabPtr slabPtr;
  enum quarry_SlabType slabType;
  int fileIn = 0;
  quarry_Lexer lexer;
  int lexerOut = 0;
  
  slabPtr = (quarry_SlabPtr)malloc(sizeof(quarry_Slab));
  qreader = (QReaderPtr)reader;
  qreader->quarry->holder.length = 0;

  if(!Quarry_HasMore(qreader->quarry)){
    if(!fillQuarryInput(qreader,slabPtr)){
      goto RETSLAB;
    }
  }
  lexer = NULL;
  while(1){
    if(lexer == NULL){
      nextChar = Quarry_PeekNextInputChar(qreader->quarry);
      lexer = qreader->lexers[nextChar];
    }
    lexerOut = (*lexer)(qreader->quarry, lexerOut);

    if(lexerOut == 0){
      slabPtr->slabType = qreader->quarry->slabType;
      goto RETSLAB;
    }

    if(Quarry_HasMore(qreader->quarry)){
      slabPtr->slabType = quarry_Error;
      goto RETSLAB;
    }

    if(!fillQuarryInput(qreader,slabPtr)){
	goto RETSLAB;
    } 
  }  

 RETSLAB:
  slabPtr->data = qreader->quarry->holder.data;
  slabPtr->slabLength  = qreader->quarry->holder.length;
  slabPtr->line = qreader->quarry->line;
  slabPtr->col = qreader->quarry->col;
  return slabPtr;
}

void quarry_freeSlab(quarry_SlabPtr slab){
  free(slab);
}

void quarry_closeReader(quarry_Reader reader){
  QReaderPtr qreader = (QReaderPtr)reader;
  if(qreader->file != NULL)
    fclose(qreader->file);
  free(qreader->lexers);
  free(qreader->quarry->input.data);
  free(qreader->quarry->holder.data);
  quarry_util_freeKeywordTable(qreader->quarry->kwTable);
  free(qreader);
}
