#include "lexers.h"
#include <stdio.h>
#include <stdlib.h>

#define IS_IDENTIFIER_CHAR(x) (((x>='A')&&(x<='Z')) || ((x>='a')&&(x<='z')) || ((x>='0')&&(x<='9')) || (x=='_')|| (x=='$') || (x>127))


int quarry_punctuation(quarry_QuarryPtr quarry, int lexerState){
  unsigned char nextChar;
  quarry->slabType = quarry_Punctuation;
  Quarry_ReadNext(quarry,nextChar)
  Quarry_AppendSingleChar(quarry,nextChar)
  return 0;
}

int quarry_operatorLexer(quarry_QuarryPtr quarry, int lexerState){
  unsigned char nextChar;
  quarry->slabType = quarry_Operator;
  Quarry_ReadNext(quarry,nextChar)
  Quarry_AppendSingleChar(quarry,nextChar)
  return 0;
}

int quarry_groupLexer(quarry_QuarryPtr quarry, int lexerState){
  unsigned char nextChar;
  quarry->slabType = quarry_Grouping;
  Quarry_ReadNext(quarry,nextChar)
  Quarry_AppendSingleChar(quarry,nextChar)
  return 0;
}

int quarry_wsLexer(quarry_QuarryPtr quarry, int lexerState){
  unsigned char nextChar;
  quarry->slabType = quarry_Whitespace;
  Quarry_ReadNext(quarry,nextChar)
  Quarry_AppendSingleChar(quarry,nextChar)
  return 0;
}

int quarry_lfLexer(quarry_QuarryPtr quarry, int lexerState){
  unsigned char nextChar;
  quarry->slabType = quarry_NewLine;
  Quarry_ReadNext(quarry,nextChar);
  Quarry_AppendSingleChar(quarry,nextChar);
  Quarry_IncrLine(quarry); 
  return 0;
}
int quarry_crlfLexer(quarry_QuarryPtr quarry, int lexerState){
  unsigned char nextChar;

  if(lexerState == 0){
    Quarry_ReadNext(quarry,nextChar);
    Quarry_AppendSingleChar(quarry,nextChar);
  }
  if(Quarry_HasMore(quarry)){
    nextChar = Quarry_PeekNextInputChar(quarry);
    if(nextChar == '\n'){
      Quarry_ReadNext(quarry,nextChar);
      Quarry_AppendNext(quarry,nextChar);
      quarry->slabType = quarry_NewLine;
      Quarry_IncrLine(quarry);
      return 0;
    }
    else{
      quarry->slabType = quarry_Whitespace;
      return 0;
    }
  }
  return 1;
}
int quarry_errLexer(quarry_QuarryPtr quarry, int lexerState){
  unsigned char nextChar;
  quarry->slabType = quarry_Error;
  if(Quarry_HasMore(quarry)){
    Quarry_ReadNext(quarry,nextChar);
    Quarry_AppendSingleChar(quarry,nextChar);
  }
  return 0;
}

int quarry_idLexer(quarry_QuarryPtr quarry, int lexerState){
  unsigned char nextChar;
  Quarry_ReadNext(quarry,nextChar);
  if(lexerState != 0){
    if(!IS_IDENTIFIER_CHAR(nextChar))
      goto RETURN_ID_SLABTYPE;
  }
  Quarry_AppendNext(quarry,nextChar);
  while (Quarry_HasMore(quarry)){
    Quarry_ReadNext(quarry,nextChar);
    if(!IS_IDENTIFIER_CHAR(nextChar))
      goto RETURN_ID_SLABTYPE;  
    Quarry_AppendNext(quarry,nextChar);
  }
  quarry->slabType = quarry_Error;
  return 1;

 RETURN_ID_SLABTYPE:
  Quarry_UnRead(quarry);
  if(quarry_util_isKeyword(quarry->kwTable,quarry->holder.data,quarry->holder.length)){
    quarry->slabType = quarry_Keyword;
  }else{
    quarry->slabType = quarry_Identifier;
  }
  return 0;
}

void quarry_StrAppendNext(quarry_QuarryPtr quarry, unsigned char nextChar){
  if(quarry->holder.length < quarry->maxSlabLength) {
    quarry->holder.data[quarry->holder.length]=nextChar;
    quarry->holder.length++;
  }
  if(quarry->holder.latest == 0){
    quarry->holder.latest = (nextChar == '\\');
  }else{
    quarry->holder.latest = 0;//reset it back to 0
  }
}


int genQLexer(const char qc,enum quarry_SlabType slabType,quarry_QuarryPtr quarry,int lexerState){
  unsigned char nextChar;
  Quarry_ReadNext(quarry,nextChar);
  if(lexerState != 0){
    if((nextChar == qc) && (!quarry->holder.latest)){
      goto RETURN_STR_SLABTYPE;
    }
    quarry_StrAppendNext(quarry,nextChar);
  }else{
    quarry->holder.latest = 0;
  }

  while (Quarry_HasMore(quarry)){
    Quarry_ReadNext(quarry,nextChar);
    if((nextChar == qc) && (!quarry->holder.latest)){
      goto RETURN_STR_SLABTYPE;
    }
    if(nextChar == '\n'){
      Quarry_IncrLine(quarry);
    }
    quarry_StrAppendNext(quarry,nextChar);
  }
  quarry->slabType = quarry_Error;
  return 1;
 RETURN_STR_SLABTYPE:
  quarry->slabType = slabType;
  return 0;
}

int quarry_dqLexer(quarry_QuarryPtr quarry, int lexerState){
  return  genQLexer('"',quarry_String,quarry,lexerState);
}

int quarry_sqLexer(quarry_QuarryPtr quarry, int lexerState){
  return genQLexer('\'',quarry_Char,quarry,lexerState);
}

int digitLexer(int (*f)(unsigned char c), quarry_QuarryPtr quarry, int lexerState){
  unsigned char nextChar;

  if(lexerState == 0){
    Quarry_ReadNext(quarry,nextChar);
    Quarry_AppendNext(quarry,nextChar);
  }
  while (Quarry_HasMore(quarry)){
    nextChar = Quarry_PeekNextInputChar(quarry);
    if((*f)(nextChar)){
      Quarry_ReadNext(quarry,nextChar);
      Quarry_AppendNext(quarry,nextChar);
    }
    else{
      quarry->slabType = quarry_Numbers;
       return 0;
    }
  }
  quarry->slabType = quarry_Error;
  return 1;
}

int quarry_isDecimal(unsigned char c){
  return ((c < 58) && (c > 47)) || (c == 95);
}

int quarry_isBinary(unsigned char c){
  return ((c == 48 ) || (c == 49) || (c == 95));
}

int quarry_isOctal(unsigned char c){
  return ((c < 56) && (c > 47)) || (c == 95);
}

int quarry_isHexadecimal(unsigned char c){
  return ((c < 58) && (c > 47)) || (c == 95) || ((c < 71) && (c > 64)) || ((c < 103) && (c >96));
}

int numberLexer(int (*f)(unsigned char),quarry_QuarryPtr quarry, int lexerState){
  unsigned char nextChar;
  int ret = 0;
  ret = digitLexer(f,quarry,lexerState);
  if(ret == 1)
    return ret;

  nextChar = Quarry_PeekNextInputChar(quarry);
  if((nextChar == 'l') || (nextChar == 'L')){
    Quarry_ReadNext(quarry,nextChar);
    Quarry_AppendNext(quarry,nextChar);
  }
  return 0;
}

int quarry_decimalLexer(quarry_QuarryPtr quarry, int lexerState){
  return numberLexer(&quarry_isDecimal,quarry,lexerState);
}

int quarry_hexBinOct0Lexer(quarry_QuarryPtr quarry, int lexerState){
  unsigned char nextChar;
  int ret;

  if(lexerState < 10){
    if(lexerState == 0){
      Quarry_ReadNext(quarry,nextChar);
      Quarry_AppendNext(quarry,nextChar);
    }
    if(!Quarry_HasMore(quarry)){
      quarry->slabType = quarry_Error;
      return 1;
    }
    nextChar = Quarry_PeekNextInputChar(quarry);
    switch(nextChar){
    case 'b':
    case 'B':{
      Quarry_ReadNext(quarry,nextChar);
      Quarry_AppendNext(quarry,nextChar);
      ret = numberLexer(&quarry_isBinary,quarry,0);
      return (ret == 0) ? 0 : (10 + ret);	
    }
    case 'x':
    case 'X':{
      Quarry_ReadNext(quarry,nextChar);
      Quarry_AppendNext(quarry,nextChar);
      ret = numberLexer(&quarry_isHexadecimal,quarry,0);
      return (ret == 0) ? 0 : (20 + ret);
    }
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':{
      Quarry_ReadNext(quarry,nextChar);
      Quarry_AppendNext(quarry,nextChar);
      ret = numberLexer(&quarry_isOctal,quarry,0);
      return (ret == 0) ? 0 : (30 + ret);
    }
    default:{
      quarry->slabType = quarry_Numbers;
      return 0;
    }
    }
  }else if(lexerState < 20){
    ret = numberLexer(&quarry_isBinary,quarry,lexerState - 10);
    return (ret == 0) ? 0 : (10 + ret);
  }else if(lexerState < 30){
    ret = numberLexer(&quarry_isHexadecimal,quarry,lexerState - 20);
    return (ret == 0) ? 0 : (20 + ret);
  }else{
    ret = numberLexer(&quarry_isOctal,quarry,lexerState - 30);
    return (ret == 0) ? 0 : (30 + ret);
  }

  return 0;
}
