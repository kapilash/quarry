/*
Copyright (c) 2013, Hemanth Kapila
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1) Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2) Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
The name of the Hemanth Kapila may NOT be used to endorse or promote products derived from this software without specific prior written permission.
3) THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
#include "lexers.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>

/*
 *Using FNV1-a hash from http://www.isthe.com/chongo/tech/comp/fnv
 *Copied the code from the same as is, as it was described to be a public domain work
 */

/* If we are interested in 64 bit hash, use the following.
#define FNV1_64_INIT ((u_int64_t)0xcbf29ce484222325ULL)
#define FNV1A_64_INIT FNV1_64_INIT
#define FNV_64_PRIME ((u_int64_t)0x100000001b3ULL)
*/
#define FNV_32_PRIME ((u_int32_t)0x01000193)
#define FNV1_32_INIT ((u_int32_t)0x811c9dc5)
#define FNV1A_32_INIT FNV1_32_INIT

#define IS_IDENTIFIER_CHAR(x) (((x>='A')&&(x<='Z')) || ((x>='a')&&(x<='z')) || ((x>='0')&&(x<='9')) || (x=='_')|| (x=='$') || (x>127))


static int isOperator(unsigned char c, unsigned char *opers, int length){
  int index = 0;int middle;
  int lower = 0; int upper = length-1;
  if(c < opers[lower])
    return 0;
  if(c > opers[upper])
    return 0;
  while(lower < upper){
    middle = (lower + upper)/2 ;
    if(c < opers[middle]){
      upper = middle-1;
    }else if(c == opers[middle])
      return 1;
    else
      lower = middle+1;
  }
  return 0;
}


int quarry_punctuation(quarry_QuarryPtr quarry, int lexerState){
  unsigned char nextChar;
  quarry->slabType = quarry_Punctuation;
  Quarry_ReadNext(quarry,nextChar)
  Quarry_AppendSingleChar(quarry,nextChar)
  return 0;
}

int quarry_operatorLexer(quarry_QuarryPtr quarry, int lexerState){
  unsigned char nextChar;
  unsigned char operChars[] = {33,37,38,42,43,45,47,58,60,61,62,63,94,124,126};

  Quarry_ReadNext(quarry,nextChar);
  if(lexerState != 0){
    if(!isOperator(nextChar,operChars,15))
      goto RETURN;
  }

  Quarry_AppendNext(quarry,nextChar);
  while (Quarry_HasMore(quarry)){
    Quarry_ReadNext(quarry,nextChar);
    if(!isOperator(nextChar,operChars,15))
      goto RETURN;  
    Quarry_AppendNext(quarry,nextChar);
  }
  quarry->slabType = quarry_Error;
  quarry->holder.md = -1;
  return 1;

 RETURN:
  Quarry_UnRead(quarry);
  quarry->slabType = quarry_Operator;
  quarry->holder.md =  quarry_util_keywordIndex(quarry->operTable,quarry->holder.data,quarry->holder.length);

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
  unsigned char nextChar;int kwIndex = -1;
  u_int32_t hval = 0;

  Quarry_ReadNext(quarry,nextChar);
  if(lexerState != 0){
    hval = quarry->holder.md;
    if(!IS_IDENTIFIER_CHAR(nextChar))
      goto RETURN_ID_SLABTYPE;
  }else{
    quarry->holder.md = FNV1A_32_INIT ; 
  }
  hval ^= (u_int32_t)nextChar;
  hval *= FNV_32_PRIME;
  Quarry_AppendNext(quarry,nextChar);
  while (Quarry_HasMore(quarry)){
    Quarry_ReadNext(quarry,nextChar);
    if(!IS_IDENTIFIER_CHAR(nextChar))
      goto RETURN_ID_SLABTYPE;  
    Quarry_AppendNext(quarry,nextChar);
    hval ^= (u_int32_t)nextChar;
    hval *= FNV_32_PRIME;
  }
  quarry->slabType = quarry_Error;
  quarry->holder.md = hval;
  return 1;

 RETURN_ID_SLABTYPE:
  Quarry_UnRead(quarry);
  kwIndex = quarry_util_keywordIndex(quarry->kwTable,quarry->holder.data,quarry->holder.length);
  if(kwIndex>=0 ){
    quarry->slabType = quarry_Keyword;
    quarry->holder.md = kwIndex;
  }else{
    quarry->slabType = quarry_Identifier;
    quarry->holder.md = hval;
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

static int digitLexer(int (*f)(unsigned char c), quarry_QuarryPtr quarry, int lexerState){
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

static int numberLexer(int (*f)(unsigned char),quarry_QuarryPtr quarry, int lexerState){
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

int quarry_metaIdLexer(quarry_QuarryPtr quarry, int lexerState){
  unsigned char nextChar;int ret = 0;

  if(lexerState < 10){
    if(lexerState == 0){
      Quarry_ReadNext(quarry,nextChar);
      Quarry_AppendNext(quarry,nextChar);
    }
    if(!Quarry_HasMore(quarry)){
      return 1;
    }
    nextChar = Quarry_PeekNextInputChar(quarry);
    if(((nextChar >= 'A') && (nextChar <= 'Z')) || ((nextChar >='a') && (nextChar <= 'z')) || (nextChar == '_')){
    }else{
      quarry->slabType = quarry_MetaId;
      quarry->holder.md = -1;
      return 0;
    }
  }
  ret = quarry_idLexer(quarry,lexerState > 0 ? lexerState - 10: 0);
  quarry->slabType = quarry_MetaId;
  return (ret == 0)? 0 : (10 + ret);
}

int quarry_decimalLexer(quarry_QuarryPtr quarry, int lexerState){
  int i =numberLexer(&quarry_isDecimal,quarry,lexerState); 
  quarry->holder.md = 0;
  return i;
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
      quarry->holder.md = 1;
      return (ret == 0) ? 0 : (10 + ret);	
    }
    case 'x':
    case 'X':{
      Quarry_ReadNext(quarry,nextChar);
      Quarry_AppendNext(quarry,nextChar);
      quarry->holder.md = 3;
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
      quarry->holder.md = 2;
      return (ret == 0) ? 0 : (30 + ret);
    }
    default:{
      quarry->slabType = quarry_Numbers;
      quarry->holder.md = 0;
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
