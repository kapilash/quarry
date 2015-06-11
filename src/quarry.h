/*
Copyright (c) 2013, Hemanth Kapila
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1) Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2) Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
The name of the Hemanth Kapila may NOT be used to endorse or promote products derived from this software without specific prior written permission.
3) THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
#ifndef __QUARRY__
#define __QUARRY__
#include <sys/types.h>
#include <stdint.h>
#include "quarry_export.h"

typedef uint32_t u_int32_t;

enum quarry_SlabType{
  quarry_Error,
  quarry_Keyword,
  quarry_Identifier,
  quarry_String,
  quarry_Char,
  quarry_Numbers,
  quarry_Operator,
  quarry_Grouping,
  quarry_Punctuation,
  quarry_Comment,
  quarry_Whitespace,
  quarry_NewLine,
  quarry_MetaId,
  quarry_Bool,
  quarry_EOF
};

enum quarry_Number_Flags {
    quarry_Number_HasL = 1<<0,
    quarry_Number_HasTwoLs = 1<<1,
    quarry_Number_HasE = 1<<2,
    quarry_Number_HasSign=1<<3,
    quarry_Number_HasDot = 1<<4,
    quarry_Number_IsDouble= 1<<5,
    quarry_Number_IsFloat = 1<<6,
    quarry_Number_IsOctal = 1<<7,
    quarry_Number_IsBinary = 1<<8,
    quarry_Number_IsHex = 1<<9,
    quarry_Number_IsUnsigned = 1 <<10
};


struct quarry_Slab_
{
  int line;
  int col;
  int slabLength;
  enum quarry_SlabType slabType;
  unsigned char *data;
  u_int32_t slabMD;
};


struct quarry_Token_
{
  unsigned int tokenType;
  unsigned short tokenLength;
  unsigned char *text;
};

typedef struct quarry_Slab_ quarry_Slab;
typedef struct quarry_Slab_ *quarry_SlabPtr;

typedef struct quarry_Token_ quarry_Token;
typedef struct quarry_Token_ *quarry_TokenPtr;

enum  quarry_PL_{
  quarry_Gen,
  quarry_C,
  quarry_Java
};

typedef enum quarry_PL_ quarry_PL;

typedef void *quarry_Reader;

QUARRY_EXPORT
quarry_Reader quarry_newReader(const char *fileName, quarry_PL pl);

QUARRY_EXPORT
quarry_SlabPtr quarry_read(quarry_Reader reader);

QUARRY_EXPORT
void quarry_freeSlab(quarry_SlabPtr slab);

QUARRY_EXPORT
void quarry_closeReader(quarry_Reader reader);



#endif
