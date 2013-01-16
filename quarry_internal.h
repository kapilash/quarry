/*
Copyright (c) 2013, Hemanth Kapila
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1) Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2) Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
The name of the Hemanth Kapila may NOT be used to endorse or promote products derived from this software without specific prior written permission.
3) THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
#ifndef __QUARRY_INTERNAL__
#define __QUARRY_INTERNAL__

#include <stdio.h>
#include "quarry_kw.h"
#include "quarry.h"

#define MAX_SLAB_LENGTH 4096;


struct quarry_Input_ {
  int length;
  unsigned char* data;
  int index;
};
typedef struct quarry_Input_ quarry_Input;
typedef struct quarry_Input_ *quarry_InputPtr;


struct quarry_SlabData_{
  size_t length;
  unsigned char latest;
  int md;
  unsigned char *data;
};

typedef struct quarry_SlabData_ quarry_SlabData;
typedef struct quarry_SlabData_ *quarry_SlabDataPtr;


struct quarry_Quarry_
{
  int line;
  int col;
  int maxSlabLength;
  quarry_Input input;
  enum quarry_SlabType slabType;
  quarry_SlabData holder;
  qu_KWTablePtr kwTable;
};

typedef struct quarry_Quarry_ quarry_Quarry;
typedef struct quarry_Quarry_ *quarry_QuarryPtr;

typedef int (*quarry_Lexer)(quarry_QuarryPtr, int);

quarry_QuarryPtr quarry_makeQuarry(unsigned char *buffer, int bufferSize);

void quarry_printQuarry(quarry_QuarryPtr q);

void quarry_printSlab(quarry_SlabPtr slab);

struct quarry_Reader_ {
  FILE *file;
  quarry_Lexer* lexers;
  quarry_QuarryPtr quarry;
};

typedef struct quarry_Reader_ *QReaderPtr;
typedef struct quarry_Reader_ QReader;

#define Quarry_HasMore(x) (((x->input.length) - (x->input.index)) > 0)

#define  Quarry_PeekNextInputChar(x)  (x->input.data[x->input.index])

#define Quarry_LastRead(x) (x->holder.latest)

#define Quarry_ReadNext(x,y) {y = x->input.data[(x->input.index)++];x->col++;}

#define  Quarry_UnRead(x)  {x->input.index = x->input.index - 1;}

#define Quarry_AppendSingleChar(x,y) {x->holder.data[0]=y;x->holder.length = 1;x->holder.latest = y;}

#define Quarry_AppendNext(x,y)  {if(x->holder.length < x->maxSlabLength) {x->holder.data[x->holder.length]=y;x->holder.length++;}x->holder.latest = y;}

#define Quarry_IncrLine(x) {x->line = x-> line + 1; x->col = 1;}
#endif
