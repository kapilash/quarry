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
