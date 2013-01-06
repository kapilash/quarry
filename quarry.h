#ifndef __QUARRY__
#define __QUARRY__


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
  quarry_EOF
};


struct quarry_Slab_
{
  int line;
  int col;
  int slabLength;
  enum quarry_SlabType slabType;
  unsigned char *data;
};

typedef struct quarry_Slab_ quarry_Slab;
typedef struct quarry_Slab_ *quarry_SlabPtr;

enum quarry_PL_{
  quarry_Gen,
  quarry_C,
  quarry_Java
};

typedef enum quarry_PL_ quarry_PL;

typedef void *quarry_Reader;

quarry_Reader quarry_newReader(const char *fileName, quarry_PL pl);

quarry_SlabPtr quarry_read(quarry_Reader reader);

void quarry_freeSlab(quarry_SlabPtr slab);

void quarry_closeReader(quarry_Reader reader);



#endif
