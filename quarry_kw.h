#ifndef __QUARRY_STRTAB__
#define __QUARRY_STRTAB__


struct quarry_util_KWsPerLetter_{
  const char *word;
  int wordCount;
  int *indices;
};



typedef struct quarry_util_KWsPerLetter_ qu_KWspl;
typedef struct quarry_util_KWsPerLetter_ *qu_KWsplPtr;

struct quarry_util_KWTable_{
  unsigned char *word;
  int *indices;
  int concatLength;
  int kwCount;
  qu_KWsplPtr kwIndices;
};

typedef struct quarry_util_KWTable_ qu_KWTable;
typedef struct quarry_util_KWTable_ *qu_KWTablePtr;

qu_KWTablePtr quarry_util_keywordTableJava();

qu_KWTablePtr quarry_util_keywordTableC();

void quarry_util_freeKeywordTable(qu_KWTablePtr kwtable);

int quarry_util_isKeyword(qu_KWTablePtr kwtable, unsigned char *identifier, int len);
#endif
