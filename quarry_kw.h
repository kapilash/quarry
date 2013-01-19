/*
Copyright (c) 2013, Hemanth Kapila
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1) Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2) Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
The name of the Hemanth Kapila may NOT be used to endorse or promote products derived from this software without specific prior written permission.
3) THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
#ifndef __QUARRY_STRTAB__
#define __QUARRY_STRTAB__


struct quarry_util_KWsPerLetter_{
  const char *word;
  int wordCount;
  int *indices;
  int beginKWId;
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

int quarry_util_keywordIndex(qu_KWTablePtr kwtable, unsigned char *identifier, int len);
#endif
