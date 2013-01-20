/* generating code */
#include "quarry_internal.h"
#include <stdlib.h>
#include <stdio.h>


qu_KWTablePtr quarry_util_keywordTableJavaOpers(){
    unsigned char *concatenated = NULL;
    int *lengths = NULL;
    int index = 0;
    qu_KWTablePtr kwTablePtr = NULL;
    qu_KWsplPtr kwStrArr = NULL;

    int keywordCount = 15;
    int concatenatedLength = 65;
    int kwIndex = 0;

    kwTablePtr = (qu_KWTablePtr)malloc(sizeof(qu_KWTable));
    concatenated = (char *)malloc(65 * sizeof(unsigned char));
    lengths = (int*)malloc(37 * sizeof(int));
    kwStrArr = (qu_KWsplPtr)malloc(128 * sizeof(qu_KWspl));
    kwTablePtr->kwIndices = kwStrArr;
    kwTablePtr->word = concatenated;kwTablePtr->concatLength = concatenatedLength;
    kwTablePtr->indices = lengths;kwTablePtr->kwCount = keywordCount;

    for(index = 0; index<128;index++){
        kwStrArr[index].word = NULL;
        kwStrArr[index].indices = NULL;
        kwStrArr[index].wordCount = 0;
    }
    concatenated[0] = 33;
    concatenated[1] = 33;
    concatenated[2] = 61;
    concatenated[3] = 37;
    concatenated[4] = 37;
    concatenated[5] = 61;
    concatenated[6] = 38;
    concatenated[7] = 38;
    concatenated[8] = 38;
    concatenated[9] = 38;
    concatenated[10] = 61;
    concatenated[11] = 42;
    concatenated[12] = 42;
    concatenated[13] = 61;
    concatenated[14] = 43;
    concatenated[15] = 43;
    concatenated[16] = 43;
    concatenated[17] = 43;
    concatenated[18] = 61;
    concatenated[19] = 45;
    concatenated[20] = 45;
    concatenated[21] = 45;
    concatenated[22] = 45;
    concatenated[23] = 61;
    concatenated[24] = 47;
    concatenated[25] = 47;
    concatenated[26] = 61;
    concatenated[27] = 58;
    concatenated[28] = 60;
    concatenated[29] = 60;
    concatenated[30] = 60;
    concatenated[31] = 60;
    concatenated[32] = 60;
    concatenated[33] = 61;
    concatenated[34] = 60;
    concatenated[35] = 61;
    concatenated[36] = 61;
    concatenated[37] = 61;
    concatenated[38] = 61;
    concatenated[39] = 62;
    concatenated[40] = 62;
    concatenated[41] = 61;
    concatenated[42] = 62;
    concatenated[43] = 62;
    concatenated[44] = 61;
    concatenated[45] = 62;
    concatenated[46] = 62;
    concatenated[47] = 61;
    concatenated[48] = 62;
    concatenated[49] = 62;
    concatenated[50] = 62;
    concatenated[51] = 62;
    concatenated[52] = 62;
    concatenated[53] = 62;
    concatenated[54] = 61;
    concatenated[55] = 63;
    concatenated[56] = 94;
    concatenated[57] = 94;
    concatenated[58] = 61;
    concatenated[59] = 124;
    concatenated[60] = 124;
    concatenated[61] = 61;
    concatenated[62] = 124;
    concatenated[63] = 124;
    concatenated[64] = 126;
    lengths[0] = 1;
    lengths[1] = 2;
    lengths[2] = 1;
    lengths[3] = 2;
    lengths[4] = 1;
    lengths[5] = 2;
    lengths[6] = 2;
    lengths[7] = 1;
    lengths[8] = 2;
    lengths[9] = 1;
    lengths[10] = 2;
    lengths[11] = 2;
    lengths[12] = 1;
    lengths[13] = 2;
    lengths[14] = 2;
    lengths[15] = 1;
    lengths[16] = 2;
    lengths[17] = 1;
    lengths[18] = 1;
    lengths[19] = 2;
    lengths[20] = 3;
    lengths[21] = 2;
    lengths[22] = 1;
    lengths[23] = 2;
    lengths[24] = 1;
    lengths[25] = 2;
    lengths[26] = 3;
    lengths[27] = 3;
    lengths[28] = 3;
    lengths[29] = 4;
    lengths[30] = 1;
    lengths[31] = 1;
    lengths[32] = 2;
    lengths[33] = 1;
    lengths[34] = 2;
    lengths[35] = 2;
    lengths[36] = 1;
    kwStrArr[126].word = &(concatenated[64]);
    kwStrArr[126].indices  = &(lengths[36]);
    kwStrArr[126].wordCount =  1;


    kwStrArr[124].word = &(concatenated[59]);
    kwStrArr[124].indices  = &(lengths[33]);
    kwStrArr[124].wordCount =  3;


    kwStrArr[94].word = &(concatenated[56]);
    kwStrArr[94].indices  = &(lengths[31]);
    kwStrArr[94].wordCount =  2;


    kwStrArr[63].word = &(concatenated[55]);
    kwStrArr[63].indices  = &(lengths[30]);
    kwStrArr[63].wordCount =  1;


    kwStrArr[62].word = &(concatenated[39]);
    kwStrArr[62].indices  = &(lengths[24]);
    kwStrArr[62].wordCount =  6;


    kwStrArr[61].word = &(concatenated[36]);
    kwStrArr[61].indices  = &(lengths[22]);
    kwStrArr[61].wordCount =  2;


    kwStrArr[60].word = &(concatenated[28]);
    kwStrArr[60].indices  = &(lengths[18]);
    kwStrArr[60].wordCount =  4;


    kwStrArr[58].word = &(concatenated[27]);
    kwStrArr[58].indices  = &(lengths[17]);
    kwStrArr[58].wordCount =  1;


    kwStrArr[47].word = &(concatenated[24]);
    kwStrArr[47].indices  = &(lengths[15]);
    kwStrArr[47].wordCount =  2;


    kwStrArr[45].word = &(concatenated[19]);
    kwStrArr[45].indices  = &(lengths[12]);
    kwStrArr[45].wordCount =  3;


    kwStrArr[43].word = &(concatenated[14]);
    kwStrArr[43].indices  = &(lengths[9]);
    kwStrArr[43].wordCount =  3;


    kwStrArr[42].word = &(concatenated[11]);
    kwStrArr[42].indices  = &(lengths[7]);
    kwStrArr[42].wordCount =  2;


    kwStrArr[38].word = &(concatenated[6]);
    kwStrArr[38].indices  = &(lengths[4]);
    kwStrArr[38].wordCount =  3;


    kwStrArr[37].word = &(concatenated[3]);
    kwStrArr[37].indices  = &(lengths[2]);
    kwStrArr[37].wordCount =  2;


    kwStrArr[33].word = &(concatenated[0]);
    kwStrArr[33].indices  = &(lengths[0]);
    kwStrArr[33].wordCount =  2;


    for(index = 0; index<128;index++){
        kwStrArr[index].beginKWId = kwIndex;
        kwIndex = kwIndex + kwStrArr[index].wordCount;
    }
    return kwTablePtr;
}
