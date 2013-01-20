/* generating code */
#include "quarry_internal.h"
#include <stdlib.h>
#include <stdio.h>


qu_KWTablePtr quarry_util_keywordTableCOperators(){
    unsigned char *concatenated = NULL;
    int *lengths = NULL;
    int index = 0;
    qu_KWTablePtr kwTablePtr = NULL;
    qu_KWsplPtr kwStrArr = NULL;

    int keywordCount = 15;
    int concatenatedLength = 58;
    int kwIndex = 0;

    kwTablePtr = (qu_KWTablePtr)malloc(sizeof(qu_KWTable));
    concatenated = (char *)malloc(58 * sizeof(unsigned char));
    lengths = (int*)malloc(35 * sizeof(int));
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
    concatenated[9] = 61;
    concatenated[10] = 42;
    concatenated[11] = 42;
    concatenated[12] = 61;
    concatenated[13] = 43;
    concatenated[14] = 43;
    concatenated[15] = 43;
    concatenated[16] = 43;
    concatenated[17] = 61;
    concatenated[18] = 45;
    concatenated[19] = 45;
    concatenated[20] = 45;
    concatenated[21] = 45;
    concatenated[22] = 61;
    concatenated[23] = 45;
    concatenated[24] = 62;
    concatenated[25] = 46;
    concatenated[26] = 47;
    concatenated[27] = 47;
    concatenated[28] = 61;
    concatenated[29] = 60;
    concatenated[30] = 60;
    concatenated[31] = 60;
    concatenated[32] = 60;
    concatenated[33] = 60;
    concatenated[34] = 61;
    concatenated[35] = 60;
    concatenated[36] = 61;
    concatenated[37] = 61;
    concatenated[38] = 61;
    concatenated[39] = 61;
    concatenated[40] = 62;
    concatenated[41] = 62;
    concatenated[42] = 61;
    concatenated[43] = 62;
    concatenated[44] = 62;
    concatenated[45] = 62;
    concatenated[46] = 62;
    concatenated[47] = 61;
    concatenated[48] = 63;
    concatenated[49] = 94;
    concatenated[50] = 94;
    concatenated[51] = 61;
    concatenated[52] = 124;
    concatenated[53] = 124;
    concatenated[54] = 61;
    concatenated[55] = 124;
    concatenated[56] = 124;
    concatenated[57] = 126;
    lengths[0] = 1;
    lengths[1] = 2;
    lengths[2] = 1;
    lengths[3] = 2;
    lengths[4] = 2;
    lengths[5] = 2;
    lengths[6] = 1;
    lengths[7] = 2;
    lengths[8] = 1;
    lengths[9] = 2;
    lengths[10] = 2;
    lengths[11] = 1;
    lengths[12] = 2;
    lengths[13] = 2;
    lengths[14] = 2;
    lengths[15] = 1;
    lengths[16] = 1;
    lengths[17] = 2;
    lengths[18] = 1;
    lengths[19] = 2;
    lengths[20] = 3;
    lengths[21] = 2;
    lengths[22] = 1;
    lengths[23] = 2;
    lengths[24] = 1;
    lengths[25] = 2;
    lengths[26] = 2;
    lengths[27] = 3;
    lengths[28] = 1;
    lengths[29] = 1;
    lengths[30] = 2;
    lengths[31] = 1;
    lengths[32] = 2;
    lengths[33] = 2;
    lengths[34] = 1;
    kwStrArr[126].word = &(concatenated[57]);
    kwStrArr[126].indices  = &(lengths[34]);
    kwStrArr[126].wordCount =  1;


    kwStrArr[124].word = &(concatenated[52]);
    kwStrArr[124].indices  = &(lengths[31]);
    kwStrArr[124].wordCount =  3;


    kwStrArr[94].word = &(concatenated[49]);
    kwStrArr[94].indices  = &(lengths[29]);
    kwStrArr[94].wordCount =  2;


    kwStrArr[63].word = &(concatenated[48]);
    kwStrArr[63].indices  = &(lengths[28]);
    kwStrArr[63].wordCount =  1;


    kwStrArr[62].word = &(concatenated[40]);
    kwStrArr[62].indices  = &(lengths[24]);
    kwStrArr[62].wordCount =  4;


    kwStrArr[61].word = &(concatenated[37]);
    kwStrArr[61].indices  = &(lengths[22]);
    kwStrArr[61].wordCount =  2;


    kwStrArr[60].word = &(concatenated[29]);
    kwStrArr[60].indices  = &(lengths[18]);
    kwStrArr[60].wordCount =  4;


    kwStrArr[47].word = &(concatenated[26]);
    kwStrArr[47].indices  = &(lengths[16]);
    kwStrArr[47].wordCount =  2;


    kwStrArr[46].word = &(concatenated[25]);
    kwStrArr[46].indices  = &(lengths[15]);
    kwStrArr[46].wordCount =  1;


    kwStrArr[45].word = &(concatenated[18]);
    kwStrArr[45].indices  = &(lengths[11]);
    kwStrArr[45].wordCount =  4;


    kwStrArr[43].word = &(concatenated[13]);
    kwStrArr[43].indices  = &(lengths[8]);
    kwStrArr[43].wordCount =  3;


    kwStrArr[42].word = &(concatenated[10]);
    kwStrArr[42].indices  = &(lengths[6]);
    kwStrArr[42].wordCount =  2;


    kwStrArr[38].word = &(concatenated[6]);
    kwStrArr[38].indices  = &(lengths[4]);
    kwStrArr[38].wordCount =  2;


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
