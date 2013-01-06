/*
Copyright (c) 2013, Hemanth Kapila
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1) Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2) Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
The name of the Hemanth Kapila may be used to endorse or promote products derived from this software without specific prior written permission.
3) THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
/* generating code */
#include "quarry_internal.h"
#include <stdlib.h>
#include <stdio.h>


qu_KWTablePtr quarry_util_keywordTableC(){
    unsigned char *concatenated = NULL;
    int *lengths = NULL;
    int index = 0;
    qu_KWTablePtr kwTablePtr = NULL;
    qu_KWsplPtr kwStrArr = NULL;

    int keywordCount = 16;
    int concatenatedLength = 203;

    kwTablePtr = (qu_KWTablePtr)malloc(sizeof(qu_KWTable));
    concatenated = (char *)malloc(203 * sizeof(unsigned char));
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
    concatenated[0] = 95;
    concatenated[1] = 66;
    concatenated[2] = 111;
    concatenated[3] = 111;
    concatenated[4] = 108;
    concatenated[5] = 95;
    concatenated[6] = 67;
    concatenated[7] = 111;
    concatenated[8] = 109;
    concatenated[9] = 112;
    concatenated[10] = 108;
    concatenated[11] = 101;
    concatenated[12] = 120;
    concatenated[13] = 95;
    concatenated[14] = 73;
    concatenated[15] = 109;
    concatenated[16] = 97;
    concatenated[17] = 103;
    concatenated[18] = 105;
    concatenated[19] = 110;
    concatenated[20] = 97;
    concatenated[21] = 114;
    concatenated[22] = 121;
    concatenated[23] = 97;
    concatenated[24] = 117;
    concatenated[25] = 116;
    concatenated[26] = 111;
    concatenated[27] = 98;
    concatenated[28] = 114;
    concatenated[29] = 101;
    concatenated[30] = 97;
    concatenated[31] = 107;
    concatenated[32] = 99;
    concatenated[33] = 97;
    concatenated[34] = 115;
    concatenated[35] = 101;
    concatenated[36] = 99;
    concatenated[37] = 104;
    concatenated[38] = 97;
    concatenated[39] = 114;
    concatenated[40] = 99;
    concatenated[41] = 111;
    concatenated[42] = 110;
    concatenated[43] = 115;
    concatenated[44] = 116;
    concatenated[45] = 99;
    concatenated[46] = 111;
    concatenated[47] = 110;
    concatenated[48] = 116;
    concatenated[49] = 105;
    concatenated[50] = 110;
    concatenated[51] = 117;
    concatenated[52] = 101;
    concatenated[53] = 100;
    concatenated[54] = 101;
    concatenated[55] = 102;
    concatenated[56] = 97;
    concatenated[57] = 117;
    concatenated[58] = 108;
    concatenated[59] = 116;
    concatenated[60] = 100;
    concatenated[61] = 111;
    concatenated[62] = 100;
    concatenated[63] = 111;
    concatenated[64] = 117;
    concatenated[65] = 98;
    concatenated[66] = 108;
    concatenated[67] = 101;
    concatenated[68] = 101;
    concatenated[69] = 108;
    concatenated[70] = 115;
    concatenated[71] = 101;
    concatenated[72] = 101;
    concatenated[73] = 110;
    concatenated[74] = 117;
    concatenated[75] = 109;
    concatenated[76] = 101;
    concatenated[77] = 120;
    concatenated[78] = 116;
    concatenated[79] = 101;
    concatenated[80] = 114;
    concatenated[81] = 110;
    concatenated[82] = 102;
    concatenated[83] = 108;
    concatenated[84] = 111;
    concatenated[85] = 97;
    concatenated[86] = 116;
    concatenated[87] = 102;
    concatenated[88] = 111;
    concatenated[89] = 114;
    concatenated[90] = 103;
    concatenated[91] = 111;
    concatenated[92] = 116;
    concatenated[93] = 111;
    concatenated[94] = 105;
    concatenated[95] = 102;
    concatenated[96] = 105;
    concatenated[97] = 110;
    concatenated[98] = 108;
    concatenated[99] = 105;
    concatenated[100] = 110;
    concatenated[101] = 101;
    concatenated[102] = 105;
    concatenated[103] = 110;
    concatenated[104] = 116;
    concatenated[105] = 108;
    concatenated[106] = 111;
    concatenated[107] = 110;
    concatenated[108] = 103;
    concatenated[109] = 114;
    concatenated[110] = 101;
    concatenated[111] = 103;
    concatenated[112] = 105;
    concatenated[113] = 115;
    concatenated[114] = 116;
    concatenated[115] = 101;
    concatenated[116] = 114;
    concatenated[117] = 114;
    concatenated[118] = 101;
    concatenated[119] = 115;
    concatenated[120] = 116;
    concatenated[121] = 114;
    concatenated[122] = 105;
    concatenated[123] = 99;
    concatenated[124] = 116;
    concatenated[125] = 114;
    concatenated[126] = 101;
    concatenated[127] = 116;
    concatenated[128] = 117;
    concatenated[129] = 114;
    concatenated[130] = 110;
    concatenated[131] = 115;
    concatenated[132] = 104;
    concatenated[133] = 111;
    concatenated[134] = 114;
    concatenated[135] = 116;
    concatenated[136] = 115;
    concatenated[137] = 105;
    concatenated[138] = 103;
    concatenated[139] = 110;
    concatenated[140] = 101;
    concatenated[141] = 100;
    concatenated[142] = 115;
    concatenated[143] = 105;
    concatenated[144] = 122;
    concatenated[145] = 101;
    concatenated[146] = 111;
    concatenated[147] = 102;
    concatenated[148] = 115;
    concatenated[149] = 116;
    concatenated[150] = 97;
    concatenated[151] = 116;
    concatenated[152] = 105;
    concatenated[153] = 99;
    concatenated[154] = 115;
    concatenated[155] = 116;
    concatenated[156] = 114;
    concatenated[157] = 117;
    concatenated[158] = 99;
    concatenated[159] = 116;
    concatenated[160] = 115;
    concatenated[161] = 119;
    concatenated[162] = 105;
    concatenated[163] = 116;
    concatenated[164] = 99;
    concatenated[165] = 104;
    concatenated[166] = 116;
    concatenated[167] = 121;
    concatenated[168] = 112;
    concatenated[169] = 101;
    concatenated[170] = 100;
    concatenated[171] = 101;
    concatenated[172] = 102;
    concatenated[173] = 117;
    concatenated[174] = 110;
    concatenated[175] = 105;
    concatenated[176] = 111;
    concatenated[177] = 110;
    concatenated[178] = 117;
    concatenated[179] = 110;
    concatenated[180] = 115;
    concatenated[181] = 105;
    concatenated[182] = 103;
    concatenated[183] = 110;
    concatenated[184] = 101;
    concatenated[185] = 100;
    concatenated[186] = 118;
    concatenated[187] = 111;
    concatenated[188] = 105;
    concatenated[189] = 100;
    concatenated[190] = 118;
    concatenated[191] = 111;
    concatenated[192] = 108;
    concatenated[193] = 97;
    concatenated[194] = 116;
    concatenated[195] = 105;
    concatenated[196] = 108;
    concatenated[197] = 101;
    concatenated[198] = 119;
    concatenated[199] = 104;
    concatenated[200] = 105;
    concatenated[201] = 108;
    concatenated[202] = 101;
    lengths[0] = 5;
    lengths[1] = 8;
    lengths[2] = 10;
    lengths[3] = 4;
    lengths[4] = 5;
    lengths[5] = 4;
    lengths[6] = 4;
    lengths[7] = 5;
    lengths[8] = 8;
    lengths[9] = 7;
    lengths[10] = 2;
    lengths[11] = 6;
    lengths[12] = 4;
    lengths[13] = 4;
    lengths[14] = 6;
    lengths[15] = 5;
    lengths[16] = 3;
    lengths[17] = 4;
    lengths[18] = 2;
    lengths[19] = 6;
    lengths[20] = 3;
    lengths[21] = 4;
    lengths[22] = 8;
    lengths[23] = 8;
    lengths[24] = 6;
    lengths[25] = 5;
    lengths[26] = 6;
    lengths[27] = 6;
    lengths[28] = 6;
    lengths[29] = 6;
    lengths[30] = 6;
    lengths[31] = 7;
    lengths[32] = 5;
    lengths[33] = 8;
    lengths[34] = 4;
    lengths[35] = 8;
    lengths[36] = 5;
    kwStrArr[119].word = &(concatenated[198]);
    kwStrArr[119].indices  = &(lengths[36]);
    kwStrArr[119].wordCount =  1;


    kwStrArr[118].word = &(concatenated[186]);
    kwStrArr[118].indices  = &(lengths[34]);
    kwStrArr[118].wordCount =  2;


    kwStrArr[117].word = &(concatenated[173]);
    kwStrArr[117].indices  = &(lengths[32]);
    kwStrArr[117].wordCount =  2;


    kwStrArr[116].word = &(concatenated[166]);
    kwStrArr[116].indices  = &(lengths[31]);
    kwStrArr[116].wordCount =  1;


    kwStrArr[115].word = &(concatenated[131]);
    kwStrArr[115].indices  = &(lengths[25]);
    kwStrArr[115].wordCount =  6;


    kwStrArr[114].word = &(concatenated[109]);
    kwStrArr[114].indices  = &(lengths[22]);
    kwStrArr[114].wordCount =  3;


    kwStrArr[108].word = &(concatenated[105]);
    kwStrArr[108].indices  = &(lengths[21]);
    kwStrArr[108].wordCount =  1;


    kwStrArr[105].word = &(concatenated[94]);
    kwStrArr[105].indices  = &(lengths[18]);
    kwStrArr[105].wordCount =  3;


    kwStrArr[103].word = &(concatenated[90]);
    kwStrArr[103].indices  = &(lengths[17]);
    kwStrArr[103].wordCount =  1;


    kwStrArr[102].word = &(concatenated[82]);
    kwStrArr[102].indices  = &(lengths[15]);
    kwStrArr[102].wordCount =  2;


    kwStrArr[101].word = &(concatenated[68]);
    kwStrArr[101].indices  = &(lengths[12]);
    kwStrArr[101].wordCount =  3;


    kwStrArr[100].word = &(concatenated[53]);
    kwStrArr[100].indices  = &(lengths[9]);
    kwStrArr[100].wordCount =  3;


    kwStrArr[99].word = &(concatenated[32]);
    kwStrArr[99].indices  = &(lengths[5]);
    kwStrArr[99].wordCount =  4;


    kwStrArr[98].word = &(concatenated[27]);
    kwStrArr[98].indices  = &(lengths[4]);
    kwStrArr[98].wordCount =  1;


    kwStrArr[97].word = &(concatenated[23]);
    kwStrArr[97].indices  = &(lengths[3]);
    kwStrArr[97].wordCount =  1;


    kwStrArr[95].word = &(concatenated[0]);
    kwStrArr[95].indices  = &(lengths[0]);
    kwStrArr[95].wordCount =  3;


    return kwTablePtr;
}
