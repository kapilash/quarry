/* generating code */
#include "quarry_internal.h"
#include <stdlib.h>
#include <stdio.h>


qu_KWTablePtr quarry_util_keywordTableJava(){
    unsigned char *concatenated = NULL;
    int *lengths = NULL;
    int index = 0;
    qu_KWTablePtr kwTablePtr = NULL;
    qu_KWsplPtr kwStrArr = NULL;

    int keywordCount = 16;
    int concatenatedLength = 289;
    int kwIndex = 0;

    kwTablePtr = (qu_KWTablePtr)malloc(sizeof(qu_KWTable));
    concatenated = (char *)malloc(289 * sizeof(unsigned char));
    lengths = (int*)malloc(50 * sizeof(int));
    kwStrArr = (qu_KWsplPtr)malloc(128 * sizeof(qu_KWspl));
    kwTablePtr->kwIndices = kwStrArr;
    kwTablePtr->word = concatenated;kwTablePtr->concatLength = concatenatedLength;
    kwTablePtr->indices = lengths;kwTablePtr->kwCount = keywordCount;

    for(index = 0; index<128;index++){
        kwStrArr[index].word = NULL;
        kwStrArr[index].indices = NULL;
        kwStrArr[index].wordCount = 0;
    }
    concatenated[0] = 97;
    concatenated[1] = 98;
    concatenated[2] = 115;
    concatenated[3] = 116;
    concatenated[4] = 114;
    concatenated[5] = 97;
    concatenated[6] = 99;
    concatenated[7] = 116;
    concatenated[8] = 97;
    concatenated[9] = 115;
    concatenated[10] = 115;
    concatenated[11] = 101;
    concatenated[12] = 114;
    concatenated[13] = 116;
    concatenated[14] = 98;
    concatenated[15] = 111;
    concatenated[16] = 111;
    concatenated[17] = 108;
    concatenated[18] = 101;
    concatenated[19] = 97;
    concatenated[20] = 110;
    concatenated[21] = 98;
    concatenated[22] = 114;
    concatenated[23] = 101;
    concatenated[24] = 97;
    concatenated[25] = 107;
    concatenated[26] = 98;
    concatenated[27] = 121;
    concatenated[28] = 116;
    concatenated[29] = 101;
    concatenated[30] = 99;
    concatenated[31] = 97;
    concatenated[32] = 115;
    concatenated[33] = 101;
    concatenated[34] = 99;
    concatenated[35] = 97;
    concatenated[36] = 116;
    concatenated[37] = 99;
    concatenated[38] = 104;
    concatenated[39] = 99;
    concatenated[40] = 104;
    concatenated[41] = 97;
    concatenated[42] = 114;
    concatenated[43] = 99;
    concatenated[44] = 108;
    concatenated[45] = 97;
    concatenated[46] = 115;
    concatenated[47] = 115;
    concatenated[48] = 99;
    concatenated[49] = 111;
    concatenated[50] = 110;
    concatenated[51] = 115;
    concatenated[52] = 116;
    concatenated[53] = 99;
    concatenated[54] = 111;
    concatenated[55] = 110;
    concatenated[56] = 116;
    concatenated[57] = 105;
    concatenated[58] = 110;
    concatenated[59] = 117;
    concatenated[60] = 101;
    concatenated[61] = 100;
    concatenated[62] = 101;
    concatenated[63] = 102;
    concatenated[64] = 97;
    concatenated[65] = 117;
    concatenated[66] = 108;
    concatenated[67] = 116;
    concatenated[68] = 100;
    concatenated[69] = 111;
    concatenated[70] = 100;
    concatenated[71] = 111;
    concatenated[72] = 117;
    concatenated[73] = 98;
    concatenated[74] = 108;
    concatenated[75] = 101;
    concatenated[76] = 101;
    concatenated[77] = 108;
    concatenated[78] = 115;
    concatenated[79] = 101;
    concatenated[80] = 101;
    concatenated[81] = 110;
    concatenated[82] = 117;
    concatenated[83] = 109;
    concatenated[84] = 101;
    concatenated[85] = 120;
    concatenated[86] = 116;
    concatenated[87] = 101;
    concatenated[88] = 110;
    concatenated[89] = 100;
    concatenated[90] = 115;
    concatenated[91] = 102;
    concatenated[92] = 105;
    concatenated[93] = 110;
    concatenated[94] = 97;
    concatenated[95] = 108;
    concatenated[96] = 102;
    concatenated[97] = 105;
    concatenated[98] = 110;
    concatenated[99] = 97;
    concatenated[100] = 108;
    concatenated[101] = 108;
    concatenated[102] = 121;
    concatenated[103] = 102;
    concatenated[104] = 108;
    concatenated[105] = 111;
    concatenated[106] = 97;
    concatenated[107] = 116;
    concatenated[108] = 102;
    concatenated[109] = 111;
    concatenated[110] = 114;
    concatenated[111] = 103;
    concatenated[112] = 111;
    concatenated[113] = 116;
    concatenated[114] = 111;
    concatenated[115] = 105;
    concatenated[116] = 102;
    concatenated[117] = 105;
    concatenated[118] = 109;
    concatenated[119] = 112;
    concatenated[120] = 108;
    concatenated[121] = 101;
    concatenated[122] = 109;
    concatenated[123] = 101;
    concatenated[124] = 110;
    concatenated[125] = 116;
    concatenated[126] = 115;
    concatenated[127] = 105;
    concatenated[128] = 109;
    concatenated[129] = 112;
    concatenated[130] = 111;
    concatenated[131] = 114;
    concatenated[132] = 116;
    concatenated[133] = 105;
    concatenated[134] = 110;
    concatenated[135] = 115;
    concatenated[136] = 116;
    concatenated[137] = 97;
    concatenated[138] = 110;
    concatenated[139] = 99;
    concatenated[140] = 101;
    concatenated[141] = 111;
    concatenated[142] = 102;
    concatenated[143] = 105;
    concatenated[144] = 110;
    concatenated[145] = 116;
    concatenated[146] = 105;
    concatenated[147] = 110;
    concatenated[148] = 116;
    concatenated[149] = 101;
    concatenated[150] = 114;
    concatenated[151] = 102;
    concatenated[152] = 97;
    concatenated[153] = 99;
    concatenated[154] = 101;
    concatenated[155] = 108;
    concatenated[156] = 111;
    concatenated[157] = 110;
    concatenated[158] = 103;
    concatenated[159] = 110;
    concatenated[160] = 97;
    concatenated[161] = 116;
    concatenated[162] = 105;
    concatenated[163] = 118;
    concatenated[164] = 101;
    concatenated[165] = 110;
    concatenated[166] = 101;
    concatenated[167] = 119;
    concatenated[168] = 112;
    concatenated[169] = 97;
    concatenated[170] = 99;
    concatenated[171] = 107;
    concatenated[172] = 97;
    concatenated[173] = 103;
    concatenated[174] = 101;
    concatenated[175] = 112;
    concatenated[176] = 114;
    concatenated[177] = 105;
    concatenated[178] = 118;
    concatenated[179] = 97;
    concatenated[180] = 116;
    concatenated[181] = 101;
    concatenated[182] = 112;
    concatenated[183] = 114;
    concatenated[184] = 111;
    concatenated[185] = 116;
    concatenated[186] = 101;
    concatenated[187] = 99;
    concatenated[188] = 116;
    concatenated[189] = 101;
    concatenated[190] = 100;
    concatenated[191] = 112;
    concatenated[192] = 117;
    concatenated[193] = 98;
    concatenated[194] = 108;
    concatenated[195] = 105;
    concatenated[196] = 99;
    concatenated[197] = 114;
    concatenated[198] = 101;
    concatenated[199] = 116;
    concatenated[200] = 117;
    concatenated[201] = 114;
    concatenated[202] = 110;
    concatenated[203] = 115;
    concatenated[204] = 104;
    concatenated[205] = 111;
    concatenated[206] = 114;
    concatenated[207] = 116;
    concatenated[208] = 115;
    concatenated[209] = 116;
    concatenated[210] = 97;
    concatenated[211] = 116;
    concatenated[212] = 105;
    concatenated[213] = 99;
    concatenated[214] = 115;
    concatenated[215] = 116;
    concatenated[216] = 114;
    concatenated[217] = 105;
    concatenated[218] = 99;
    concatenated[219] = 116;
    concatenated[220] = 102;
    concatenated[221] = 112;
    concatenated[222] = 115;
    concatenated[223] = 117;
    concatenated[224] = 112;
    concatenated[225] = 101;
    concatenated[226] = 114;
    concatenated[227] = 115;
    concatenated[228] = 119;
    concatenated[229] = 105;
    concatenated[230] = 116;
    concatenated[231] = 99;
    concatenated[232] = 104;
    concatenated[233] = 115;
    concatenated[234] = 121;
    concatenated[235] = 110;
    concatenated[236] = 99;
    concatenated[237] = 104;
    concatenated[238] = 114;
    concatenated[239] = 111;
    concatenated[240] = 110;
    concatenated[241] = 105;
    concatenated[242] = 122;
    concatenated[243] = 101;
    concatenated[244] = 100;
    concatenated[245] = 116;
    concatenated[246] = 104;
    concatenated[247] = 105;
    concatenated[248] = 115;
    concatenated[249] = 116;
    concatenated[250] = 104;
    concatenated[251] = 114;
    concatenated[252] = 111;
    concatenated[253] = 119;
    concatenated[254] = 116;
    concatenated[255] = 104;
    concatenated[256] = 114;
    concatenated[257] = 111;
    concatenated[258] = 119;
    concatenated[259] = 115;
    concatenated[260] = 116;
    concatenated[261] = 114;
    concatenated[262] = 97;
    concatenated[263] = 110;
    concatenated[264] = 115;
    concatenated[265] = 105;
    concatenated[266] = 101;
    concatenated[267] = 110;
    concatenated[268] = 116;
    concatenated[269] = 116;
    concatenated[270] = 114;
    concatenated[271] = 121;
    concatenated[272] = 118;
    concatenated[273] = 111;
    concatenated[274] = 105;
    concatenated[275] = 100;
    concatenated[276] = 118;
    concatenated[277] = 111;
    concatenated[278] = 108;
    concatenated[279] = 97;
    concatenated[280] = 116;
    concatenated[281] = 105;
    concatenated[282] = 108;
    concatenated[283] = 101;
    concatenated[284] = 119;
    concatenated[285] = 104;
    concatenated[286] = 105;
    concatenated[287] = 108;
    concatenated[288] = 101;
    lengths[0] = 8;
    lengths[1] = 6;
    lengths[2] = 7;
    lengths[3] = 5;
    lengths[4] = 4;
    lengths[5] = 4;
    lengths[6] = 5;
    lengths[7] = 4;
    lengths[8] = 5;
    lengths[9] = 5;
    lengths[10] = 8;
    lengths[11] = 7;
    lengths[12] = 2;
    lengths[13] = 6;
    lengths[14] = 4;
    lengths[15] = 4;
    lengths[16] = 7;
    lengths[17] = 5;
    lengths[18] = 7;
    lengths[19] = 5;
    lengths[20] = 3;
    lengths[21] = 4;
    lengths[22] = 2;
    lengths[23] = 10;
    lengths[24] = 6;
    lengths[25] = 10;
    lengths[26] = 3;
    lengths[27] = 9;
    lengths[28] = 4;
    lengths[29] = 6;
    lengths[30] = 3;
    lengths[31] = 7;
    lengths[32] = 7;
    lengths[33] = 9;
    lengths[34] = 6;
    lengths[35] = 6;
    lengths[36] = 5;
    lengths[37] = 6;
    lengths[38] = 8;
    lengths[39] = 5;
    lengths[40] = 6;
    lengths[41] = 12;
    lengths[42] = 4;
    lengths[43] = 5;
    lengths[44] = 6;
    lengths[45] = 9;
    lengths[46] = 3;
    lengths[47] = 4;
    lengths[48] = 8;
    lengths[49] = 5;
    kwStrArr[119].word = &(concatenated[284]);
    kwStrArr[119].indices  = &(lengths[49]);
    kwStrArr[119].wordCount =  1;


    kwStrArr[118].word = &(concatenated[272]);
    kwStrArr[118].indices  = &(lengths[47]);
    kwStrArr[118].wordCount =  2;


    kwStrArr[116].word = &(concatenated[245]);
    kwStrArr[116].indices  = &(lengths[42]);
    kwStrArr[116].wordCount =  5;


    kwStrArr[115].word = &(concatenated[203]);
    kwStrArr[115].indices  = &(lengths[36]);
    kwStrArr[115].wordCount =  6;


    kwStrArr[114].word = &(concatenated[197]);
    kwStrArr[114].indices  = &(lengths[35]);
    kwStrArr[114].wordCount =  1;


    kwStrArr[112].word = &(concatenated[168]);
    kwStrArr[112].indices  = &(lengths[31]);
    kwStrArr[112].wordCount =  4;


    kwStrArr[110].word = &(concatenated[159]);
    kwStrArr[110].indices  = &(lengths[29]);
    kwStrArr[110].wordCount =  2;


    kwStrArr[108].word = &(concatenated[155]);
    kwStrArr[108].indices  = &(lengths[28]);
    kwStrArr[108].wordCount =  1;


    kwStrArr[105].word = &(concatenated[115]);
    kwStrArr[105].indices  = &(lengths[22]);
    kwStrArr[105].wordCount =  6;


    kwStrArr[103].word = &(concatenated[111]);
    kwStrArr[103].indices  = &(lengths[21]);
    kwStrArr[103].wordCount =  1;


    kwStrArr[102].word = &(concatenated[91]);
    kwStrArr[102].indices  = &(lengths[17]);
    kwStrArr[102].wordCount =  4;


    kwStrArr[101].word = &(concatenated[76]);
    kwStrArr[101].indices  = &(lengths[14]);
    kwStrArr[101].wordCount =  3;


    kwStrArr[100].word = &(concatenated[61]);
    kwStrArr[100].indices  = &(lengths[11]);
    kwStrArr[100].wordCount =  3;


    kwStrArr[99].word = &(concatenated[30]);
    kwStrArr[99].indices  = &(lengths[5]);
    kwStrArr[99].wordCount =  6;


    kwStrArr[98].word = &(concatenated[14]);
    kwStrArr[98].indices  = &(lengths[2]);
    kwStrArr[98].wordCount =  3;


    kwStrArr[97].word = &(concatenated[0]);
    kwStrArr[97].indices  = &(lengths[0]);
    kwStrArr[97].wordCount =  2;


    for(index = 0; index<128;index++){
        kwStrArr[index].beginKWId = kwIndex;
        kwIndex = kwIndex + kwStrArr[index].wordCount;
    }
    return kwTablePtr;
}
