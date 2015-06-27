/*
Copyright (c) 2013, Hemanth Kapila
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1) Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2) Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
The name of the Hemanth Kapila may NOT be used to endorse or promote products derived from this software without specific prior written permission.
3) THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#pragma once
#include "quarry_export.h"

#ifdef __cplusplus
extern "C" {
#endif    

    struct quarry_Token {
	int line;
	int column;
	unsigned int tokenType;
	size_t length;
	unsigned char *textPtr;
	void *opaque;
    };

    QUARRY_EXPORT void *quarry_fromFile(int lang, const char *fileName);
    QUARRY_EXPORT void *quarry_fromStr(int lang, const unsigned char *byteArray, unsigned long length, int l, int col);

    QUARRY_EXPORT struct quarry_Token *quarry_nextToken(void *quarry);

    QUARRY_EXPORT void quarry_freeToken(struct quarry_Token *t);

    QUARRY_EXPORT void quarry_moveToStr(void *quarry, const unsigned char *byteArray, unsigned long length, int l, int col);
    QUARRY_EXPORT void quarry_moveToFile(void *quarry, const char *fileName);
    QUARRY_EXPORT void quarry_close(void *p);

    QUARRY_EXPORT unsigned int quarry_numberType(struct quarry_Token *token);
    QUARRY_EXPORT int quarry_toInt(struct quarry_Token *token);
    /*    QUARRY_EXPORT unsigned int quarry_toUnsignedInt(struct quarry_Token *token);
    QUARRY_EXPORT unsigned long long quarry_toUnsignedLongLong(struct quarry_Token *token);
    QUARRY_EXPORT unsigned long quarry_toUnsignedLong(struct quarry_Token *token);
    QUARRY_EXPORT long long quarry_toLongLong(struct quarry_Token *token);
    QUARRY_EXPORT long quarry_toLong(struct quarry_Token *token);
    QUARRY_EXPORT float quarry_toFloat(struct quarry_Token *token);
    QUARRY_EXPORT double quarry_toUnsignedLong(struct quarry_Token *token);*/
    QUARRY_EXPORT unsigned int quarry_toChar(struct quarry_Token *token); 
    
#ifdef __cplusplus    
}
#endif
