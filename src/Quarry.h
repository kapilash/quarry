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

    /** \brief The structure representing each token.
       
     */
    struct quarry_Token {
	int line; /**< line number */
	int column; /**< column number */
	unsigned int tokenType; /**< code representing the token */
	size_t length; /**< Length of the token. */
	unsigned char *textPtr; /**< The token spans for *length* bytes beginning from this pointer. 
				    **NOTE** The text is not a C string. It does not end with '\0' */
	void *opaque; /**< An internal pointer */
    };
    typedef struct quarry_Token quarry_TokenStr;



    /** \brief construct a quarry instance from a file.

	This procedure returns an opaque quarry object.
     */
    QUARRY_EXPORT void *quarry_fromFile(int lang, const char *fileName);

    /** \brief construct an instance of quarry from an array of UTF-8 bytes
     */
    QUARRY_EXPORT void *quarry_fromStr(int lang, const unsigned char *byteArray, unsigned long length, int l, int col);

    /** \brief next token from the quarry instance.

	If the input has been consumed, this method returns a token where tokentype is EOF.
     */
    QUARRY_EXPORT struct quarry_Token *quarry_nextToken(void *quarry);

    /** \brief releasing the token

	Use this to free the memory of token instance. Do not use free object. It will lead to memory leaks.
     */
    QUARRY_EXPORT void quarry_freeToken(struct quarry_Token *t);

    /** \brief use this method to set a quarry object to point to a new source of input

	If you have more than one string to scan, using this will be quicker.
     */
    QUARRY_EXPORT void quarry_moveToStr(void *quarry, const unsigned char *byteArray, unsigned long length, int l, int col);

    /** \brief use this method to set a quarry object to point to a new source of input

	If you have more than one string to scan, using this will be quicker.
     */    
    QUARRY_EXPORT void quarry_moveToFile(void *quarry, const char *fileName);

    /** \brief closing the quarry instance

	Use this method and not free or delete for freeing the resources related to a quarry instance.
     */

    QUARRY_EXPORT void quarry_close(void *p);

    
    QUARRY_EXPORT unsigned int quarry_numberType(struct quarry_Token *token);

    QUARRY_EXPORT int quarry_toInt(struct quarry_Token *token);
    QUARRY_EXPORT unsigned int quarry_toUInt(struct quarry_Token *token);
    QUARRY_EXPORT long long quarry_toLong(struct quarry_Token *token);
    QUARRY_EXPORT unsigned  long long quarry_toULong(struct quarry_Token *token);

    QUARRY_EXPORT float quarry_toFloat(struct quarry_Token *token);
    QUARRY_EXPORT double quarry_toDouble(struct quarry_Token *token);

    QUARRY_EXPORT unsigned int quarry_toChar(struct quarry_Token *token); 
    
#ifdef __cplusplus    
}
#endif
