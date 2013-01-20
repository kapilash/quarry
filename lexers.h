/*
Copyright (c) 2013, Hemanth Kapila
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1) Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2) Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
The name of the Hemanth Kapila may NOT be used to endorse or promote products derived from this software without specific prior written permission.
3) THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
#include "quarry_internal.h"

int quarry_commentLexer(quarry_QuarryPtr quarryPointer, int lexerState);

int quarry_punctuation(quarry_QuarryPtr quarry, int lexerState);

int quarry_operatorLexer(quarry_QuarryPtr quarry, int lexerState);

int quarry_groupLexer(quarry_QuarryPtr quarry, int lexerState);

int quarry_wsLexer(quarry_QuarryPtr quarry, int lexerState);

int quarry_sqLexer(quarry_QuarryPtr quarry, int lexerState);

int quarry_dqLexer(quarry_QuarryPtr quarry, int lexerState);

int quarry_decimalLexer(quarry_QuarryPtr quarry, int lexerState);

int quarry_hexBinOct0Lexer(quarry_QuarryPtr quarry, int lexerState);

int quarry_lfLexer(quarry_QuarryPtr quarry, int lexerState);

int quarry_crlfLexer(quarry_QuarryPtr quarry, int lexerState);

int quarry_errLexer(quarry_QuarryPtr quarry,int lexerState);

int quarry_idLexer(quarry_QuarryPtr quarry, int lexerState);

int quarry_metaIdLexer(quarry_QuarryPtr quarry, int lexerState);
