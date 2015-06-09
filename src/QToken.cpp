/*
Copyright (c) Hemanth Kapila
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
The name of Hemanth Kapila may NOT be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <string>
#include "QToken.h"
#include "quarry_export.h"


namespace Quarry {
    void fillTokenContent(quarry_TokenPtr tokenPtr, const std::string &str)
    {
    }
    void fillTokenContent(quarry_TokenPtr tokenPtr, const int i)
    {
    }
    void fillTokenContent(quarry_TokenPtr tokenPtr, const unsigned int i)
    {
    }
    void fillTokenContent(quarry_TokenPtr tokenPtr, const std::u32string &str)
    {
    }
    void fillTokenContent(quarry_TokenPtr tokenPtr, const char32_t wideChar)
    {
    }
    void fillTokenContent(quarry_TokenPtr tokenPtr, const bool b)
    {
    }

    Token:: Token(int line, int column, enum TokenType t): line(line), column(column), tokenType(t)
    {

    }
    Token:: Token(const Token &other)  : line(other.line), column(other.column), tokenType(other.tokenType)
    {
    }

    quarry_TokenPtr Token:: toTokenPtr() const
    {
	quarry_TokenPtr tokenPtr = new quarry_Token();

	return tokenPtr;
    }

    
    quarry_TokenPtr NumericalToken::toTokenPtr() const
    {
	quarry_TokenPtr tokenPtr = new quarry_Token();

	return tokenPtr;
    }

    void fillExtra(unsigned char *info, unsigned char *extra, int i) { }
    void fillExtra(unsigned char *info, unsigned char *extra, unsigned int i) { }
    void fillExtra(unsigned char *info, unsigned char *extra, long l) { }
    void fillExtra(unsigned char *info, unsigned char *extra, unsigned long l) { }
    void fillExtra(unsigned char *info, unsigned char *extra, long long l) { }
    void fillExtra(unsigned char *info, unsigned char *extra, unsigned long long i) { }
    void fillExtra(unsigned char *info, unsigned char *extra, float f) { }
    void fillExtra(unsigned char *info, unsigned char *extra, double d) { }
    void fillExtra(unsigned char *info, unsigned char *extra, long double d) { }
}
