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
    /*Token:: Token(int l, int c, enum TokenType t)  : line(l), column(c), tokenType(t), length(0), textPtr(nullptr)
    {
    }*/

    Token:: Token(int l, int c, enum TokenType t, std::size_t len, const unsigned char *p )  : line(l), column(c), tokenType(t), length(len), textPtr(p)
    {
    }

    void Token::writeTo(std::ostream &out) {
	
	out << "{(" << line << "," << column << ")" << tokenTypeStrs[tokenType] << ": ("  <<  length << ")" << inputPtrToString(textPtr, length) << std::endl;
    }

    std::string inputPtrToString(const unsigned char *ptr, std::size_t len) {
	if (ptr == nullptr) {
	    return "<null>";
	}
	std::string text;
	for(int i = 0; i < len; i++) {
	    text.push_back(ptr[i]);
	}
	return text;
    }
}
