/*
Copyright (c) Hemanth Kapila
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
The name of Hemanth Kapila may NOT be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#pragma once

#include <string>
#include <boost/functional/hash.hpp>
#include "quarry_export.h"

namespace Quarry {
    enum TokenType {
	ERROR,
	KEYWORD,
	IDENT,
	STRING,
	CHAR,
	NUMBER,
	OPERATOR,
	OPEN_BRACE,
	CLOSE_BRACE,
	OPEN_BRACKET,
	CLOSE_BRACKET,
	SQUARE_OPEN,
	SQUARE_CLOSE,
	DOT,
	COLON,
	SEMI_COLON,
	QUESTION_MARK,
	COMMA,
	COMMENT,
	WHITESPACE,
	NEWLINE,
	META_ID,
	BOOL,
	QEOF
    };

    enum QNumberType {
	QComplexFloat,
	QComplexDouble,
	QComplexLongDouble,
	QLongDouble,
	QDouble,
	QFloat,
	QInt,
	QLong,
	QLongLong,
	QUnsignedInt,
	QUnsignedLong,
	QUnsignedLongLong,
	QRational,
    };

    class Token{
    public:
	const int line;
	const int column;
	enum TokenType tokenType;

	QUARRY_EXPORT Token(int line, int column, enum TokenType t);
	QUARRY_EXPORT Token(const Token &other);
	QUARRY_EXPORT virtual ~Token() {}
    };

    template <typename T, enum TokenType tokenType>
	class GenericToken : public Token{
    public:
	const T value;
	QUARRY_EXPORT GenericToken(int line, int column, T value): Token(line,column, tokenType), value(value) {}
	template <typename K, enum TokenType Y>
	    GenericToken(const GenericToken<K,Y> &other): Token(other.line,other.column, tokenType), value(other.value) {}
    };

    class CharToken : public Token{
    public:
	const char32_t value;
	QUARRY_EXPORT CharToken(int line, int column, char32_t value);
	
        QUARRY_EXPORT CharToken(const CharToken &other);
    };

    class ErrorToken : public Token{
    public:
      const std::string value;
      QUARRY_EXPORT ErrorToken(int line, int column, std::string text);
      
      QUARRY_EXPORT ErrorToken(const ErrorToken &other);
    };

    class StringToken : public Token{
    public:
      const std::u32string value;
      QUARRY_EXPORT StringToken(int line, int column, std::u32string text);
      
      QUARRY_EXPORT StringToken(const StringToken &other);
    };

    class IdentifierToken : public Token{
    public:
      const std::string value;
      QUARRY_EXPORT IdentifierToken(int line, int column, std::string text);
      
      QUARRY_EXPORT IdentifierToken(const IdentifierToken &other);
    };

    class NumberToken : public Token {
    public:
	const long double asDouble;
	const long long asLong;
	const bool isSigned;
	const bool isExact;
	QUARRY_EXPORT NumberToken(int line, int column, long double d);
	QUARRY_EXPORT NumberToken(int line, int column, long long l, bool hasSign=false);
	QUARRY_EXPORT NumberToken(const NumberToken &other);
    };

    typedef GenericToken<int, KEYWORD> Keyword;

    typedef GenericToken<std::string, META_ID> MetaToken;
    typedef GenericToken<unsigned int, META_ID> ReferredId; 
    typedef GenericToken<double, NUMBER> DoubleToken;
    typedef GenericToken<float, NUMBER> FloatToken;
    typedef GenericToken<bool, BOOL> BoolToken;
    typedef GenericToken<std::string, COMMENT> CommentToken;
}
