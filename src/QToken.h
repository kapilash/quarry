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
#include <boost/lexical_cast.hpp>
#include "quarry_export.h"
#include <iostream>

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
	const TokenType tokenType;

	QUARRY_EXPORT Token(int line, int column, enum TokenType t);
	QUARRY_EXPORT Token(const Token &other);
	QUARRY_EXPORT virtual ~Token() {}
    };

    template <typename T, TokenType genericType >
	class GenericToken : public Token{
    public:
        const T value;
         QUARRY_EXPORT GenericToken(int line, int column,const T &v)
               : Token(line, column, genericType), value(v) {
	}
    };

    template <typename T, QNumberType nt>
    class NumberToken : public Token {
    public:
	const QNumberType numberType;
	const T value;
	const std::string text;

	QUARRY_EXPORT NumberToken(int line, int column, const std::string &txt)
	    : Token(line, column, NUMBER),
	    text(txt),
	    numberType(nt),
	    value(boost::lexical_cast<T>(txt)) {}
	
    };

    typedef GenericToken<int, KEYWORD> Keyword;
    typedef GenericToken<std::string, ERROR> ErrorToken;
    typedef GenericToken<std::u32string, STRING> StringToken;
    typedef GenericToken<std::u32string, IDENT> IdentifierToken;
    typedef GenericToken<char32_t, CHAR> CharToken;
    typedef GenericToken<std::u32string, META_ID> MetaToken;
    typedef GenericToken<unsigned int, META_ID> ReferredId; 
    typedef GenericToken<bool, BOOL> BoolToken;
    typedef GenericToken<std::string, COMMENT> CommentToken;
    typedef NumberToken<unsigned int, QUnsignedInt> UnsignedIntToken;
}
