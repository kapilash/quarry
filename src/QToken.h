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
#include "quarry_export.h"
#include <iostream>

namespace Quarry {
    enum PL {
	C = 0,
	JAVA
    };

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

    static std::string tokenTypeStrs[] = {
    "ERROR",
    "KEYWORD",
    "IDENT",
    "STRING",
    "CHAR",
    "NUMBER",
    "OPERATOR",
    "OPEN_BRACE",
    "CLOSE_BRACE",
    "OPEN_BRACKET",
    "CLOSE_BRACKET",
    "SQUARE_OPEN",
    "SQUARE_CLOSE",
    "DOT",
    "COLON",
    "SEMI_COLON",
    "QUESTION_MARK",
    "COMMA",
    "COMMENT",
    "WHITESPACE",
    "NEWLINE",
    "META_ID",
    "BOOL",
    "QEOF"
    };
    
    enum QNumberType {
	QLongDouble = 1,
	QDouble,
	QFloat,
	QInt,
	QLong,
	QLongLong,
	QUnsignedInt,
	QUnsignedLong,
	QUnsignedLongLong,
    };

    static std::string numberTypeStrs[] = {
	"<Not Number>",
    "Long Double",
    "Double",
    "Float",
    "Int",
    "Long",
    "Long Long",
    "Unsigned Int",
    "Unsigned Long",
    "Unsigned Long Long"
    };

    std::string inputPtrToString(const unsigned char *ptr, std::size_t len);
    
    class Token{
    public:
	const int line;
	const int column;
	const TokenType tokenType;
	const unsigned char *const textPtr;
	const std::size_t length;

	QUARRY_EXPORT  Token(int l, int c, enum TokenType t, std::size_t len, const unsigned char *p );
	
	QUARRY_EXPORT virtual void writeTo(std::ostream &out) ;

	QUARRY_EXPORT virtual ~Token() {}
    };

    template <typename T, TokenType genericType >
	class GenericToken : public Token{
    public:
        const T value;

	QUARRY_EXPORT GenericToken(int line, int column,const T &v, std::size_t len,const unsigned char *p)
	    : Token(line, column, genericType,len, p), value(v) {
	}

    };

    class NumericalToken : public Token {
    public:
	const QNumberType numberType;
	QUARRY_EXPORT NumericalToken(int line, int column, QNumberType nt, std::size_t len, const unsigned char *cptr)
	    :Token(line, column - len, NUMBER, len, cptr),
	    numberType(nt)
	 {
	 }
    };
    
    template <typename T, QNumberType nt>
    class NumberToken : public NumericalToken {
    public:
	const T value;

	QUARRY_EXPORT NumberToken(int line, int column,  const T v, std::size_t len, const unsigned char *cptr)
	    : NumericalToken(line, column, nt, len, cptr),
	    value(v) {}

	QUARRY_EXPORT virtual void writeTo(std::ostream &out) {
	    out << "{(" << line << "," << column << ")  " << value << " " <<  numberTypeStrs[nt] <<" [" << inputPtrToString(textPtr, length) << "]}" << std::endl;
	}
    };

    class ErrorToken : public Token {
    public:
	const std::string message;
	QUARRY_EXPORT ErrorToken(int line,int column, std::string msg) : Token(line, column, ERROR, 0, nullptr), message(msg) {}
    };
    typedef GenericToken<int, KEYWORD> Keyword;
    typedef GenericToken<int, OPERATOR> Operator;
    
    typedef GenericToken<std::u32string, STRING> StringToken;
    typedef GenericToken<std::string, IDENT> IdentifierToken;
    typedef GenericToken<std::string, IDENT> OperatorId;
    typedef GenericToken<char32_t, CHAR> CharToken;
    typedef GenericToken<std::string, META_ID> MetaToken;
    typedef GenericToken<unsigned int, META_ID> ReferredId; 
    typedef GenericToken<bool, BOOL> BoolToken;
    
    typedef NumberToken<unsigned int, QUnsignedInt> UIntToken;
    typedef NumberToken<unsigned long, QUnsignedLong> ULongToken;
    typedef NumberToken<unsigned long long, QUnsignedLongLong> ULongLongToken;

    typedef NumberToken<float, QFloat> FlToken;
    typedef NumberToken<double, QDouble> DblToken;
    typedef NumberToken<long double, QLongDouble> LDblToken;

    typedef NumberToken<int, QInt> IntToken;
    typedef NumberToken<long, QLong> LongToken;
    typedef NumberToken<long long, QLongLong> LongLongToken;


    QUARRY_EXPORT void* fromFile(PL lang, const char *file);
    QUARRY_EXPORT void* fromString(PL lang, const unsigned char *byteArray, unsigned long length, int line, int column);
    QUARRY_EXPORT void moveToFile(void *quarry, const char *fileName);
    QUARRY_EXPORT void moveToText(void *quarry,const unsigned char *byteArray, unsigned long length, int l, int col);
    QUARRY_EXPORT Token* nextToken(void *quarry);
    QUARRY_EXPORT void closeQuarry(void *opaque);

}
