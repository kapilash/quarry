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
#include "quarry.h"
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
	QLongDouble,
	QDouble,
	QFloat,
	QInt,
	QLong,
	QLongLong,
	QUnsignedInt,
	QUnsignedLong,
	QUnsignedLongLong,
    };

    void fillTokenContent(quarry_TokenPtr tokenPtr, const std::string &str);
    void fillTokenContent(quarry_TokenPtr tokenPtr, const int i);
    void fillTokenContent(quarry_TokenPtr tokenPtr, const unsigned int i);
    void fillTokenContent(quarry_TokenPtr tokenPtr, const std::u32string &str);
    void fillTokenContent(quarry_TokenPtr tokenPtr, const char32_t wideChar);
    void fillTokenContent(quarry_TokenPtr tokenPtr, const bool b);

    void fillExtra(unsigned char *info, unsigned char *extra, int i);
    void fillExtra(unsigned char *info, unsigned char *extra, unsigned int i);
    void fillExtra(unsigned char *info, unsigned char *extra, long l);
    void fillExtra(unsigned char *info, unsigned char *extra, unsigned long l);
    void fillExtra(unsigned char *info, unsigned char *extra, long long l);
    void fillExtra(unsigned char *info, unsigned char *extra, unsigned long long i);
    void fillExtra(unsigned char *info, unsigned char *extra, float f);
    void fillExtra(unsigned char *info, unsigned char *extra, double d);
    void fillExtra(unsigned char *info, unsigned char *extra, long double d);
    
    class Token{
    public:
	const int line;
	const int column;
	const TokenType tokenType;

	QUARRY_EXPORT Token(int line, int column, enum TokenType t);
	QUARRY_EXPORT Token(const Token &other);
	QUARRY_EXPORT virtual quarry_TokenPtr toTokenPtr() const;
	QUARRY_EXPORT virtual void writeTo(std::ostream &out) {
	    out << "{(" << line << "," << column << ")" << tokenType << "}" << std::endl;
	}

	QUARRY_EXPORT virtual ~Token() {}
    };

    template <typename T, TokenType genericType >
	class GenericToken : public Token{
    public:
        const T value;
         QUARRY_EXPORT GenericToken(int line, int column,const T &v)
               : Token(line, column, genericType), value(v) {
	}
	QUARRY_EXPORT virtual quarry_TokenPtr toTokenPtr() const {
	    quarry_TokenPtr p = new quarry_Token();
	    p->tokenType = (int)genericType;
	    fillTokenContent(p, value);
	    return p;
	}

	QUARRY_EXPORT virtual void writeTo(std::ostream &out) {
	    out << "{(" << line << "," << column << ") type:" << tokenType << "and " <<   "}" << std::endl;
	}
    };

    class NumericalToken : public Token {
    public:
	const QNumberType numberType;
	const std::string text;
	QUARRY_EXPORT NumericalToken(int line, int column, QNumberType nt, const std::string &s)
	    :Token(line, column, NUMBER),
	    numberType(nt),
	    text(s)
	 {
	 }
	QUARRY_EXPORT virtual quarry_TokenPtr toTokenPtr() const ;
	virtual void fillExtraInfo(unsigned char *i, unsigned char *e){
	    
	}
    };
    
    template <typename T, QNumberType nt>
    class NumberToken : public NumericalToken {
    public:
	const T value;
	QUARRY_EXPORT NumberToken(int line, int column, const std::string &txt, const T v)
	    : NumericalToken(line, column, nt, txt),
	    value(v) {}

	virtual void fillExtraInfo(unsigned char *i, unsigned char *c){
	    fillExtra(i,c, value);
	}


	QUARRY_EXPORT virtual void writeTo(std::ostream &out) {
	    out << "{(" << line << "," << column << ")  " << value << " [" << text << "]}" << std::endl;
	}
    };

    typedef GenericToken<int, KEYWORD> Keyword;
    typedef GenericToken<int, OPERATOR> Operator;
    
    typedef GenericToken<std::string, ERROR> ErrorToken;
    typedef GenericToken<std::u32string, STRING> StringToken;
    typedef GenericToken<std::string, IDENT> IdentifierToken;
    typedef GenericToken<std::string, IDENT> OperatorId;
    typedef GenericToken<char32_t, CHAR> CharToken;
    typedef GenericToken<std::string, META_ID> MetaToken;
    typedef GenericToken<unsigned int, META_ID> ReferredId; 
    typedef GenericToken<bool, BOOL> BoolToken;
    typedef GenericToken<std::string, COMMENT> CommentToken;

    
    typedef NumberToken<unsigned int, QUnsignedInt> UIntToken;
    typedef NumberToken<unsigned long, QUnsignedLong> ULongToken;
    typedef NumberToken<unsigned long long, QUnsignedLongLong> ULongLongToken;

    typedef NumberToken<float, QFloat> FlToken;
    typedef NumberToken<double, QDouble> DblToken;
    typedef NumberToken<long double, QLongDouble> LDblToken;

    typedef NumberToken<int, QInt> IntToken;
    typedef NumberToken<long, QLong> LongToken;
    typedef NumberToken<long long, QLongLong> LongLongToken;
}
