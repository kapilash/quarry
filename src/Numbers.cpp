/*
Copyright (c)  Hemanth Kapila
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1) Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2) Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
The name of the Hemanth Kapila may NOT be used to endorse or promote products derived from this software without specific prior written permission.
3) THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
#include "QInternal.h"
#include <boost/lexical_cast.hpp>
#include <iostream>
#include <sstream>

namespace Quarry {

    template<typename numeric>
    static numeric toNumber(unsigned char c) {
	numeric v = 0;
	if( c >= '0' && c <= '9') {
	    v = (numeric)(c - '0');
	}
	else if (c >= 'A' && c <= 'F') {
	    v = (numeric)(10 + (c - 'A'));
	}
	else{
	    v = (numeric)(10 + (c - 'a'));
	}
	//std::cout << c << " = " << v << " ";
	return v;
    }
    
    template<typename T, typename numeric>
    static Token * tokenOrError(QReader &reader, std::string &text) {
	reader.addColumns(text.length());
	std::istringstream iss(text);
	numeric v;
	if (iss >> v){
	    return new T(reader.getLine(), reader.getCol(), text,v);
	}
	return new ErrorToken(reader.getLine(), reader.getCol(), "Invalid Numerical Constant");
    }

    
    template<typename T, typename numeric>
    static Token * tokenOrError(QReader &reader, std::string &text, int base, int pos) {
	reader.addColumns(text.length());
	std::istringstream iss(text);
	numeric v = 0;
	int len = text.length();
	for (int p = pos; p < text.length(); p++){
	    if((text[p] == 'u') || (text[p] == 'U') || (text[p] == 'l') || (text[p] == 'L'))
		break;
	    v = (v * base) + toNumber<numeric>(text[p]);
	}
	return new T(reader.getLine(), reader.getCol(), text,v);
    }

    static Token *appendMantissa(std::string &text, QReader &reader)
    {
	while( reader.hasMore() && ((reader.peekNext() >= '0') && (reader.peekNext() <= '9'))) {
	    text.push_back(reader.next());
	}

	if( reader.hasMore() && ((reader.peekNext() == 'E') || (reader.peekNext() == 'e'))) {
	    reader.next();
	    text.push_back('E');
	    if(reader.hasMore() &&( ( reader.peekNext() == '+') || (reader.peekNext() == '-')) ) {
		text.push_back(reader.next());
	    }
	    while( reader.hasMore() && ((reader.peekNext() >= '0') && (reader.peekNext() <= '9'))) {
		text.push_back(reader.next());
	    }
	}
	
	if(reader.hasMore() && ((reader.peekNext() == 'f') || (reader.peekNext() == 'F'))) {
	    text.push_back(reader.next());
	    return tokenOrError<FlToken, float>(reader, text);
	}
	if(reader.hasMore() && ((reader.peekNext() == 'l') || (reader.peekNext() == 'L'))) {
	    text.push_back(reader.next());
	    return tokenOrError<LDblToken, long double>(reader, text);
	}
	
	return tokenOrError<DblToken, double>(reader, text);
    }
    
    static Token *tryOptionalU(std::string &text, QReader &reader, int base = 10, int pos = 0) {
	if(reader.hasMore() && ((reader.peekNext() == 'u') || (reader.peekNext() == 'U'))) {
	    reader.next();
	    text.push_back('U');

	    if(reader.hasMore() && ((reader.peekNext() == 'l') || (reader.peekNext() == 'L'))) {
		text.push_back('L');
		reader.next();
		
		if (reader.hasMore() && ((reader.peekNext() == 'l') || (reader.peekNext() == 'L'))) {
		    text.push_back('L');
		    reader.next();
		    return tokenOrError<ULongLongToken, unsigned long long>(reader, text, base, pos);
		}
		
		return tokenOrError<ULongToken, unsigned long>(reader, text, base, pos);
	    }
	    
	    return tokenOrError<UIntToken, unsigned int>(reader, text, base, pos);
	}
	
	return nullptr;
    }

    static Token *tryOptionalLL(std::string &text, QReader &reader, int base = 10, int pos = 0) {
	bool isUnsigned = false;
	bool isLongLong = false;
	bool isLong  = false;

	if(reader.hasMore() && ((reader.peekNext() == 'l') || (reader.peekNext() == 'L'))) {
	    text.push_back('L');
	    reader.next();
	    isLong = true;
	    if (reader.hasMore() && ((reader.peekNext() == 'l') || (reader.peekNext() == 'L'))) {
		text.push_back('L');
		reader.next();
		isLongLong = true;
	    }
	    if(reader.hasMore() && ((reader.peekNext() == 'u') || (reader.peekNext() == 'U'))) {
		reader.next();
		text.push_back('U');
		isUnsigned = true;
	    }
	}

	if (isLong) {
	    if (isLongLong) {
		if (isUnsigned)
		    return tokenOrError<ULongLongToken, unsigned long long>(reader, text, base, pos);
		else
		    return tokenOrError<LongLongToken, long long>(reader, text, base, pos);
	    }
	    if (isUnsigned)	    
		return tokenOrError<ULongToken, unsigned long>(reader, text, base, pos);
	    
	    return tokenOrError<LongToken, long>(reader, text,base, pos);
	}
	
	return tryOptionalU(text, reader, base, pos);
    }
    
    Token* numberLexer(QReader &reader, QContext &context)
    {
	auto c = reader.next();
	std::string text;
	text.push_back(c);
    if (c == '-' && (!reader.hasMore() || (reader.peekNext() < '0') || (reader.peekNext() > '9'))) {
            return new Operator(reader.getLine(), reader.getCol(), context.operatorIndex(text));
    }
	if (c != '0') {
	    while(reader.hasMore() && ((reader.peekNext() >= '0') && (reader.peekNext() <= '9'))) {
		text.push_back(reader.next());
	    }
	    auto ullPtr = tryOptionalLL(text,reader);
	    if (ullPtr != nullptr) {
		return ullPtr;
	    }
	    else if(reader.hasMore() && ((reader.peekNext() == 'f') || (reader.peekNext() == 'f'))) {
		reader.next();
		return tokenOrError<FlToken,float>(reader, text);
	    }else if(reader.hasMore() && ((reader.peekNext() == 'd') || (reader.peekNext() == 'D'))) {
		reader.next();
		return tokenOrError<DblToken, double>(reader, text); 
	    }else if(reader.hasMore() && (reader.peekNext() == '.')) {
		text.push_back(reader.next());
		return appendMantissa(text, reader);
	    }
	    
	    return tokenOrError<IntToken,int>(reader, text);
	}
	if (reader.hasMore() && (reader.peekNext() == '.')){
	    text.push_back(reader.next());
	    return appendMantissa(text, reader);
	}
	int base = 10;
	int pos  = 0;
	if (reader.hasMore() && ((reader.peekNext() == 'x') || (reader.peekNext() == 'X'))) {
	    text.push_back(reader.next());
	    base = 16;
	    pos = 2; 
	    while(reader.hasMore() && 
		  (
		   ((reader.peekNext() >= '0') && (reader.peekNext() <= '9')) ||
		   ((reader.peekNext() >= 'A') && (reader.peekNext() <= 'F')) ||
		   ((reader.peekNext() >= 'a') && (reader.peekNext() <= 'f')) )) {
		text.push_back(reader.next());		    
	    }
	}
	else if(reader.hasMore() && ((reader.peekNext() == 'b') || (reader.peekNext() == 'B'))) {
	    text.push_back(reader.next());
	    base = 2;
	    pos = 2;
	    while(reader.hasMore() && ((reader.peekNext() == '0') || (reader.peekNext() == '1'))) {
		text.push_back(reader.next());
	    }
	}
	else{
	    base = 8;
	    pos = 1;
	    while(reader.hasMore() && ((reader.peekNext() >= '0') && (reader.peekNext() <= '7'))) {
		text.push_back(reader.next());
	    }
	}

	auto ullPtr = tryOptionalLL(text, reader, base, pos);
	if (ullPtr != nullptr) {
	    return ullPtr;
	}

	return tokenOrError<IntToken,int>(reader, text, base, pos);
    }
}
