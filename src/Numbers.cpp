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
namespace Quarry {

    static Token *appendMantissa(std::string &text, QReader &reader)
    {
	bool isFloat = true;
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
	    if(reader.hasMore() && ((reader.peekNext() == 'f') || (reader.peekNext() == 'F'))) {
		text.push_back(reader.next());
	    }
	    else if(reader.hasMore() && ((reader.peekNext() == 'l') || (reader.peekNext() == 'L'))) {
		text.push_back(reader.next());
		isFloat = false;
	    }else {
	    }
	}
	// convert string to double. pass isFloat.
	return nullptr; // float
    }
    
    static Token *tryOptionalU(std::string &text, QReader &reader) {
	bool isUnsigned = false;
	bool isLongLong = false;
	bool isLong  = false;
	    
	if(reader.hasMore() && ((reader.peekNext() == 'u') || (reader.peekNext() == 'U'))) {
	    reader.next();
	    text.push_back('U');
	    isUnsigned = true;
	    if(reader.hasMore() && ((reader.peekNext() == 'l') || (reader.peekNext() == 'L'))) {
		text.push_back('L');
		reader.next();
		isLong = true;
		
		if (reader.hasMore() && ((reader.peekNext() == 'l') || (reader.peekNext() == 'L'))) {
		    text.push_back('L');
		    reader.next();
		    isLongLong = true;
		}
	    }
	}
	if (isLongLong || isLong) {
	    // return 64 bit 
	    return nullPtr;
	}

	return nullptr;
    }

    static Token *tryOptionalLL(std::string &text, QReader &reader) {
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
	if (isUnsigned || isLongLong || isLong) {
	    if (isLong || isLongLong) {
		// return 64Bit.
	    }
	    // return unsigned int
	    return nullPtr;
	}
	return tryOptionalU(text, reader);
    }
    
    Token* numberLexer(QReader &reader, QContext &context)
    {
       	auto col = reader.getCol();
	auto line = reader.getLine();
	auto c = reader.next();
	std::string text;
	text.push_back(c);
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
		// return float
	    }else if(reader.hasMore() && ((reader.peekNext() == 'd') || (reader.peekNext() == 'D'))) {
		reader.next();
		// return double
	    }else if(reader.hasMore() && (reader.peekNext() == '.')) {
		text.push_back(reader.next());
		return appendMantissa(text, reader);
	    }
	    // return signed int
	}
	if (reader.hasMore() && (reader.peekNext() == '.')){
	    text.push_back(reader.next());
	    return appendMantissa(text, reader);
	}
	if (reader.hasMore() && ((reader.peekNext() == 'x') || (reader.peekNext() == 'X'))) {
	    text.push_back(reader.next());
	    // hexadecimal
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
	    while(reader.hasMore() && ((reader.peekNext() == '0') || (reader.peekNext() == '1'))) {
		text.push_back(reader.next());
	    }
	}
	else{
	    while(reader.hasMore() && ((reader.peekNext() >= '0') && (reader.peekNext() <= '9'))) {
		text.push_back(reader.next());
	    }
	}

	auto ullPtr = tryOptionalLL(text, reader);
	if (ullPtr != nullptr) {
	    return ullPtr;
	}
	return nullptr; //return integer
    }
}
