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

#include <boost/iostreams/device/mapped_file.hpp>
#include <iostream>
#include "quarry_export.h"

namespace Quarry {
    class QReader{
    private:
	QUARRY_EXPORT void read();
    public:
	QUARRY_EXPORT QReader(const char *fileName);
	QUARRY_EXPORT QReader(const unsigned char *byteArray, std::size_t length, int l, int col);

	inline bool hasMore()  {
	  return (position < length);
	}
            
	inline unsigned char peekNext() const{
	    return bytes[position];
	}
            
	inline unsigned char next() {
	    unsigned char c = bytes[position];
	    position++;
	  if (c == '\n') {
	      ++line;
	      column = 1;
	  }
	  return c;
	}

	char32_t nextChar() ;

	inline const unsigned char* current() {
	  return &(bytes[position]);
	}

	inline std::size_t currPosition() const {
	    return position;
	}
	
	inline int getCol() { return column;}
            
	inline int getLine() { return line;}

	inline void setColumn(int c) { column = c; }

	inline void addColumns(int c) { column += c; }

	QUARRY_EXPORT void appendWhile(bool (*predicate)(unsigned char), std::string&);
	QUARRY_EXPORT int till(bool (*predicate)(unsigned char));
	QUARRY_EXPORT int asLongAs(bool (*predicate)(unsigned char));

	QUARRY_EXPORT ~QReader();
    private:
	boost::iostreams::mapped_file_source file;
	const unsigned char *bytes;
	std::size_t length;
	std::size_t position;
	int line;
	int column;
    };
}
