#pragma once

#include <boost/iostreams/device/mapped_file.hpp>
#include <iostream>
#include "quarry_export.h"

namespace Quarry {
    QUARRY_EXPORT class QReader{
    private:
	QUARRY_EXPORT void read();
    public:
	QUARRY_EXPORT QReader(const char *fileName);
	QUARRY_EXPORT QReader(const unsigned char *byteArray, size_t length, int l, int col);



	inline bool hasMore()  {
	  return (position < length);
	}
            
	inline unsigned char peekNext() const{
	    return bytes[position];
	}
            
	inline unsigned char next() {
	    unsigned char c = bytes[position];
	    position++;
	    column++;
	  if (c == '\n') {
	      ++line;
	      column = 0;
	  }
	  return c;
	}
            
	inline int getCol() { return column;}
            
	inline int getLine() { return line;}

	QUARRY_EXPORT ~QReader();
    private:
	boost::iostreams::mapped_file_source file;
	bool isInMemory;
	const unsigned char *bytes;
	std::size_t length;
	std::size_t position;
	int line;
	int column;
    };
}
