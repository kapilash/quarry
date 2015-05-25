#pragma once

#include <vector>
#include <cstdio>
#include <stack>
#include "quarry_export.h"

namespace Quarry {
    QUARRY_EXPORT class QReader{
    private:
	QUARRY_EXPORT void read();
    public:
	QUARRY_EXPORT QReader(const char *fileName);
	QUARRY_EXPORT QReader(const unsigned char *byteArray, size_t length, int l, int col);



	inline bool hasMore()  {
	    if (bytes.empty()) {
		read();
	    }
	    return !(bytes.empty());
	}
            
	inline unsigned char peekNext() const{
	    return bytes.top();
	}
            
	inline unsigned char next() {
	    unsigned char n = bytes.top();
	    bytes.pop();
	    ++column;
	    if (n == '\n') {
		++line;
		column = 0;
	    }
	    return n;
	}
            
	inline int getCol() { return column;}
            
	inline int getLine() { return line;}

	QUARRY_EXPORT ~QReader();
    private:
	bool isInMemory;
	std::FILE *fp;

	std::stack<unsigned char> bytes;
	
	int line;
	int column;
    };
}
