#pragma once

#include <vector>
#include <cstdio>

namespace Quarry {
    class QReader{
    public:
	QReader(const char *fileName);
	QReader(const unsigned char *byteArray, int length, int l, int col);

	bool read();

	inline bool hasMore() const {
	    return !(pos >= length);
	}
            
	inline unsigned char peekNext() const{
	    return bytes[pos];
	}
            
	inline unsigned char next() {
	    unsigned char n = bytes[pos];++pos; ++column; return n;
	}

	inline void incrementLine(){
	    ++line;
	    column = 1;
	}
            
	inline int getCol() { return column;}
            
	inline int getLine() { return line;}

	~QReader();
    private:
	bool isInMemory;
	std::FILE *fp;
	unsigned char *bytes;
	size_t length;
	size_t pos;
	int line;
	int column;
    };
}
