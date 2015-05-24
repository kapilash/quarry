#pragma once

#include <vector>
#include <cstdio>
#include <stack>

namespace Quarry {
    class QReader{
    private:
	void read();
    public:
	QReader(const char *fileName);
	QReader(const unsigned char *byteArray, size_t length, int l, int col);



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

	~QReader();
    private:
	bool isInMemory;
	std::FILE *fp;

	std::stack<unsigned char> bytes;
	
	int line;
	int column;
    };
}
