#pragma once

#include <map>
#include <string>
#include "QReader.h"

namespace Quarry {
    enum PL {
	C = 0,
	JAVA,
	R5RS_Scheme
    };
    class QContext;
    
    class BaseLexer {
    public:
	BaseLexer(){}
	virtual BaseLexer* scan(QReader &reader, QContext &context) = 0;
	virtual ~BaseLexer(){}
    };
    
    class QContext{
    public:
	QContext(enum PL i);

	inline int keywordIndex(std::string &str) const {
	    auto it = keywords.find(str);
	    if (it == keywords.end()) {
		return -1;
	    }
	    return it->second;
	}
	
	int operatorIndex(std::string &str) const {
	    auto it = keywords.find(str);
	    if (it == keywords.end()) {
		return -1;
	    }
	    return it->second;
	}
	
	inline BaseLexer* lexer(unsigned char c) const {
	    return lexers[c];
	}

	~QContext();
    private:
	std::map<std::string, int> keywords;
	std::map<std::string, int> operators;
	BaseLexer* lexers[256];
    };
}
