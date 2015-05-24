#pragma once

#include <map>
#include <string>
#include "QReader.h"
#include "quarry.h"

namespace Quarry {
    enum PL {
	C = 0,
	JAVA,
	R5RS_Scheme
    };

    class QContext;
    class BaseLexer;

    class QResult {
	
    public:
	BaseLexer * const lexer;
	struct quarry_Slab_ *const slab;
    QResult(BaseLexer *l) : lexer(l),slab(nullptr) {}
    QResult(struct quarry_Slab_ *s) :slab(s), lexer(nullptr) {}
    };

    
    class BaseLexer {
    public:
	BaseLexer(){}
	virtual quarry_SlabPtr scan(QReader &reader, QContext &context) const = 0;
	virtual ~BaseLexer(){}
    };
    
    class QContext{
    public:
	friend void addPunctuationLexers (QContext &context);
	friend void addGroupLexers (QContext &context) ;
	friend void addWhitespaceLexers (QContext  &context);
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

    BaseLexer* getDblCharCommentLexer(char b, char s, char e);
}
