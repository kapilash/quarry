#pragma once

#include <map>
#include <string>
#include <set>
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

    
    QUARRY_EXPORT class BaseLexer {
    public:
	BaseLexer(){}
	virtual quarry_SlabPtr scan(QReader &reader, QContext &context) const = 0;
	virtual ~BaseLexer(){}
    };
    
    QUARRY_EXPORT class QContext{
    public:
	friend void addPunctuationLexers (QContext &context);
	friend void addGroupLexers (QContext &context) ;
	friend void addWhitespaceLexers (QContext  &context);
	QUARRY_EXPORT QContext(enum PL i);

	inline int keywordIndex(std::string &str) const {
	    auto it = keywords.find(str);
	    if (it == keywords.end()) {
		return -1;
	    }
	    return it->second;
	}
	
	inline int operatorIndex(std::string &str) const {
	    auto it = keywords.find(str);
	    if (it == keywords.end()) {
		return -1;
	    }
	    return it->second;
	}
	
	QUARRY_EXPORT inline BaseLexer* lexer(unsigned char c) const {
	    return lexers[c];
	}

	QUARRY_EXPORT ~QContext();
    private:
	std::map<std::string, int> keywords;
	std::map<std::string, int> operators;
	BaseLexer* lexers[256];
    };

    class DelimitedLexer : public BaseLexer {
	const char delimiter;
	const char escape;
	const quarry_SlabType tokenType;
    public:
        QUARRY_EXPORT DelimitedLexer(char delim, char esc, quarry_SlabType t) :delimiter(delim), escape(esc), tokenType(t) {} 
	QUARRY_EXPORT quarry_SlabPtr scan(QReader &reader, QContext &context) const;
    };

    class CIdentifier : public BaseLexer {
    public:
	QUARRY_EXPORT quarry_SlabPtr scan(QReader &reader, QContext &context) const;
    };

    class DoubleCharComment : public BaseLexer {
    private:
	const char begin;
	const char second;
	const char last;
    public:
	QUARRY_EXPORT DoubleCharComment(char b, char s, char l):begin(b), second(s), last(l) {}
	
	QUARRY_EXPORT quarry_SlabPtr scan(QReader &reader, QContext &context) const;
    };

    class CLikeComment : public BaseLexer {
    public:
	QUARRY_EXPORT quarry_SlabPtr scan(QReader &reader, QContext &context) const;
    };

    class SingleCharLexer : public BaseLexer {
    private:
	const char c;
	const enum quarry_SlabType slabType;
	
    public:
	QUARRY_EXPORT SingleCharLexer(char given, enum quarry_SlabType givenSlabType) : c(given), slabType(givenSlabType) {}
	QUARRY_EXPORT quarry_SlabPtr scan(QReader &reader, QContext &context) const ;
    };

    class LFLexer : public SingleCharLexer {
    public :
	QUARRY_EXPORT LFLexer(char g = '\n', enum quarry_SlabType gst = quarry_NewLine) : SingleCharLexer (g, gst) {}
	
	QUARRY_EXPORT quarry_SlabPtr scan(QReader &reader, QContext &context) const ;
    };

    class CRLexer : public BaseLexer {
    public:
	QUARRY_EXPORT quarry_SlabPtr scan(QReader &reader, QContext &context) const ;
    };

    class NumberLexer : public BaseLexer {
	
    public:
	QUARRY_EXPORT quarry_SlabPtr scan(QReader &reader, QContext &context) const;
    };

    class OperatorLexer : public BaseLexer {
	const std::string allowed;
    public:
	QUARRY_EXPORT OperatorLexer(std::string operChars) : allowed(operChars) {}
	QUARRY_EXPORT quarry_SlabPtr scan(QReader &reader, QContext &context) const;
    };
    QUARRY_EXPORT BaseLexer* getDblCharCommentLexer(char b, char s, char e);
}
