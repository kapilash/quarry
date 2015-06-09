#pragma once

#include <map>
#include <string>
#include <set>
#include <vector>
#include "QReader.h"
#include "quarry.h"
#include "QToken.h"

namespace Quarry {
    enum PL {
	C = 0,
	JAVA,
	R5RS_Scheme
    };

    class QContext ;

    typedef Token * (*Lexer)(QReader &reader, QContext &context);
    
    class QContext{
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
	
	QUARRY_EXPORT inline Lexer lexer(unsigned char c) const {
	    return lexers[c];
	}

	QUARRY_EXPORT ~QContext();
    private:
	std::map<std::string, int> keywords;
	std::map<std::string, int> operators;
	std::vector<Lexer> lexers;
    };

    template<TokenType t>
	Token* singleCharLexer(QReader &reader, QContext &context) {
	int col = reader.getCol();
	int line = reader.getLine();
	reader.next();
	return new Token(line, col, t);
    }

    QUARRY_EXPORT Token* spaceLexer(QReader &reader, QContext &context);

    QUARRY_EXPORT Token* charLexer(QReader &reader, QContext &context);

    QUARRY_EXPORT Token* cstringLexer(QReader &reader, QContext &context);

    QUARRY_EXPORT Token* numberLexer(QReader &reader, QContext &context);

    QUARRY_EXPORT Token* csComments(QReader &reader, QContext &context);

    QUARRY_EXPORT Token* csIdLexer(QReader &reader, QContext &context);

    QUARRY_EXPORT Token *csOperatorLexer(QReader &reader, QContext &context);
}
