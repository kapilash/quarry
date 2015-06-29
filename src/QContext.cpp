/*
Copyright (c) Hemanth Kapila
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
The name of Hemanth Kapila may NOT be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "QInternal.h"
#include "Quarry.h"

namespace Quarry{
    extern void collectCKeywords(std::map<std::string,int> &kwmap);
    extern void collectCOperators(std::map<std::string,int> &kwmap);
    extern void collectJavaKeywords(std::map<std::string,int> &kwmap);
    extern void collectJavaOperators(std::map<std::string,int> &kwmap);
	
    QContext::QContext(enum PL p) {

	for (int i=0; i<33; i++) {
	    lexers.push_back(spaceLexer);
	}
	for (int i=33; i<128; i++) {
	    lexers.push_back(csOperatorLexer);
	}
	for(int i = 128; i<256;i++){
	    lexers.push_back(csIdLexer);
	}
	lexers['"'] = cstringLexer;
	lexers['\''] = charLexer;

	for(int i=48; i < 58; i++) {
	    lexers[i] = numberLexer;
	}
	lexers['('] = singleCharLexer<OPEN_BRACKET>;
	lexers[')'] = singleCharLexer<CLOSE_BRACKET>;

	lexers['{'] = singleCharLexer<OPEN_BRACE>;
	lexers['}'] = singleCharLexer<CLOSE_BRACE>;
       
	lexers['['] = singleCharLexer<SQUARE_OPEN>;
	lexers[']'] = singleCharLexer<SQUARE_CLOSE>;

	lexers['.'] = singleCharLexer<DOT>;
	lexers[':'] = singleCharLexer<COLON>;
	lexers[';'] = singleCharLexer<SEMI_COLON>;
	lexers['?'] = singleCharLexer<QUESTION_MARK>;
	lexers[','] = singleCharLexer<COMMA>;
	lexers['/'] = csComments;

	for(int i=64;i<91; i++){
	    lexers[i] = csIdLexer;
	}
	lexers['_'] = csIdLexer;
	for(int i = 97; i < 123; i++) {
	       lexers[i] = csIdLexer;
	}

	lexers['$'] = csIdLexer;
	if (p  == C ) {
	    collectCKeywords(keywords);
	    collectCOperators(operators);
	}
	else if (p == JAVA) {
	    collectJavaKeywords(keywords);
	    collectJavaOperators(operators);
	}
	else {
	}
    }

    QContext::~QContext(){

    }

    class QuarryHolder {
    public:
	QReader *reader;
	QContext context;

	QuarryHolder(PL lang, const char *fileName)
	  : reader{new QReader(fileName)},
	      context(lang)
	{
	}
	QuarryHolder(PL lang, const unsigned char *byteArray, unsigned long length, int l, int col)
	  :reader(new QReader(byteArray,length, l, col)),
	     context(lang)
	{
	}

      void moveToFile(const char *fileName){
	delete reader;
	reader = new QReader(fileName);
      }

      void moveToText(const unsigned char *byteArray, unsigned long length, int l, int col) {
	delete reader;
	reader = new QReader(byteArray, length, l, col);
      }
      
	~QuarryHolder() {
	  if (reader != nullptr) {
	    delete reader;
	  }
	}
    };

    void* fromFile(PL lang, const char *fileName) {
	QuarryHolder *holder = new QuarryHolder(lang, fileName);
	return holder;
    }

    void *fromStr(PL lang, const unsigned char *byteArray, unsigned long length, int l, int col)
    {
	QuarryHolder *holder = new Quarry::QuarryHolder(lang, byteArray, length, l, col);
	return holder;
    }

  void moveToFile(void *quarry, const char *fileName) {
    QuarryHolder *holder = reinterpret_cast<QuarryHolder *>(quarry);
    holder->moveToFile(fileName);
  }

  void moveToText(void *quarry,const unsigned char *byteArray, unsigned long length, int l, int col)
  {
    QuarryHolder *holder = reinterpret_cast<QuarryHolder *>(quarry);
    holder->moveToText(byteArray, length, l, col);
  }
  
    Token* nextToken(void *quarry) {
	QuarryHolder *holder = reinterpret_cast<QuarryHolder *>(quarry);
	if(holder->reader->hasMore()) {
	    unsigned char c = holder->reader->peekNext();
	    Lexer l = holder->context.lexer(c);
	    return l(*(holder->reader), holder->context);
	}
	return new Token(holder->reader->getLine(), holder->reader->getCol(), QEOF,0, nullptr);
	    
    }

    void closeQuarry(void *quarry) {
	QuarryHolder *holder = reinterpret_cast<QuarryHolder *>(quarry);
	delete holder;
    }
}
//extern "C" {
void *quarry_fromFile(int lang, const char *fileName)
{
    return Quarry::fromFile((Quarry::PL)lang, fileName);
}

void *quarry_fromStr(int lang, const unsigned char *byteArray, unsigned long length, int l, int col)
{
    return Quarry::fromStr((Quarry::PL)lang, byteArray, length, l, col);
}


void quarry_moveToFile(void *quarry, const char *fileName)
{
    Quarry::moveToFile(quarry, fileName);
}

void quarry_moveToStr(void *quarry, const unsigned char *byteArray, unsigned long length, int l, int col)
{
  Quarry::moveToText(quarry, byteArray, length, l, col);
}

void quarry_close(void *p) {
    Quarry::closeQuarry(p);
}

struct quarry_Token *quarry_nextToken(void *quarry)
{
    auto token = Quarry::nextToken(quarry);
    quarry_Token *t = new quarry_Token();
    t->line = token->line;
    t->column = token->column;
    t->tokenType = (int)(token->tokenType);
    t->textPtr = (unsigned char *)(token->textPtr);
    t->length = token->length;
    t->opaque =  token;
    return t;
}

void  quarry_freeToken(struct quarry_Token *t)
{
    Quarry::Token *inner = reinterpret_cast<Quarry::Token *>(t->opaque);
    delete inner;
    delete t;
}

unsigned int quarry_numberType(struct quarry_Token *token) {
    Quarry::Token *inner = reinterpret_cast<Quarry::Token *>(token->opaque);
    Quarry::NumericalToken *numeric = dynamic_cast<Quarry::NumericalToken *>(inner);
    if (numeric == nullptr) {
	return 0;
    }
    return (unsigned int)(numeric->numberType);
}

unsigned int quarry_toChar(struct quarry_Token *token) {
    Quarry::Token *inner = reinterpret_cast<Quarry::Token *>(token->opaque);
    Quarry::CharToken *charToken = dynamic_cast<Quarry::CharToken *>(inner);
    if (charToken == nullptr) {
	return 0;
    }
    return charToken->value;
}

int quarry_toInt(struct quarry_Token *token) {
    Quarry::Token *inner = reinterpret_cast<Quarry::Token *>(token->opaque);
    Quarry::IntToken *intToken = dynamic_cast<Quarry::IntToken *>(inner);
    if (intToken == nullptr) {
	return 0;
    }
    return intToken->value;
}

unsigned int quarry_toUInt(struct quarry_Token *token) {
    Quarry::Token *inner = reinterpret_cast<Quarry::Token *>(token->opaque);
    Quarry::UIntToken *intToken = dynamic_cast<Quarry::UIntToken *>(inner);
    if (intToken == nullptr) {
	return 0;
    }
    return intToken->value;
}

long long quarry_toLong(struct quarry_Token *token) {
    Quarry::Token *inner = reinterpret_cast<Quarry::Token *>(token->opaque);
    Quarry::LongToken *longToken = dynamic_cast<Quarry::LongToken *>(inner);
    if (longToken != nullptr) {
	return longToken->value;
    }

    Quarry::LongLongToken *longLongToken = dynamic_cast<Quarry::LongLongToken *>(inner);
    if (longLongToken != nullptr) {
	return longLongToken->value;
    }
    return 0L;
}

unsigned long long quarry_toULong(struct quarry_Token *token) {
    Quarry::Token *inner = reinterpret_cast<Quarry::Token *>(token->opaque);
    Quarry::ULongToken *ulongToken = dynamic_cast<Quarry::ULongToken *>(inner);
    if (ulongToken != nullptr) {
	return ulongToken->value;
    }

    Quarry::ULongLongToken *ullToken = dynamic_cast<Quarry::ULongLongToken *>(inner);
    if (ullToken != nullptr) {
	return ullToken->value;
    }
    return 0;
}

float quarry_toFloat(struct quarry_Token *token) {
    Quarry::Token *inner = reinterpret_cast<Quarry::Token *>(token->opaque);
    Quarry::FlToken *floatToken = dynamic_cast<Quarry::FlToken *>(inner);
    if (floatToken != nullptr) {
	return floatToken->value;
    }
    return 0;
}

double quarry_toCDouble(struct quarry_Token *token) {
    Quarry::Token *inner = reinterpret_cast<Quarry::Token *>(token->opaque);

    Quarry::DblToken *dblToken = dynamic_cast<Quarry::DblToken *>(inner);
    if (dblToken != nullptr) {
	return dblToken->value;
    }

    Quarry::LDblToken *lDblToken = dynamic_cast<Quarry::LDblToken *>(inner);
    if (lDblToken != nullptr) {
	return lDblToken->value;
    }
    return 0;
}


//}
