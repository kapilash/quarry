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
	QReader reader;
	QContext context;

	QuarryHolder(int lang, const char *fileName)
	    : reader{fileName},
	      context((PL)lang)
	{
	}
	QuarryHolder(int lang, const unsigned char *byteArray, unsigned long length, int l, int col)
	    :reader(byteArray,length, l, col),
	     context((PL)lang)
	{
	}
	~QuarryHolder() {}
    };
}
//extern "C" {
void *quarry_fromFile(int lang, const char *fileName)
{
    Quarry::QuarryHolder *holder = new Quarry::QuarryHolder(lang, fileName);
    return holder;
}

void *quarry_fromStr(int lang, const unsigned char *byteArray, unsigned long length, int l, int col)
{
    Quarry::QuarryHolder *holder = new Quarry::QuarryHolder(lang, byteArray, length, l, col);
    return holder;
}

void quarry_close(void *p) {
    Quarry::QuarryHolder *holder = reinterpret_cast<Quarry::QuarryHolder *>(p);
    delete holder;
}

struct quarry_Token *quarry_nextToken(void *quarry)
{
    Quarry::QuarryHolder *holder = reinterpret_cast<Quarry::QuarryHolder *>(quarry);
    if(holder->reader.hasMore()) {
	unsigned char c = holder->reader.peekNext();
	Quarry::Lexer l = holder->context.lexer(c);
	auto token = l(holder->reader, holder->context);
	quarry_Token *t = new quarry_Token();
	t->line = token->line;
	t->column = token->column;
	t->tokenType = (int)(token->tokenType);
	t->textPtr = (unsigned char *)(token->textPtr);
	t->length = token->length;
	delete token;
	return t;
    }
    quarry_Token *eofToken = new quarry_Token();
    eofToken->line = holder->reader.getLine();
    eofToken->column = holder->reader.getCol();
    eofToken->tokenType = (int)Quarry::QEOF;
    eofToken->length = 0;
    eofToken->textPtr = nullptr;
    return eofToken;
}

void  quarry_freeToken(struct quarry_Token *t)
{
    delete t;
}
//}
