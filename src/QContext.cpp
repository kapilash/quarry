#include "QInternal.h"

namespace Quarry{
    extern void collectCKeywords(std::map<std::string,int> &kwmap);
    extern void collectCOperators(std::map<std::string,int> &kwmap);
    extern void collectJavaKeywords(std::map<std::string,int> &kwmap);
    extern void collectJavaOperators(std::map<std::string,int> &kwmap);
	
    QContext::QContext(enum PL p) {
	for (int i=0; i<256; i++) {
	    lexers.push_back(nullptr);
	}
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
}
