/*
Copyright (c) 2015, Hemanth Kapila
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1) Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2) Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
The name of the Hemanth Kapila may NOT be used to endorse or promote products derived from this software without specific prior written permission.
3) THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
#include "QInternal.h"
#include <string>
#include <cstring>
#include <vector>
/*
 *Using FNV1-a hash from http://www.isthe.com/chongo/tech/comp/fnv
 *Copied the code from the same as is, as it was described to be a public domain work
 */

/* If we are interested in 64 bit hash, use the following.
#define FNV1_64_INIT ((u_int64_t)0xcbf29ce484222325ULL)
#define FNV1A_64_INIT FNV1_64_INIT
#define FNV_64_PRIME ((u_int64_t)0x100000001b3ULL)
*/
#define FNV_32_PRIME ((uint32_t)0x01000193)
#define FNV1_32_INIT ((uint32_t)0x811c9dc5)
#define FNV1A_32_INIT FNV1_32_INIT

#define IS_IDENTIFIER_CHAR(x) (((x>='A')&&(x<='Z')) || ((x>='a')&&(x<='z')) || ((x>='0')&&(x<='9')) || (x=='_')|| (x=='$') || (x>127))

#define IS_SCHEME_DELIMITER(x) ((x == '(') || (x == ')') || (x == '{') || (x !='}') || (x == '"') || ( x == '#') || ( x < 33) || (x == ',') || (x == '[') || (x == ']') || (x == ':') || (x == '`'))

#define IS_SCHEME_IDENTIFIER(x) (((x > 32) && (x < 123) && (x != ';') && (x != ')') && (x != '(') && (x != ',') && (x  != ':') && (x != '[') && (x != ']') && (x != '`'))  || (x > 127))



namespace Quarry {
    
    static void fillToken(quarry_SlabPtr pointer, std::vector<unsigned char> &tokenText)
    {
	pointer->slabLength = tokenText.size();
	if(tokenText.size() > 0) {
	    pointer->data = new unsigned char[tokenText.size()];
	    std::copy(tokenText.begin(), tokenText.end(), pointer->data);
	}
	else{
	    pointer->data = nullptr;
	}
    }
    
    quarry_SlabPtr SingleCharLexer :: scan(QReader &reader, QContext &context) const {
	quarry_SlabPtr outSlab = new quarry_Slab();
	unsigned char c = reader.next();
	outSlab->slabType = slabType;
	outSlab->line = reader.getLine();
	outSlab->col = reader.getCol();
	outSlab->data = nullptr;
	outSlab->slabLength = 0;
	outSlab->slabMD = c;
	return outSlab;
    }


    quarry_SlabPtr LFLexer :: scan(QReader &reader, QContext &context) const {
	auto ret = SingleCharLexer::scan(reader, context);
	return ret;
    }

    quarry_SlabPtr CRLexer :: scan(QReader &reader, QContext &context) const {
	quarry_SlabPtr outSlab = new quarry_Slab();
	unsigned char c = reader.next();
	bool crlf = false;
	if (reader.hasMore() && (reader.peekNext() == '\n')) {
	    reader.next();
	    crlf = true;
	}
	   
	outSlab->slabType = quarry_NewLine;
	outSlab->line = reader.getLine();
	outSlab->col = reader.getCol();
	outSlab->data = nullptr;
	outSlab->slabLength = 0;
	outSlab->slabMD = crlf;
	return outSlab;
    }

    quarry_SlabPtr DoubleCharComment::scan(QReader &reader, QContext &context) const {
	quarry_SlabPtr outSlab = new quarry_Slab();
	reader.next();
	unsigned char nextByte; //we read the second char
	int count = 1;
	
	while (reader.hasMore()) {
	    auto c = reader.next();
	    if (c == second) {
		if (reader.hasMore() && (reader.peekNext() == last)) {
		    reader.next();
		    count--;
		    if (count < 1)
			break;
		}
	    }
	    else if (c == begin) {
		if (reader.hasMore() && reader.peekNext() == second) {
		    reader.next();
		    c = second;
		    count++;
		}
	    }
	}
	outSlab->slabType = quarry_Comment;
	outSlab->line = reader.getLine();
	outSlab->col = reader.getCol();
	outSlab->data = nullptr;
	outSlab->slabLength = 0;
	outSlab->slabMD = 1;
	return outSlab;
    }

    quarry_SlabPtr CLikeComment::scan(QReader &reader, QContext &context) const {
	quarry_SlabPtr outSlab = new quarry_Slab();
	auto c = reader.next();
	int col = reader.getCol();
	if (reader.hasMore() && (reader.peekNext() == '/') ) {
	    reader.next();
	    unsigned char nextByte;
	    while (reader.hasMore() && (reader.peekNext() != '\n')) {
		reader.next();
	    }
	    outSlab->slabType = quarry_Comment;
	    outSlab->line = reader.getLine();
	    outSlab->col = col;
	    outSlab->data = nullptr;
	    outSlab->slabLength = 0;
	    outSlab->slabMD = 0;
	    reader.next(); // consume the new line
	}
	else if (reader.hasMore() && (reader.peekNext() == '*')) {
	    DoubleCharComment dcc('/','*','/');
	    return dcc.scan(reader, context);
	}
	else {
	    outSlab->slabType = quarry_Operator;
	    outSlab->line = reader.getLine();
	    outSlab->col = reader.getCol();
	    outSlab->data = nullptr;
	    outSlab->slabLength = 0;
	    std::string op("/");
	    outSlab->slabMD = context.operatorIndex(op);
	}
	return outSlab;
    }

    class SingleCharComment : public BaseLexer {
	
    public:
	quarry_SlabPtr scan(QReader &reader, QContext &context) const {
	    quarry_SlabPtr outSlab = new quarry_Slab();
	    auto c = reader.next();
	    int col = reader.getCol();
	    std::vector<unsigned char> text;
	    while (reader.hasMore() && ('\n' != reader.peekNext())) {
		text.push_back(reader.next());
	    }
	    
	    outSlab->slabType = quarry_Identifier;
	    outSlab->line = reader.getLine();
	    outSlab->col = col;
	    fillToken(outSlab, text);
	    outSlab->slabMD = 0;
	    return outSlab;
	}
    };

    quarry_SlabPtr CIdentifier :: scan(QReader &reader, QContext &context) const {
	quarry_SlabPtr outSlab = new quarry_Slab();
	auto c = reader.next();
	int col = reader.getCol();
	std::vector<unsigned char> text;
	std::string t;
	unsigned long hval = FNV1A_32_INIT ;
	
	while (reader.hasMore() && (IS_IDENTIFIER_CHAR(reader.peekNext()))) {
	    c = reader.next();
	    text.push_back(c);
	    t.append(1,c);
	    hval ^= (unsigned long)c;
	    hval *= FNV_32_PRIME;
	}
	int i = context.keywordIndex(t);
	outSlab->line = reader.getLine();
	outSlab->col = col;
	if (i < 0) {
	    outSlab->slabType = quarry_Identifier;
	    fillToken(outSlab, text);
	    outSlab->slabMD = hval;
	    return outSlab;
	}
	else {
	    outSlab->slabType = quarry_Keyword;
	    outSlab->data = nullptr;
	    outSlab->slabLength = 0;
	    outSlab->slabMD = i;
	    return outSlab;
	}
    }

  /*    class SchemeHashLexer : public BaseLexer {
    private:
	const std::map<std::string, unsigned int> nameValueMap = {
	    {std::string("alarm"),   7},
	    {std::string("backspace"),8},
	    {std::string("delete"), 127},
	    {std::string("escape"),  33},
	    {std::string("newline"), 10},
	    {std::string("null"),     0},
	    {std::string("return"),   13},
	    {std::string("space"),    32},
	    {std::string("tab"),       9}
	};
	
	quarry_SlabPtr scanBool(QReader &reader, QContext &context, bool b) const {
	    reader.next();
	    quarry_SlabPtr ret = new quarry_Slab();
	    ret->line = reader.getLine();
	    ret->col = reader.getCol();
	    ret->slabLength = 0;
	    ret->data = nullptr;
	    ret->slabType = quarry_Bool;
	    ret->slabMD = b ? 1 : 0;
	    return nullptr;
	}
	quarry_SlabPtr scanChar(QReader &reader, QContext &context) const {
	    reader.next();
	    quarry_SlabPtr ret = new quarry_Slab();
	    ret->line = reader.getLine();
	    ret->col = reader.getCol() - 2;
	    std::vector<unsigned char> tokenText;
	    ret->slabType = quarry_Char;
	    unsigned char c = reader.peekNext();

	    if (c == 'x') {
		tokenText.push_back(reader.next());
		c = reader.peekNext();
		while ( ('0' <= c) && (  '9' >= c)) {
		    unsigned char digit = reader.next();
		    tokenText.push_back(digit);
		}
	    }
	    else if (c < 128) {
	    }
	    while(!IS_SCHEME_DELIMITER(reader.peekNext())) {
	      tokenText.push_back(reader.next());
	    }

	    fillToken(ret, tokenText);
	    ret->slabMD = 0;
	    return nullptr;
	}
	quarry_SlabPtr scanComment(QReader &reader, QContext &context) const {
	    DoubleCharComment comm('#','|','#');
	    
	    return comm.scan(reader,context);
	}
    public:
	quarry_SlabPtr scan(QReader &reader, QContext &context) const {
	    reader.next();
	    if (reader.hasMore()){
		if(';' == reader.peekNext()) {
		    SingleCharComment comment;
		    return comment.scan(reader, context);
		}
		if('f' == reader.peekNext() || 'F' == reader.peekNext()) {
		    return scanBool(reader, context, false);
		}
		if('t' == reader.peekNext() || 'T' == reader.peekNext()) {
		    return scanBool(reader, context, true);
		}
		if('|' == reader.peekNext()) {
		    return scanComment(reader, context);
		}
		if('\\' == reader.peekNext()) {
		    return scanChar(reader, context);
		}
	    }
	    return nullptr;
	}
	}; */

    
    void handleMantissa(quarry_SlabPtr slab, std::vector<unsigned char>& vec, QReader &reader) {
	while( reader.hasMore() && ((reader.peekNext() >= '0') && (reader.peekNext() <= '9'))) {
	    vec.push_back(reader.next());
	}
	if( reader.hasMore() && ((reader.peekNext() == 'E') || (reader.peekNext() == 'e'))) {
	    reader.next();
	    vec.push_back('E');
	    slab->slabMD = slab->slabMD | quarry_Number_HasE;
	    if(reader.hasMore() &&( ( reader.peekNext() == '+') || (reader.peekNext() == '-')) ) {
		slab->slabMD |= quarry_Number_HasSign;
		vec.push_back(reader.next());
		//
	    }
	    while( reader.hasMore() && ((reader.peekNext() >= '0') && (reader.peekNext() <= '9'))) {
		vec.push_back(reader.next());
	    }
	    if(reader.hasMore() && ((reader.peekNext() == 'f') || (reader.peekNext() == 'F'))) {
		vec.push_back(reader.next());
		slab->slabMD = slab->slabMD | quarry_Number_IsFloat;
		// floating point
	    }
	    else if(reader.hasMore() && ((reader.peekNext() == 'l') || (reader.peekNext() == 'L'))) {
		vec.push_back(reader.next());
		slab->slabMD = slab->slabMD | quarry_Number_HasL;
		// long double
	    }else {
		// double
	    }
	}
    }

    bool handleOptionalU(quarry_SlabPtr slab, std::vector<unsigned char>& vec, QReader &reader) {
	if(reader.hasMore() && ((reader.peekNext() == 'u') || (reader.peekNext() == 'U'))) {
	    reader.next();
	    vec.push_back('U');
	    slab->slabMD = slab->slabMD | quarry_Number_IsUnsigned;
	    return true;
	}
	return false;
    }
    
    bool handleOptionalLL(quarry_SlabPtr slab, std::vector<unsigned char>& vec, QReader &reader) {
	if(reader.hasMore() && ((reader.peekNext() == 'l') || (reader.peekNext() == 'L'))) {
	    vec.push_back('L');
	    reader.next();
	    slab->slabMD = slab->slabMD | quarry_Number_HasL;
	    if (reader.hasMore() && ((reader.peekNext() == 'l') || (reader.peekNext() == 'L'))) {
		vec.push_back('L');
		reader.next();
		slab->slabMD = slab->slabMD | quarry_Number_HasTwoLs;
	    }
	    return true;
	}
	return false;
    }

    quarry_SlabPtr OperatorLexer :: scan(QReader &reader, QContext &context) const {
	auto c = reader.next();
 	quarry_SlabPtr outSlab = new quarry_Slab();
	outSlab->col = reader.getCol();
	outSlab->line = reader.getLine();
	outSlab->slabType = quarry_Operator;
	std::vector<unsigned char> text;
	std::string t;
	text.push_back(c);
	t.push_back(c);
	while(reader.hasMore() &&  (allowed.find(reader.peekNext()) == std::string::npos)) {
	    text.push_back(c);
	    t.push_back(c);
	}
	fillToken(outSlab, text);
	outSlab->slabMD = context.operatorIndex(t);
	return outSlab;
    }
    
    quarry_SlabPtr NumberLexer :: scan(QReader &reader, QContext &context) const {
	auto c = reader.next();
	std::vector<unsigned char> vec;
	int column = reader.getCol();
	int line = reader.getLine();
	quarry_SlabPtr ret = new quarry_Slab();
	ret->line = line;
	ret->col = column;
	ret->slabType = quarry_Numbers;
	ret->slabMD = 0;
	
	vec.push_back(c);
	if (c > '0') {
	    while(reader.hasMore() && ((reader.peekNext() >= '0') && (reader.peekNext() <= '9'))) {
		vec.push_back(reader.next());
	    }
	    ret->slabMD = 0;
	    handleOptionalLL(ret, vec, reader);
	    auto hasU = handleOptionalU(ret, vec, reader);

	    if(handleOptionalLL(ret, vec, reader) || hasU) {
		//
	    }else if(handleOptionalU(ret, vec, reader)) {
		handleOptionalLL(ret, vec, reader);
	    }
	    else if(reader.hasMore() && ((reader.peekNext() == 'f') || (reader.peekNext() == 'f'))) {
		vec.push_back(reader.next());
		ret->slabMD = ret->slabMD | quarry_Number_IsFloat;
	    }else if(reader.hasMore() && ((reader.peekNext() == 'd') || (reader.peekNext() == 'D'))) {
		vec.push_back(reader.next());
		ret->slabMD = ret->slabMD | quarry_Number_IsDouble;
	    }else if(reader.hasMore() && (reader.peekNext() == '.')) {
		vec.push_back(reader.next());
		handleMantissa(ret, vec, reader);
		ret->slabMD = ret->slabMD | quarry_Number_HasDot;
	    }
	}
	else if (reader.hasMore() && (reader.peekNext() == '.')){
	    vec.push_back(reader.next());
	    ret->slabMD = ret->slabMD | quarry_Number_HasDot;
	    handleMantissa(ret, vec, reader);
	}
	else {
	    if (reader.hasMore() && ((reader.peekNext() == 'x') || (reader.peekNext() == 'X'))) {
		vec.push_back(reader.next());
		// hexadecimal
		while(reader.hasMore() && 
		      (
		       ((reader.peekNext() >= '0') && (reader.peekNext() <= '9')) ||
		       ((reader.peekNext() >= 'A') && (reader.peekNext() <= 'F')) ||
		       ((reader.peekNext() >= 'a') && (reader.peekNext() <= 'f')) )) {
		    vec.push_back(reader.next());		    
		}
		ret->slabMD = ret->slabMD | quarry_Number_IsHex;
	    }
	    else if(reader.hasMore() && ((reader.peekNext() == 'b') || (reader.peekNext() == 'B'))) {
		vec.push_back(reader.next());
		ret->slabMD = ret->slabMD | quarry_Number_IsBinary;
		while(reader.hasMore() && ((reader.peekNext() == '0') || (reader.peekNext() == '1'))) {
		    vec.push_back(reader.next());
		}
	    }
	    else{
		ret->slabMD = ret->slabMD | quarry_Number_IsOctal;
		while(reader.hasMore() && ((reader.peekNext() >= '0') && (reader.peekNext() <= '9'))) {
		    vec.push_back(reader.next());
		}
	    }
	    if(handleOptionalLL(ret, vec, reader)) {
		handleOptionalU(ret, vec, reader);
	    }
	    else {
		handleOptionalU(ret, vec, reader);
		handleOptionalLL(ret, vec, reader);
	    }
	}
	fillToken(ret, vec);
	return ret;
    }
    
    quarry_SlabPtr DelimitedLexer :: scan(QReader &reader, QContext &context) const {
	auto c = reader.next();
 	quarry_SlabPtr outSlab = new quarry_Slab();
	outSlab->col = reader.getCol();
	std::vector<unsigned char> text;
	text.push_back(c);
	if (reader.hasMore()) {
	    c = reader.next();
	    if (c == delimiter) {
		outSlab->line = reader.getLine();
		outSlab->slabMD = 0;
		outSlab->slabType = tokenType;
		fillToken(outSlab, text);
		return outSlab;
	    }
	    else if (c == escape) {
		if (reader.hasMore()) {
		    text.push_back(c);
		    text.push_back(reader.next());
		}
		else {
		    outSlab->line = reader.getLine();
		    outSlab->slabMD = 0;
		    outSlab->slabType = quarry_Error;
		    fillToken(outSlab, text);
		    return outSlab;
		}
	    }
	}
	else {
	    outSlab->line = reader.getLine();
	    outSlab->slabMD = 0;
	    outSlab->slabType = quarry_Error;
	    fillToken(outSlab, text);
	    return outSlab;
	}
    }
    quarry_SlabPtr SkipSpace :: scan(QReader &reader, QContext &context) const {
	auto initLine = reader.getLine();
	quarry_SlabPtr outSlab = new quarry_Slab();
	outSlab->col = reader.getCol();
	reader.next();
	while(reader.hasMore() && reader.peekNext() < 33) {
	    reader.next();
	}


	outSlab->slabType = quarry_Whitespace;
	outSlab->line = reader.getLine();
	outSlab->slabMD = initLine;
	outSlab->slabLength = 0;
	outSlab->data = nullptr;
	return outSlab;
    }
    
    void addPunctuationLexers (QContext &context) {
	context.lexers[';'] = new SingleCharLexer (';', quarry_Punctuation);
	context.lexers[':'] = new SingleCharLexer (':', quarry_Punctuation);
	context.lexers[','] = new SingleCharLexer (',', quarry_Punctuation);
	context.lexers['.'] = new SingleCharLexer ('.', quarry_Punctuation);
	context.lexers['?'] = new SingleCharLexer ('?', quarry_Punctuation);
	context.lexers['!'] = new SingleCharLexer ('!', quarry_Punctuation);
	context.lexers['`'] = new SingleCharLexer ('`', quarry_Punctuation);
    }
    
    void addGroupLexers (QContext &context) {
	context.lexers['('] = new SingleCharLexer ('(', quarry_Grouping);
	context.lexers[')'] = new SingleCharLexer (')', quarry_Grouping);
	context.lexers['{'] = new SingleCharLexer ('{', quarry_Grouping);
	context.lexers['}'] = new SingleCharLexer ('}', quarry_Grouping);
	context.lexers['['] = new SingleCharLexer ('[', quarry_Grouping);
	context.lexers[']'] = new SingleCharLexer (']', quarry_Grouping);
	context.lexers['<'] = new SingleCharLexer ('<', quarry_Grouping);
	context.lexers['>'] = new SingleCharLexer ('>', quarry_Grouping);
    }

    void addWhitespaceLexers (QContext  &context) {
	for(char index = 1;index<33;index++){
	    context.lexers[index] = new SingleCharLexer (index, quarry_Whitespace);
	}
	context.lexers['\r'] = new CRLexer();
	context.lexers['\n'] = new LFLexer();
    }

    BaseLexer* getDblCharCommentLexer(char b, char s, char e) {
	return new DoubleCharComment(b,s,e);
    }
}
