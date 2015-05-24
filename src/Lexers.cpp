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
    class SingleCharLexer : public BaseLexer {
    private:
	const char c;
	const enum quarry_SlabType slabType;
	
    public:
	SingleCharLexer(char given, enum quarry_SlabType givenSlabType) : c(given), slabType(givenSlabType) {}
	quarry_SlabPtr scan(QReader &reader, QContext &context) const {
	    quarry_SlabPtr outSlab = new quarry_Slab();
	    unsigned char c = reader.next();
	    outSlab->slabType = slabType;
	    outSlab->line = reader.getLine();
	    outSlab->col = reader.getCol();
	    outSlab->data = new unsigned char[1];
	    outSlab->data[0] = c;
	    outSlab->slabLength = 1;
	    outSlab->slabMD = c;
	    return outSlab;
	}
    };

    class LFLexer : public SingleCharLexer {
    public :
	LFLexer(char g = '\n', enum quarry_SlabType gst = quarry_NewLine) : SingleCharLexer (g, gst) {}
	
	quarry_SlabPtr scan(QReader &reader, QContext &context) const {
	    auto ret = SingleCharLexer::scan(reader, context);
	    reader.incrementLine();
	    return ret;
	}
    };

    class CRLexer : public BaseLexer {
    public:
	quarry_SlabPtr scan(QReader &reader, QContext &context) const {
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
	    outSlab->data = new unsigned char[1];
	    outSlab->data[0] = c;
	    outSlab->slabLength = 1;
	    outSlab->slabMD = crlf;
	    return outSlab;
	}
    };

    class DoubleCharComment : public BaseLexer {
    private:
	const char begin;
	const char second;
	const char last;
    public:
	
	DoubleCharComment(char b, char s, char l):begin(b), second(s), last(l) {}
	
	// This assumes that, when it is called, we Peek points to the second char
	quarry_SlabPtr scan(QReader &reader, QContext &context) const {
	    quarry_SlabPtr outSlab = new quarry_Slab();
	    reader.next();
	    std::string text;
	    unsigned char nextByte; //we read the second char
	    int count = 1;
	    
	    while (reader.hasMore()) {
		auto c = reader.next();
		if (c == second) {
		    if (reader.hasMore() && (reader.peekNext() == second)) {
			reader.next();
			count--;
			if (count < 1)
			    break;
		    }
		}
		else if (c == begin) {
		    if (reader.hasMore() && reader.peekNext() == second) {
			reader.next();
			text.append(1,begin);
			c = second;
			count++;
		    }
		}
		text.append(1,c);
	    }
	    outSlab->slabType = quarry_Comment;
	    outSlab->line = reader.getLine();
	    outSlab->col = reader.getCol();
	    outSlab->data = (unsigned char *)text.c_str();
	    outSlab->slabLength = text.length();
	    outSlab->slabMD = 1;
	    return outSlab;
	}
    };
    class CLikeComment : public BaseLexer {
    public:
	quarry_SlabPtr scan(QReader &reader, QContext &context) const {
	    quarry_SlabPtr outSlab = new quarry_Slab();
	    auto c = reader.next();
	    int col = reader.getCol();
	    if (reader.hasMore() && (reader.peekNext() == '/') ) {
		reader.next();
		unsigned char nextByte;
		std::string text;
		while (reader.hasMore() && (reader.peekNext() != '\n')) {
		    nextByte = reader.next();
		    if (nextByte != '\r') {
			text.append(1,nextByte);
		    }
		}
		outSlab->slabType = quarry_Comment;
		outSlab->line = reader.getLine();
		outSlab->col = col;
		outSlab->data = (unsigned char*)text.c_str();
		outSlab->slabLength = text.length();
		outSlab->slabMD = 0;
	    }
	    else if (reader.hasMore() && (reader.peekNext() != '*')) {
		reader.next();
		std::string text;
		unsigned char nextByte;
		while (reader.hasMore()) {
		    auto c = reader.next();
		    if (c == '*') {
			if (reader.hasMore() && (reader.peekNext() == '/')) {
			    reader.next();
			    break;
			}
		    }
		    if (c == '\n') {
			reader.incrementLine();
		    }
		    text.append(1,c);
		}
		outSlab->slabType = quarry_Comment;
		outSlab->line = reader.getLine();
		outSlab->col = col;
		outSlab->data = (unsigned char *)text.c_str();
		outSlab->slabLength = text.length();
		outSlab->slabMD = 1;
	    }
	    else {
		outSlab->slabType = quarry_Operator;
		outSlab->line = reader.getLine();
		outSlab->col = reader.getCol();
		outSlab->data = new unsigned char[1];
		outSlab->data[0] = c;
		outSlab->slabLength = 1;
		outSlab->slabMD = c;
	    }
	    return outSlab;
	}
    };

    class SingleCharComment : public BaseLexer {
	
    public:
	quarry_SlabPtr scan(QReader &reader, QContext &context) const {
	    quarry_SlabPtr outSlab = new quarry_Slab();
	    auto c = reader.next();
	    int col = reader.getCol();
	    std::string text;
	    while (reader.hasMore() && IS_IDENTIFIER_CHAR(reader.peekNext())) {
		text.append(1,reader.next());
	    }
	    outSlab->slabType = quarry_Identifier;
	    outSlab->line = reader.getLine();
	    outSlab->col = col;
	    outSlab->data = (unsigned char *)text.c_str();
	    outSlab->slabLength = text.length();
	    outSlab->slabMD = 0;
	    return outSlab;
	}
    };

    class CIdentifier : public BaseLexer {
    public:
	quarry_SlabPtr scan(QReader &reader, QContext &context) const {
	    quarry_SlabPtr outSlab = new quarry_Slab();
	    auto c = reader.next();
	    int col = reader.getCol();
	    std::string text;
	    while (reader.hasMore() && (reader.peekNext() != '\r') && (reader.peekNext() != '\n')) {
		text.append(1,reader.next());
	    }
	    int i = context.keywordIndex(text);
	    outSlab->line = reader.getLine();
	    outSlab->col = col;
	    if (i < 0) {
		outSlab->slabType = quarry_Identifier;
		outSlab->data = (unsigned char *)text.c_str();
		outSlab->slabLength = text.length();
		outSlab->slabMD = 0;
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
    };

    class SchemeHashLexer : public BaseLexer {
    private:
	quarry_SlabPtr scanBool(QReader &reader, QContext &context, bool b) const {
	    return nullptr;
	}
	quarry_SlabPtr scanChar(QReader &reader, QContext &context) const {
	    reader.next();
	    
	    return nullptr;
	}
	quarry_SlabPtr scanComment(QReader &reader, QContext &context) const {
	    return nullptr;
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
    };
    class CCharLexer : public BaseLexer {
    public:
	quarry_SlabPtr scan(QReader &reader, QContext &context) const {
	    quarry_SlabPtr outSlab = new quarry_Slab();
	    reader.next();
	    int col = reader.getCol();
	    std::string text;
	    if (reader.hasMore()) {
		auto c = reader.next();
		if (c == '\'') {
		    //return error
		}
		else if (c == '\\') {
		    if (reader.hasMore()) {
			//append c and the next.
			//
		    }
		    else {
			// return unexpected end of file
		    }
		}
	    }
	    else {
		// return a single-quote operator
	    }
	    while (reader.hasMore() && (reader.peekNext() != '\r') && (reader.peekNext() != '\n')) {
		text.append(1,reader.next());
	    }
	    int i = context.keywordIndex(text);
	    outSlab->line = reader.getLine();
	    outSlab->col = col;
	    if (i < 0) {
		outSlab->slabType = quarry_Identifier;
		outSlab->data = (unsigned char *)text.c_str();
		outSlab->slabLength = text.length();
		outSlab->slabMD = 0;
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
    };
    
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
}
