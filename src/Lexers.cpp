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
        
    // this is expected to be called only after the first has already been consumed.
    template <char begin='/', char second = '*' >
        Token *genericBlockComment(QReader &reader, QContext &context) {
            int line = reader.getLine();
            int col = reader.getCol()-1;
            std::string text;
            int count = 1;
            reader.next();

            while (reader.hasMore()) {
                auto c = reader.next();
                if ( c > 127) {
                    // this is the beginning of a UTF8 value
                    while (reader.hasMore() && reader.peekNext() > 127) {
                        text.push_back(reader.next());
                    }
                }
                else if ( c == '\n') {
                    col = 0;
                    continue;
                }
                else if (c == second && reader.hasMore() && (reader.peekNext() == begin)) {
                    col++;
                    count--;
                    reader.next();
                    if (count < 1) {
                        break;
                    }
                    text.push_back(c); text.push_back(begin);
                }
                else if (c == begin && reader.hasMore() && (reader.peekNext() == second)) {
                    text.push_back(c); text.push_back(reader.next());
                    col++;
                    count++;
                }
                else {
                    text.push_back(c);
                }
                col++;
            }
            return new CommentToken(line, col, text);
        }

    Token* csComments(QReader &reader, QContext &context) {
        auto col = reader.next();
        if (reader.hasMore() && reader.peekNext() == '/') {
            std::string text("//");
            while (reader.hasMore() && reader.peekNext() != '\n') {
                text.push_back(reader.next());
            }
            if (reader.hasMore())
                reader.next();
            return new CommentToken(reader.getLine(), col, text);
        }
        else if (reader.hasMore() && reader.peekNext() == '*') {
            return genericBlockComment<>(reader, context);
        }
        else {
            std::string t;
            t.push_back(reader.next());
            return new Operator(reader.getLine(), col, context.operatorIndex(t));
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
    
    template <char delimiter, char escape, enum TokenType tokenType>
    Token* delimitedStrToken(QReader &reader, QContext &context){
	auto c = reader.next();
	auto col = reader.getCol();
	auto line = reader.getLine();
	std::string text;
	text.push_back(c);
	while (reader.hasMore()) {
	    c = reader.next();
	    text.push_back(c);
	    if (c == delimiter) {
		return new GenericToken<std::string, tokenType>(line, col, std::move(text)); 
	    }
	    else if (c == escape) {
		if (reader.hasMore()) {
		    text.push_back(reader.next());
		}
	    }
	}
	return new GenericToken<std::string, ERROR>(line, col, std::move(text)); 
    }

    Token*  spaceLexer(QReader &reader, QContext &context) {
	auto line = reader.getLine();
	auto col = reader.getCol();
	reader.next();
	
	while(reader.hasMore() && reader.peekNext() < 33) {
	    reader.next();
	}
	return new Token(line, col, WHITESPACE);
    }

    
    void addPunctuationLexers (QContext &context) {
	
    }
    
    void addGroupLexers (QContext &context) {
	
    }

    void addWhitespaceLexers (QContext  &context) {
    }

}
