/*
Copyright (c)  Hemanth Kapila
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1) Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2) Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
The name of the Hemanth Kapila may NOT be used to endorse or promote products derived from this software without specific prior written permission.
3) THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
#include "QInternal.h"
#include <boost/lexical_cast.hpp>
namespace Quarry {

    
    /**

     * The accepted syntax is as follows:
     character-literal : ' character '
     
     character : single-character 
                 | simple-escape-sequence  
		 | hexadecimal-escape-sequence 
		 | unicode-escape-sequence 
	       
	 
      single-character :  any character except single-quote, backslash and new-line
      simple-escape-sequence : \' \" \\ \0 \a \b \f \n \t \r \v
      hexadecimal-escape-sequence: \x hex-digit (1-4)
      unicode-escape-sequence : \u hex-digit (x4)
	                         | \U hex-digit (x8)


     * Decoding UTF8 is heavily Influenced by rfc:3629 and  http://en.wikipedia.org/wiki/UTF-8
     
     
     * UTF-8 Format is
     
     Char. number range  |        UTF-8 octet sequence
        (hexadecimal)    |              (binary)
     --------------------+---------------------------------------------
     0000 0000-0000 007F | 0xxxxxxx
     0000 0080-0000 07FF | 110xxxxx 10xxxxxx
     0000 0800-0000 FFFF | 1110xxxx 10xxxxxx 10xxxxxx
     0001 0000-0010 FFFF | 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
     
     
    */

    static unsigned char eigthBitMask = 128;  //10000000
    static unsigned char seventhBitMask = 64; //01000000
    static unsigned char sixthBitMask = 32;   //00100000
    static unsigned char fifthBitMask = 16;   //00010000
    static unsigned char fourthBitMask = 8;

    enum CharErrorCode { InvalidUTFByte,  CharEOF, ValidUTF };

    inline bool isValidUtfTail (unsigned char uc) {
	//192 = 128 + 64. that is, 11000000
	return (uc & 192) == 128;
    }

    inline bool isValidHexChar(unsigned char c) {
	return ((c >= 'A' && c <= 'F') || ( c >= 'a' && c <= 'f') || (c >= '0' && c <= '9'));
    }
    
    // this method is called after the first byte has been read and has already been decided that this is 
    // a simple-character
    CharErrorCode readCodePoint(QReader &reader, unsigned char firstByte, char32_t &codePoint) {

	codePoint = 0;
	if (firstByte < 128) {
	    //we are in the first row (second column).
	    codePoint = firstByte;
	    return ValidUTF;
	}

	// we are in the second line (refering to the second column of above table)
	if (!(firstByte & seventhBitMask)) {
	    // firstByte is form 10xxxxxx. Incorrect. 
	    codePoint = 0;
	    return InvalidUTFByte;
	}

	if(!reader.hasMore()) {
	    // end of file after the first byte
	    codePoint = 0;
	    return CharEOF;
	}
	unsigned char secondByte = reader.next();
	if( !isValidUtfTail(secondByte)) {
	    return InvalidUTFByte;
	}
	if ((firstByte & sixthBitMask) == 0) {
	    // a valid two-byte sequence.
	    // Here, 0x3080 = 00110000 10000000
	    codePoint = (firstByte << 6) + secondByte - 0x3080;
	    return ValidUTF;
	}

	if(!reader.hasMore()) {
	    codePoint = 0;
	    return CharEOF;
	}
	unsigned char thirdByte = reader.next();
	if(!isValidUtfTail(thirdByte)) { 
	    return InvalidUTFByte;
	}
	if ( (firstByte & fifthBitMask) == 0) {
	    // a valid three byte sequence. 8th, 7th and 6th were set. fifth bit is unset.
	    codePoint = (firstByte << 12) + (secondByte << 6) + thirdByte - 0xE2080 ;
	    return ValidUTF;
	}
	
	if(!reader.hasMore()) {
	    codePoint = 0;
	    return CharEOF;
	}
	unsigned char fourthByte = reader.next();

	if(!isValidUtfTail(fourthByte)) {
	    codePoint = 0;
	    return InvalidUTFByte;
	}
	
	if ((firstByte & fourthBitMask) == 0) {
	    codePoint = (firstByte << 18) + (secondByte << 12) + (thirdByte << 6) + fourthByte - 0x3C82080;
	    return ValidUTF;
	}

	codePoint = 0;
	return InvalidUTFByte;
    }

    bool hexToUnicode(const std::string &bytes, char32_t &code){
	try {
	    char32_t x = std::stoul(bytes, nullptr, 16);
	    code = x;
	    return true;
	}
	catch(...) {
	    code = 0;
	    return false;
	}
    }
    
    Token* charLexer(QReader &reader, QContext &context) {
	auto col = reader.getCol();
	auto line = reader.getLine();
	auto c = reader.next();

	if (!reader.hasMore()) {
	    return new ErrorToken(line, col, std::string("EOF after quote"));
	}
	c = reader.next();
	char32_t code;
	if ( c != '\\' ) {
	    code = 0;
	    auto ret = readCodePoint(reader, c, code);
	    if (ret == InvalidUTFByte) {
		return new ErrorToken(line, col, "invalid UTF-8 byte");
	    }
	    if (ret == CharEOF) {
		return new ErrorToken(line, col, "unexpected end of file");
	    }
	    if (!reader.hasMore())
		return new ErrorToken(line, col, "unexpected end of file");
	    if(reader.peekNext() != '\'') {
		return new ErrorToken(line, col, "Invalid Char " );
	    }
	    reader.next();
	    reader.setColumn(col + 3);
	    return new CharToken(line, col, code);
	}
	//c == '\'
	if (!reader.hasMore()) {
	    return new ErrorToken(line, col, "unexpected end of file at '");
	}
	c = reader.next();
	int newCol = col + 4;
	switch(c) {
	case '\\' : code = '\\'; break;
	case '\'':  code = '\''; break;
	case '"' :  code = '"' ; break;
	case '0':   code =    0; break;
	case 'a':   code = '\a'; break;
	case 'n':   code = '\n' ;break;
	case 'b':   code = '\b'; break;
	case 't':   code = '\t'; break;
	case 'f':   code = '\f'; break;
	case 'r':   code = '\r'; break;
	case 'v':   code = '\v'; break;
	case 'u':  {
	    std::string hexStr("0x");
	    int count = 0;
	    // here, read four bytes and convert them into a hexadecimal value

	    while ((count < 4) && reader.hasMore()) {
		hexStr.append(1, reader.next());
		count++;
	    }
	    if(!(count == 4 && hexToUnicode(hexStr, code))) {
		return new ErrorToken(line,col, "Invalid Character");
	    }
	    newCol = col + 8; //2 quotes. \, U and 4 bytes.
	    break;
	}
	case 'U': {
	    // here, read eight bytes and convert them into a hexadeimal value
	    std::string hexStr("0x");
	    int count = 0;
	    while ((count < 8) && reader.hasMore()) {
		hexStr.append(1, reader.next());
		count++;
	    }
	    if(!(count == 8 && hexToUnicode(hexStr, code))) {
		return new ErrorToken(line,col, "Invalid Character");
	    }
	    newCol = col + 12; //2 quotes. 1 \, 1 U and 8 bytes
	    break;
	}
	case 'x': {
	    std::string hexStr("0x");
	    int count = 0;
	    // read upto four 0-9A-Fa-f
	    while ((count < 4) && reader.hasMore() && isValidHexChar(reader.peekNext())) {
		hexStr.append(1, reader.next());
		count++;
	    }
	    if(!hexToUnicode(hexStr, code)) {
		return new ErrorToken(line, col, "Invalid Character");
	    }

	    break;
	}
	default:
	    std::string message = "invalid escape sequence -\\" ;
	    message.append(1, c);
	    return new ErrorToken(line, col, message);
	}
	// if we are here, we have read a valid escape sequence.
	// Ensure that the next char is the closing quote and return.
	reader.setColumn(newCol);
	if(reader.hasMore() && reader.peekNext() == '\'') {
	    reader.next();
	    return new CharToken(line, col, code);
	}
	return new ErrorToken(line, col, "invalid character ");
    }

      
  Token* cstringLexer(QReader &reader, QContext &context) {
	auto col = reader.getCol();
	auto line = reader.getLine();
	auto c = reader.next();

	int additional = 0;
	char32_t code = 0;
	std::u32string text;
	while (reader.hasMore() && reader.peekNext() != '"') {
	  	c = reader.next();
		if ( c != '\\' ) {
		  code = 0;
		  auto ret = readCodePoint(reader, c, code);
		  if (ret == InvalidUTFByte) {
		    return new ErrorToken(line, col, "invalid UTF-8 byte");
		  }
		  if (ret == CharEOF) {
		    return new ErrorToken(line, col, "unexpected end of file");
		  }
		  text.push_back(code);
		}
		else {
		  	if (!reader.hasMore()) {
			  return new ErrorToken(line, col, "unexpected end of file at '");
			}

			c = reader.next();
			additional++;
			switch(c) {
			case '\\' : code = '\\'; break;
			case '\'':  code = '\''; break;
			case '"' :  code = '"' ; break;
			case '0':   code =    0; break;
			case 'a':   code = '\a'; break;
			case 'n':   code = '\n' ;break;
			case 'b':   code = '\b'; break;
			case 't':   code = '\t'; break;
			case 'f':   code = '\f'; break;
			case 'r':   code = '\r'; break;
			case 'v':   code = '\v'; break;
			case 'u':  {
			  std::string hexStr("0x");
			  int count = 0;
			  // here, read four bytes and convert them into a hexadecimal value

			  while ((count < 4) && reader.hasMore()) {
			    hexStr.append(1, reader.next());
			    count++;
			  }
			  if(!(count == 4 && hexToUnicode(hexStr, code))) {
			    return new ErrorToken(line,col, "Invalid Character");
			  }
			  additional = additional + 4;
			  break;
			}
			case 'U': {
			  // here, read eight bytes and convert them into a hexadeimal value
			  std::string hexStr("0x");
			  int count = 0;
			  while ((count < 8) && reader.hasMore()) {
			    hexStr.append(1, reader.next());
			    count++;
			  }
			  if(!(count == 8 && hexToUnicode(hexStr, code))) {
			    return new ErrorToken(line,col, "Invalid Character");
			  }
			  additional = additional + 8;
			  break;
			}
			case 'x': {
			  std::string hexStr("0x");
			  int count = 0;
			  // read upto four 0-9A-Fa-f
			  while ((count < 4) && reader.hasMore() && isValidHexChar(reader.peekNext())) {
			    hexStr.append(1, reader.next());
			    count++;
			  }
			  if(!hexToUnicode(hexStr, code)) {
			    return new ErrorToken(line, col, "Invalid Character");
			  }
			  additional = additional + count;
			  break;
			}
			default:
			  std::string message = "invalid escape sequence -\\" ;
			  message.append(1, c);
			  return new ErrorToken(line, col, message);
			}
			text.push_back(code);
		}
	}
	if (!reader.hasMore()) {
	  // unexpected EOF
	  return new ErrorToken(line, col, "runaway string");
	}
	reader.next();
	reader.setColumn(col + text.length() + additional);
	return new StringToken(line, col, std::move(text));
    }

    
    Token *genericIdLexer(bool (isValidIdentChar)(unsigned char), QReader &reader, QContext &context) {
            int line = reader.getLine();
            int col = 1;
            std::string text;
            int count = 1;
            text.push_back(reader.next());
	    auto c = reader.peekNext();
            while (reader.hasMore() && isValidIdentChar(c)) {
		text.push_back(reader.next());
		if (c > 127) {
		    while( reader.hasMore() && reader.peekNext() > 127) {
			text.push_back(reader.next());
		    }
		}
		col++;
		c = reader.peekNext();
	    }
	    reader.addColumns(col);
	    auto index = context.keywordIndex(text);
	    if (index < 0)
	    return new IdentifierToken(line, reader.getCol(), text);

	    return new Keyword(line,col, index);
    }

    static bool isCIdentChar(unsigned char b) {
	return ( b >= 'a' && b <= 'z')  
	    || (b >= 'A' && b <= 'Z')
	    || (b >= '0' && b <= '9')
	    || (b == '_')
	    || (b == '$');
							
    }

    class OperatorLexer {
    private:
	bool isValidOper(unsigned char c) {
	    auto i = operators.find(c);
	    return (i != std::string::npos);		
	}
	
    public:
	const std::string operators;
	OperatorLexer(const std::string &s) : operators(s) {
	}

	
	Token *scan(QReader &reader, QContext &context) {
	    int line = reader.getLine();
            int col = 1;
            std::string text;
            int count = 1;
            text.push_back(reader.next());
	    auto c = reader.peekNext();
            while (reader.hasMore() && isValidOper(c)) {
		text.push_back(reader.next());
		if (c > 127) {
		    while( reader.hasMore() && reader.peekNext() > 127) {
			text.push_back(reader.next());
		    }
		}
		col++;
		c = reader.peekNext();
	    }
	    reader.addColumns(col);
	    auto index = context.keywordIndex(text);
	    if (index < 0)
	    return new OperatorId(line, reader.getCol(), text);

	    return new Operator(line,col, index);
	}
    };

    Token *csIdLexer(QReader &reader, QContext &context) {
	return genericIdLexer(isCIdentChar,reader, context);
    }

    Token *csOperatorLexer(QReader &reader, QContext &context) {
	OperatorLexer lex("!#$%&*+-/<=>@^_`~");
	return lex.scan(reader, context);
    }
}
