/*
Copyright (c) Hemanth Kapila
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
The name of Hemanth Kapila may NOT be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <string>
#include "QToken.h"
#include "quarry_export.h"


namespace Quarry {
  Token:: Token(int line, int column, enum TokenType t): line(line), column(column), tokenType(t)
  {}
  Token:: Token(const Token &other)  : line(other.line), column(other.column), tokenType(other.tokenType)
  {
  }

  CharToken::CharToken(int line, int column, char32_t value): Token(line,column, CHAR), value(value)
  {
  }
	
  CharToken::CharToken(const CharToken &other): Token(other.line,other.column, CHAR), value(other.value)
  {
  }

  ErrorToken::ErrorToken(int line, int column, std::string text): Token(line,column, ERROR), value(text)
  {
  }
	
  ErrorToken::ErrorToken(const ErrorToken &other): Token(other.line,other.column, ERROR), value(other.value)
  {
  }

  
  StringToken::StringToken(int line, int column, std::u32string text): Token(line,column, STRING), value(text)
  {
  }
	
  StringToken::StringToken(const StringToken &other): Token(other.line,other.column, STRING), value(other.value)
  {
  }

  
  IdentifierToken::IdentifierToken(int line, int column, std::string text): Token(line,column, IDENT), value(text)
  {
  }
	
  IdentifierToken::IdentifierToken(const IdentifierToken &other): Token(other.line,other.column, IDENT), value(other.value)
  {
  }

}
