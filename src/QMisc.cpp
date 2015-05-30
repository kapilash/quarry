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
#include <iomanip>
namespace Quarry {

    std::string asBits(unsigned long v)
    {
	std::string digits = "0123456789abcdef";
	std::string result;
	do {
	    result = digits[v % 2] + result;
	    v /= 2;
	}
	while(v);
	return result;
    }

    void printSlab(quarry_SlabPtr p,int index) {
	if (index >= 0) {
	    std::cout << std::setfill(' ') << std::setw(5) << (index+1) << "." ;
	}
	if (p == nullptr) {
	    std::cout << "(null)" ;
	}
	else{
	    std::cout << "{(position = " << p->line << "," << p->col << ")[" ;
	    switch(p->slabType) {
	    case   quarry_Error:      { std::cout << "ERROR";break;}
	    case   quarry_Keyword:    { std::cout << "KEYWORD";break;}
	    case   quarry_Identifier: {std::cout << "IDENTIFIER";break;}
	    case   quarry_String:     {std::cout << "STRING";break;}
	    case   quarry_Char:       {std::cout << "CHAR";break;}
	    case   quarry_Numbers:    {std::cout << "NUMBERS";break;}
	    case   quarry_Operator:   {std::cout << "OPERATOR";break;}
	    case   quarry_Grouping:   {std::cout << "GROUPING";break;}
	    case   quarry_Punctuation:{std::cout << "PUNCTUATION";break;}
	    case   quarry_Comment:    {std::cout << "COMMENT";break;}
	    case   quarry_Whitespace: {std::cout << "WHITESPACE";break;}
	    case   quarry_NewLine:    {std::cout << "NEWLINE";break;}
	    case quarry_EOF:          {std::cout << "EOF";break;}
	    case quarry_MetaId:       {std::cout << "META";break;}
	    default: std::cout << "UNKNOWN";
	    }
	    std::cout << "," << p->slabLength << "]";
	    for(int i=0; i< p->slabLength; i++) {
		std::cout << p->data[i];
	    }
	    std::cout << "<" << p->slabMD << "=" << asBits(p->slabMD);
	    std::cout << ">}";
	}
	std::cout << std::endl;
    }
}
