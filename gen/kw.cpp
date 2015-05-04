/*
Copyright (c) 2013, Hemanth Kapila
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
The name of Hemanth Kapila may NOT be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <fstream>
#include <string>
#include <iostream>
#include <vector>
#include <set>
#include <algorithm>
#include <cstdlib>
#include <locale>

class Keyword {
private:
    std::string keyword;
    std::string name;
public:
    Keyword(std::string k, std::string n) : keyword(k), name(n){
    }
    Keyword() {}
    Keyword(Keyword&& other){
	keyword = std::move(other.keyword);
	name = std::move(other.name);
    }
    Keyword(const Keyword& other){
	keyword = other.keyword;
	name = other.name;
    }
    std::string getName() const { return name; }
    std::string getKeyword() const {return keyword;}

    friend bool operator< (const Keyword &l, const Keyword &r)
    {
	return l.keyword < r.keyword;
    }

    void operator= (const Keyword that)
    {
	keyword = that.keyword;
	name = that.name;
    }

    friend std::istream& operator>>(std::istream & i, Keyword &key) {
	i >> key.keyword >> key.name;
	return i;
    }
};
void addCPPBeginning(std::ofstream &outfile) {
    outfile << "/* this is generated code */" << std::endl
	    << "#include <map>" << std::endl
	    << "#include <string>" << std::endl
	    << std::endl
	    << std::endl
	    << "namespace Quarry{" << std::endl;
}

void addCPPEnding(std::ofstream &outfile) {
    outfile << "}" << std::endl;
}

void addCSBeginning(std::ofstream &outfile) {
    outfile << "/* this is generated code */" << std::endl
	    << "namespace Quarry {" << std::endl
	    << "using System; " << std::endl;
}
void addCSEnding(std::ofstream &outfile){
    outfile << "}" << std::endl;
}

std::string toUpperCase(std::string &str) {
    std::string out(str);
    std::transform(str.begin(), str.end(), out.begin(), ::toupper);
    return out;
}
void genCSEnum(std::ofstream &outfile, std::string &langName, std::vector<Keyword> &keywords){
    outfile << "    public enum " << langName << " {" << std::endl ;
    int  index = 0;
    for (auto it = keywords.begin(); it != keywords.end(); ++it) {
	if (index != 0){
	    outfile << "," << std::endl;
	}
	outfile << "        " << it->getName() << " = " << index ;
	index++;
    }
    outfile << std::endl;
    outfile << "    }" << std::endl << std::endl;
}
void genCPPCode(std::ofstream &outfile, std::string &langName, std::vector<Keyword> &set) {
   outfile    << "    void collect" << langName << "(std::map<std::string,int> &kwmap) {" << std::endl;
   int ind = 0;
    for (auto it = set.begin(); it != set.end(); ++it) {
	outfile << "        kwmap[\"" << it->getKeyword() << "\"] = " << ind << ";" << std::endl;
	ind++;
    }
    outfile << "    }" << std::endl;
}

int main(int argc, char **argv){
    if (argc < 3) {
	std::cout << "usage:" << std::endl;
	std::cout << argv[0] << " outputfile <keywords-file> [keywords-file> ... ]" << std::endl;
	return 1;
    }
    std::string cppFileName(argv[1]);
    std::string csFileName(argv[1]);
    cppFileName += ".cpp";
    csFileName += ".cs";
    std::ofstream cppFile(cppFileName.c_str());
    std::ofstream csFile(csFileName.c_str());
    addCPPBeginning(cppFile);
    addCSBeginning(csFile);
    for(int i=2; i < argc; i++) {
	std::ifstream inFile(argv[i]);
	std::string langName(argv[i]);
	std::set<Keyword> kwset;
	Keyword temp;
	while (inFile >> temp) {
	    kwset.insert(temp);
	}
	std::vector<Keyword> kwvec;
	for(std::set<Keyword>::iterator it = kwset.begin(); it != kwset.end(); ++it) {
	    kwvec.push_back(*it);
	}
	std::sort(kwvec.begin(), kwvec.end());
	genCPPCode(cppFile, langName, kwvec);
	genCSEnum(csFile, langName, kwvec);
    }
    addCPPEnding(cppFile);
    addCSEnding(csFile);
}
