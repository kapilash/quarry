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

struct KWIndex {
    int asciiCode;
    int concIndex;
    int kwLenIndex;
    int numWords;
};

void addBeginning(std::ofstream &outfile) {
    outfile << "/* this is generated code */" << std::endl
	    << "#include <set>" << std::endl
	    << "#include <string>" << std::endl
	    << std::endl
	    << std::endl
	    << "namespace quarry{" << std::endl;
}

void addEnding(std::ofstream &outfile) {
    outfile << "}" << std::endl;
}
void genCode(std::ofstream &outfile, std::string &langName, std::set<std::string> &set) {
   outfile    << "    void collect" << langName << "(std::set<std::string> &kwset) {" << std::endl;

    for (std::set<std::string>::iterator it = set.begin(); it != set.end(); ++it) {
	outfile << "        kwset.insert(\"" << *it << "\");" << std::endl;
    }
    outfile << "    }" << std::endl;
}

int main(int argc, char **argv){
    if (argc < 3) {
	std::cout << "usage:" << std::endl;
	std::cout << argv[0] << " outputfile <keywords-file> [keywords-file> ... ]" << std::endl;
	return 1;
    }

    std::ofstream outFile(argv[1]);
    addBeginning(outFile);
    for(int i=2; i < argc; i++) {
	std::ifstream inFile(argv[i]);
	std::string langName(argv[i]);
	std::string line;
	std::set<std::string> kwset;
	while (std::getline(inFile, line)) {
	    kwset.insert(line);
	}	
	genCode(outFile, langName, kwset);
    }
    addEnding(outFile);
    
}
