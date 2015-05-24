#include <QReader.h>


namespace Quarry {
    static const size_t BUF_SIZE = 4096;

    static std::FILE* OpenFile(const char *fileName) {
#ifdef _MSC_VER
        std::FILE *fp;
        int err = fopen_s(&fp, fileName, "rb");
        if (0 == err){
            return fp;
        }
        return NULL;
#else
        return fopen(fileName,"rb");
#endif
    }

    static void fillStack(std::stack<unsigned char> &bytes,const unsigned char *byteArray, size_t arrayLength) {
	for(int i=arrayLength; i>0; --i) {
	    auto c = byteArray[i-1];
	    if (c != '\r') {
		bytes.push(c);
	    }
	}	
    }
    
    QReader::QReader(const char *fileName){
        fp = OpenFile(fileName);
        if (NULL == fp) {
            throw new std::invalid_argument("could not open file");
        }
        isInMemory = false;
        line = 1;
        column = 1;
    }

    QReader::QReader(const unsigned char *byteArray, size_t arrayLength, int l, int c){
        fp = NULL;
        fillStack(bytes, byteArray, arrayLength);
        line = l; 
        column = c;
        isInMemory = true;
    }

    void QReader::read() {
        if (isInMemory) {
            return ;
        }
        
        if (feof(fp)) {
            return;
        }

	unsigned char *byteArray = new unsigned char[BUF_SIZE];
        int length = fread(byteArray, sizeof(unsigned char), BUF_SIZE, fp);
        if (ferror(fp)) {
            return;
        }
	fillStack(bytes, byteArray, length);
        return;                
    }
    
    QReader::~QReader() {
        if (NULL != fp) {
            fclose(fp);
        }
        line = 0;
        column = 0;
    }
}
