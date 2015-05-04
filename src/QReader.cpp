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
    
    QReader::QReader(const char *fileName){
        fp = OpenFile(fileName);
        if (NULL == fp) {
            throw new std::invalid_argument("could not open file");
        }
        isInMemory = false;
        line = 1;
        column = 1;
        bytes = new unsigned char[BUF_SIZE];
        length = BUF_SIZE;
        pos = -1;
    }

    QReader::QReader(const unsigned char *byteArray, int arrayLength, int l, int c){
        fp = NULL;
        length = arrayLength;
        bytes = new unsigned char[length];
        memcpy(bytes, byteArray, length);
        line = l; 
        column = c;
        isInMemory = true;
        pos = 0;
    }

    bool QReader::read() {
        if (isInMemory) {
            return false;
        }
        
        if (feof(fp)) {
            return false;
        }

        length = fread(bytes, sizeof(unsigned char), length, fp);
        pos = 0;
        if (ferror(fp)) {
            return false;
        }
        return true;                
    }
    
    QReader::~QReader() {
        if (NULL != fp) {
            fclose(fp);
        }
        delete []bytes;
        length = 0;
        line = 0;
        column = 0;
        pos = 0;
    }
}
