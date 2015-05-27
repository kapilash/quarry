#include <QReader.h>


namespace Quarry {

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
      file.open(fileName);
      if(!file.is_open()){
	throw new std::invalid_argument("could not open file");
      }
      line = 1;
      column = 1;
      position = 0;
      length = file.size();
      bytes = reinterpret_cast<const unsigned char *>(file.data());
      isInMemory = false;
    }

    QReader::QReader(const unsigned char *byteArray, size_t arrayLength, int l = 1, int c = 1){
	bytes = new unsigned char[BUFSIZ];
	std::memcpy((void *)bytes, byteArray, arrayLength);
        line = l; 
        column = c;
	position = 0;
	length = arrayLength;
	isInMemory = true;
    }
    
    QReader::~QReader() {
      if (file.is_open()) {
	file.close();
      }
      line = 0;
      column = 0;
      position = 0;
      if (isInMemory)
	  delete []bytes;
    }
}
