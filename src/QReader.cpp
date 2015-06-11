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

  std::size_t skipBom(std::size_t position, std::size_t length,const unsigned char *bytes) {
    if(position + 3 < length) {
      if(bytes[position] == 239 && bytes[position+1] == 187 && bytes[position+2] == 191) {
	return ( position + 3);
      }
    }
    return position;
  }

    
    QReader::QReader(const char *fileName){
      file.open(fileName);
      if(!file.is_open()){
	throw new std::invalid_argument("could not open file");
      }
      line = 1;
      column = 1;
     
      length = file.size();
      bytes = reinterpret_cast<const unsigned char *>(file.data());
      position = skipBom(0, length, bytes);
    }

    QReader::QReader(const unsigned char *byteArray, size_t arrayLength, int l = 1, int c = 1){
	bytes = new unsigned char[BUFSIZ];
	std::memcpy((void *)bytes, byteArray, arrayLength);
        line = l; 
        column = c;
	position = 0;
	length = arrayLength;
	position = skipBom(0, length, bytes);
    }
  
    QReader::~QReader() {
      if (file.is_open()) {
	file.close();
      }
      else {
	  delete []bytes;
      }
      line = 0;
      column = 0;
      position = 0;
    }

    int QReader::matchWhile(bool (*predicate)(unsigned char))
    {
	size_t i = position;
	while((i < length) && predicate(bytes[i])) {
	    ++i;
	}
	return i;
    }

    int QReader::till(bool (*predicate)(unsigned char))
    {
	int count = 0;
	for(size_t i = position; i < length; i++) {
	    if(predicate(bytes[i]))
		break;
	    count++;
	}
	return count;
    }

    void QReader::move(int count) {
	for(int i = 0; (i < count) && ( position < length); i++){
	    position++;
	}
    }
}
