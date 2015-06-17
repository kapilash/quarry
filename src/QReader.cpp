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

    void QReader::appendWhile(bool (*predicate)(unsigned char), std::string& text)
    {
	while(hasMore() && predicate(bytes[position])) {
	    text.push_back(bytes[position]);
	    next();
	}
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

    int QReader::asLongAs(bool (*predicate)(unsigned char)) {
	int i = 0;
	while(hasMore() && predicate(bytes[position])) {

	    next();
	    i++;
	}
	return i;
    }

    /** Decoding UTF8 is heavily Influenced by rfc:3629 and  http://en.wikipedia.org/wiki/UTF-8
      UTF-8 Format is
     
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
    

    int readCodePoint(const unsigned char *bytes, std::size_t pos, std::size_t max, char32_t &codePoint) {
	codePoint = 0;
	unsigned char firstByte = bytes[pos];
	if (bytes[pos] < 128) {
	    //we are in the first row (second column).
	    codePoint = firstByte;
	    return 1;
	}

	pos++;
	if (pos >= max) {
	    return -1;
	}
	unsigned char secondByte = bytes[pos];
	if ((firstByte & sixthBitMask) == 0) {
	    // a valid two-byte sequence.
	    // Here, 0x3080 = 00110000 10000000
	    codePoint = (firstByte << 6) + secondByte - 0x3080;
	    return 2;// we consumed two bytes
	}
	pos++;
	if(pos >= max) { 
	    codePoint = 0;
	    return -2;
	}
	unsigned char thirdByte = bytes[pos];
	
	if ( (firstByte & fifthBitMask) == 0) {
	    // a valid three byte sequence. 8th, 7th and 6th were set. fifth bit is unset.
	    codePoint = (firstByte << 12) + (secondByte << 6) + thirdByte - 0xE2080 ;
	    return 3;
	}
	pos++;

	if(pos >= max) {
	    codePoint = 0;
	    return -3;
	}
	unsigned char fourthByte = bytes[pos];
	
	if ((firstByte & fourthBitMask) == 0) {
	    codePoint = (firstByte << 18) + (secondByte << 12) + (thirdByte << 6) + fourthByte - 0x3C82080;
	    return 4;
	}
	return -4;
    }

    char32_t QReader::nextChar() {
	char32_t codePoint = 0;
	auto i = readCodePoint(bytes, position,length, codePoint);
	if ( i > 0) {
	    if (codePoint == '\n') {
		line++;
		column = 1;
	    }
	    else {
		column++;
	    }
	    position = position + i;
	    return codePoint;
	}
	column++;
	
	return 0;
    }

}
