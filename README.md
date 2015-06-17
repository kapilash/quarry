# quarry

Quarry is a library for getting tokens from  UTF8 encoded text files.

The library can be used to tokenize a source file or an in memory string.

## Token
A slab is a structure with:
*  tokenType (an enum)
* line number
* column number
* text 



### Token Types
The various types of tokens are as follows
*  __Error__ 
*  __Keyword__ currently c and java keywords are supported. Library is
flexible enough to support a new set of keywords. Adding a new set is semi
automated. 
*  __Identifier__ a valid java identifier  (UTF-8) with a '$'.

*  __String__  Anything between double quotes (c#, java or c style string literals)
*  __Char__ same as double String but with single quotes
*  __Numbers__ decimals, binary, hexadecimal and octal numbers. The syntax follows c
rules. 
*  __Operator__  asterisk etc
*  __Grouping__ brackets (square,circular,curly and angular)
*  __Punctuation__ colon, semicolon, dot, comma, question mark, exclamation
mark 
*  __Comment__ java style block and single line comments. (metadata = 0 for
block comments and 1 for line comments)
*  __Whitespace__ a space or a tab character
*  __NewLine__ LF or CRLF
*  __EOF__ a token representing an end of file (this occurs after the last
line)



## Example
please consult example/example.c

## Build
> mkdir build
> cd build
> cmake .. -G <Your Generator> [-DBOOST_DIR=path-to-boost-build-dir]


For building the example,
> cd example
> mkdir build
> cmake .. -G <Your Generator> -DQU_DIR=<path-quarry-dir>

The cmake lists file assumes that the build dir is named build and is inside the root folder

builds an executable exquarry.out which prints some statistics about a given java source code

## Notes

* When a file is given as an input, it is memory-mapped and read

* The return value from a quarry_nextToken should not be deleted. More over, it is
intended to be copied immediately and freed (using quarry_freeToken) 

## TODO
* C++ API to be made more usable
* Proper packaging
* Clean up Numbers.cpp
* UTF-8 decoding is duplicated at two places.

