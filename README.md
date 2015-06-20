# quarry

Quarry is a library for getting tokens from  UTF8 encoded text files.

The library can be used to tokenize a source file or an in memory string.

## C Interface

### Token
A token is a structure with:
*  line (an int)
*  column (an int)
*  tokenType (an int)
*  pointer to an array of UTF-8 bytes representing the text of current token (This can be null for certain _obvious_ tokens)
*  length a number indicating the length of current token. It is set to 0 when the pointer is set to nullptr.


### Eventually,

we will have further api that can extract other useful info given a token. At the moment, it is of little use other than that
things like a literate programming tool.




## Example
please consult example/example.c

## Build

You need cmake (version 3 or above) and boost.

Create a directory in the root folder (or any other place actually).

> mkdir build

Move to the build folder and use your favourite generator. For unix like systems, if boost is *installed*,  you do not need to provide a value to  BOOST_DIR.
On windows with visual studio, you may have to. Typically, the value is the directory where you've built your boost sources.

For mingw on windows, if you are using  this awesome version of mingw (http://www.nuwen.net/mingw.html), you just need to use the MSys generator. 

> cmake .. -G <Your Generator> [-DBOOST_DIR=path-to-boost-build-dir]


Building the example in the example directory is a little crude, for now. Just look at the CMakeLists.txt or modify it appropriately.


## Notes

* When a file is given as an input, it is memory-mapped and read

* The return value from a quarry_nextToken should not be deleted. More over, it is
intended to be copied immediately and freed (using quarry_freeToken) 

## TODO
* C++ API to be made more usable
* Proper packaging
* Clean up Numbers.cpp
* UTF-8 decoding code is duplicated at two places.


