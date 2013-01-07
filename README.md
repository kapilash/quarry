# quarry

Quarry is a library for getting crude tokens from programming languages.

The library can be used to read a source file in terms of "Slabs".

## Slab 
A slab is a structure with:
*  type (an enum)
* line number
* column number
* text 

Think slab as a crude token.

### Slab Types
The following are the various types of slabs
*  __Error__ 
*  __Keyword__ currently c and java keywords are supported. Library is
flexible enough to support a new set of keywords. Adding a new set is semi automated. 
*  __Identifier__ a valid java identifier  (UTF-8) with a '$'
*  __String__  Anything between double quotes (but the second double quote
should not be preceded by an unescaped backslash)
*  __Char__ same as double String but with single quotes
*  __Numbers__ decimals, binary, hexadecimal and octal numbers (following c rules)
*  __Operator__  asterisk etc
*  __Grouping__ brackets (square,circular,curly and angular)
*  __Punctuation__ colon, semicolon, dot, comma, question mark, exclamation
mark 
*  __Comment__ java style block and single line comments. 
*  __Whitespace__ a space or a tab character
*  __NewLine__ LF or CRLF
*  __EOF__ a token representing an end of file (this occurs after the last
line)

## Example
please consult exquarry.c

## Build
> make check

will build and run the tests

> make example

builds an executable exquarry.out which prints some statistics about a given
java source code

## Notes

* There is a hard limit on the size of individual slab. For any given slab, only
the first 4096 bytes will be considered. The rest will be ignored.

* The return value from a quarry_read should not be deleted. More over, it is
intended to be copied immediately and freed (using quarry_freeSlab) before the
next call to quarry_read. (The ideal use of this value is to copy the contents into a GC'ed 
memory and releasing the slab).

## TODO
* add tests for UTF-8 identifiers
* add comprehensive tests for c
* support for haskell and ada style comments
