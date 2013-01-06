
libquarry.a : comments.o quarry_util.o misc_lexers.o javaKwTable.o cKwTable.o quarry.o
	ar -cvq libquarry.a $^


quarry.o: quarry.h quarry_internal.h quarry.c
	gcc -c -O3 quarry.c -o quarry.o

javaKwTable.o: quarry_kw.h javaKwTable.c
	gcc -c -O3 javaKwTable.c -o javaKwTable.o

cKwTable.o: quarry_kw.h cKwTable.c
	gcc -c -O3 cKwTable.c -o cKwTable.o

cKwTable.c: C KWStrGenerator.hs
	ghc --make KWStrGenerator.hs
	./KWStrGenerator C > cKwTable.c

javaKwTable.c:	Java KWStrGenerator.hs
	ghc --make KWStrGenerator.hs
	./KWStrGenerator Java > javaKwTable.c

misc_lexers.o : lexers.h quarry.h quarry_kw.h misc_lexers.c
	gcc -c -O3 misc_lexers.c -o misc_lexers.o

comments.o: lexers.h quarry.h comments.c
	gcc -c -O3 comments.c -o comments.o

quarry_util.o: quarry_util.c quarry_internal.h quarry.h
	gcc -c -O3 quarry_util.c -o quarry_util.o

comments_tests.o: comments_tests.c quarry_testsuites.h libquarry.a
	gcc -c  comments_tests.c -o comments_tests.o

reader_tests.o: reader_tests.c quarry_testsuites.h libquarry.a
	gcc -c reader_tests.c -o reader_tests.o

quotes_tests.o:quotes_tests.c quarry_testsuites.h libquarry.a
	gcc -c quotes_tests.c -o quotes_tests.o

numbers_tests.o:numbers_tests.c quarry_testsuites.h libquarry.a
	gcc -c numbers_tests.c -o numbers_tests.o

javakw_tests.o:javakw_tests.c quarry_testsuites.h libquarry.a
	gcc -c javakw_tests.c -o javakw_tests.o

identifier_tests.o:identifier_tests.c quarry_testsuites.h libquarry.a
	gcc -c identifier_tests.c -o $@

libtestquarry.a : quotes_tests.o comments_tests.o numbers_tests.o javakw_tests.o identifier_tests.o reader_tests.o
	ar -cvq $@ $^

quarry_tests.out: quarry_testsuites.h quarry_testrunner.c libquarry.a libtestquarry.a
	gcc -o quarry_tests.out quarry_testrunner.c libtestquarry.a libquarry.a -lcheck

check: quarry_tests.out libtestquarry.a libquarry.a
	./quarry_tests.out


example: exquarry.c libquarry.a
	gcc -o exquarry.out exquarry.c libquarry.a

clean:
	rm -f *.a
	rm -f *.o
	rm -f *.out
	rm -f *.hi
	rm -f KWStrGenerator

