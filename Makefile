CC = g++
CPP = g++
CFLAGS = -Wall -Wextra -O -std=c99
CPPFLAGS = -Wall -Wextra -O

test: test.o bdd.o

test.o: test.c bdd.h

bdd.o: bdd.cpp

.PHONY: clean
clean:
	rm -f *.o test
