CC = gcc
CPP = g++
CFLAGS = -Wall -Wextra -O -std=c99
CPPFLAGS = -Wall -Wextra -O

test: test.o bdd_triple_vector.o
	$(CC) -o test $^

test.o: test.c bdd.h

bdd_triple_vector.o: bdd_triple_vector.cpp bdd_triple.h

.PHONY: clean
clean:
	rm -f *.o test
