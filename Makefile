CC = gcc
CFLAGS = -Wall -Wextra -pedantic -O2 -std=c99

test: test.o bdd.o node_vector.o

test.o: test.c bdd.h

bdd.o: bdd.c bdd.h node_vector.h node.h

node_vector.o: node_vector.c node_vector.h bdd.h node.h

.PHONY: clean
clean:
	rm -f *.o test
