CC = gcc
CFLAGS = -Wall -Wextra -pedantic -O2 -std=c99

test: test.o bdd.o node_vector.o node_hash_table.o

test.o: test.c bdd.h

bdd.o: bdd.c bdd.h node_vector.h node_hash_table.h node.h

node_vector.o: node_vector.c node_vector.h bdd.h node.h

node_hash_table.o: node_hash_table.c node_hash_table.h bdd.h node.h

.PHONY: clean
clean:
	rm -f *.o test
