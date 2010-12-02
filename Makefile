CC = gcc
CFLAGS = -Wall -Wextra -pedantic -std=c99 -g

test: test.o bdd.o node_vector.o node_hash_table.o node.o

test.o: test.c bdd.h

bdd.o: bdd.c bdd.h node_vector.h node_hash_table.h node.h

node_vector.o: node_vector.c node_vector.h bdd.h node.h

node_hash_table.o: node_hash_table.c node_hash_table.h bdd.h node.h

node.o : node.c node.h bdd.h

.PHONY: clean
clean:
	rm -f *.o test
