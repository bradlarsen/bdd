CC = gcc
CFLAGS = -Wall -Wextra -pedantic -std=c99 -g

NODE_HASH_TABLE_OBJECTS =			\
	node_hash_table_create_destroy.o	\
	node_hash_table_bucket.o		\
	node_hash_table_accessors.o		\
	node_hash_table_lookup_insert.o         \
	node_hash_table_hash.o                  \
	node_hash_table_invariant_check.o

NODE_VECTOR_OBJECTS =				\
	node_vector.o

test: test.o bdd.o node.o $(NODE_HASH_TABLE_OBJECTS) $(NODE_VECTOR_OBJECTS)

.PHONY: clean
clean:
	rm -f *.o test
