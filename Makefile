CC = gcc
CFLAGS = -Wall -Wextra -pedantic -std=c99

BDD_PAIR_HASH_TABLE_OBJECTS =			\
	bdd_pair_hash_table_create_destroy.o	\
	bdd_pair_hash_table_bucket.o		\
	bdd_pair_hash_table_accessors.o		\
	bdd_pair_hash_table_lookup_insert.o	\
	bdd_pair_hash_table_hash.o		\
	bdd_pair_hash_table_invariant_check.o	\
	bdd_pair.o				\
	hash_pair.o

NODE_HASH_TABLE_OBJECTS =			\
	node_hash_table_create_destroy.o	\
	node_hash_table_bucket.o		\
	node_hash_table_accessors.o		\
	node_hash_table_lookup_insert.o         \
	node_hash_table_hash.o                  \
	node_hash_table_invariant_check.o	\
	node.o					\
	hash_pair.o

NODE_VECTOR_OBJECTS =				\
	node_vector.o

BDD_OBJECTS =					\
	bdd_create_destroy.o			\
	bdd_accessors.o				\
	bdd_ops.o				\
	bdd_impl.o

test: test.o $(BDD_OBJECTS) $(BDD_PAIR_HASH_TABLE_OBJECTS) $(NODE_HASH_TABLE_OBJECTS) $(NODE_VECTOR_OBJECTS)

.PHONY: debug
debug:
	$(CC) $(CFLAGS) -g *.c -o test_debug

.PHONY: release
release:
	$(CC) $(CFLAGS) -O2 -DNDEBUG *.c -o test_release

.PHONY: clean
clean:
	rm -f *.o test test_debug test_release
