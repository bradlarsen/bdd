BDD_LIB_SOURCES =				\
	bdd_accessors.c				\
	bdd_create_destroy.c			\
	bdd_impl.c				\
	bdd_ops.c				\
	bdd_pair.c				\
	bdd_pair_ht.c				\
	hash_pair.c				\
	node.c					\
	node_ht.c				\
	node_vec.c

CC = gcc
CFLAGS = -Wall -Wextra -pedantic -std=c99
DEBUGFLAGS = -g
RELEASEFLAGS = -O2 -DNDEBUG

%.debug.o: %.c
	$(CC) $(CFLAGS) $(DEBUGFLAGS) -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) $(RELEASEFLAGS) -c -o $@ $<

BDD_LIB_OBJECTS = $(BDD_LIB_SOURCES:.c=.o)
BDD_DEBUG_LIB_OBJECTS = $(BDD_LIB_SOURCES:.c=.debug.o)

.PHONY: all
all: test test.debug bdd.a bdd.debug.a

test: test.o bdd.a

test.debug: test.debug.o bdd.debug.a

bdd.a: $(BDD_LIB_OBJECTS)
	$(AR) rcs $@ $^

bdd.debug.a: $(BDD_DEBUG_LIB_OBJECTS)
	$(AR) rcs $@ $^

.PHONY: clean
clean:
	rm -f *.o *.a test test.debug
