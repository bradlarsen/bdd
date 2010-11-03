CFLAGS = -Wall -Wextra -O -std=c99

test: test.o

test.o: test.c bdd.h

.PHONY: clean
clean:
	rm -f *.o test
