#ifndef HASH_PAIR_INCLUDED
#define HASH_PAIR_INCLUDED

/* The pair hash function from Andersen's ``An introduction to binary
 * decision diagrams''. */
#define hash_pair(i, j) \
    ((((i) + (j)) * ((i) + (j) + 1) / 2) + (i))

#endif /* HASH_PAIR_INCLUDED */
