#ifndef HASH_PAIR_INCLUDED
#define HASH_PAIR_INCLUDED

/* The pair hash function from Andersen's ``An introduction to binary
 * decision diagrams''. */
static inline unsigned
hash_unsigned_pair (unsigned i, unsigned j)
{
    return ((i + j) * (i + j + 1u) / 2u) + i;
}

#endif /* HASH_PAIR_INCLUDED */
