#ifndef BDD_PAIR_INCLUDED
#define BDD_PAIR_INCLUDED

#include "bdd.h"
#include "hash_pair.h"
#include <stdbool.h>

typedef struct bdd_pair
{
    bdd_t first;
    bdd_t second;
} bdd_pair_t;

inline bool
bdd_pair_equal (bdd_pair_t p1, bdd_pair_t p2)
{
    return p1.first == p2.first && p1.second == p2.second;
}

inline unsigned
bdd_pair_hash (bdd_pair_t p)
{
    return hash_pair (p.first, p.second);
}

#endif /* BDD_PAIR_INCLUDED */
