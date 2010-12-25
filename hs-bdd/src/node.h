#ifndef NODE_INCLUDED
#define NODE_INCLUDED

#include "bdd.h"
#include "hash_pair.h"
#include <stdio.h>

typedef struct
{
    unsigned var;             /* the variable index of the node */
    bdd_t low;                /* the value if the var is false */
    bdd_t high;               /* the value if the var is true */

    /* invariant: low != high */
    /* invariant: !bdd_is_complement(high) */
} node_t;

#define node_check_invariants(n)                                        \
    do {                                                                \
        if (bdd_to_idx((n).var) > 1) assert( (n).low != (n).high );     \
        assert( !bdd_is_complement( (n).high ) );                       \
    } while (0)

static inline bool
node_equal (node_t n1, node_t n2)
{
    return n1.var == n2.var && n1.low == n2.low && n1.high == n2.high;
}

/* The node hash function h from Andersen's ``An introduction to
   binary decision diagrams''. */
static inline unsigned
node_hash (node_t n)
{
    return hash_pair(n.var, hash_pair(n.low, n.high)) % 15485863;
}

static inline node_t
node_negate (node_t n)
{
    n.low = -n.low;
    n.high = -n.high;
    return n;
}

#endif /* NODE_INCLUDED */
