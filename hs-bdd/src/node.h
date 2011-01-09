#ifndef NODE_INCLUDED
#define NODE_INCLUDED

#include "boolean.h"
#include "hash_pair.h"
#include "raw_bdd.h"
#include <stdio.h>

typedef struct
{
    unsigned var;             /* the variable index of the node */
    raw_bdd_t low;            /* the value if the var is false */
    raw_bdd_t high;           /* the value if the var is true */
} node_t;

static inline boolean
node_equal (node_t n1, node_t n2)
{
    return n1.var == n2.var && n1.low == n2.low && n1.high == n2.high;
}

/* The node hash function h from Andersen's ``An introduction to
   binary decision diagrams''. */
static inline unsigned
node_hash (node_t n)
{
    return hash_pair(n.var, hash_pair(n.low, n.high)) % 999999937u;
}

#endif /* NODE_INCLUDED */
