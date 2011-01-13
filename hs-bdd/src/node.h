#ifndef NODE_INCLUDED
#define NODE_INCLUDED

#include "boolean.h"
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

#endif /* NODE_INCLUDED */
