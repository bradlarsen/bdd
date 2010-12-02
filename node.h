#ifndef NODE_INCLUDED
#define NODE_INCLUDED

#include "bdd.h"

#include <stdbool.h>

typedef struct
{
    unsigned var;             /* the variable index of the node */
    bdd low;                  /* the value if the var is false */
    bdd high;                 /* the value if the var is true */

    /* invariant: low != high */
} node_t;

inline bool
node_equal (node_t n1, node_t n2)
{
    return n1.var == n2.var && n1.low == n2.low && n1.high == n2.high;
}

#endif /* NODE_INCLUDED */
