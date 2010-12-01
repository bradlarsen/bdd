#ifndef NODE_INCLUDED
#define NODE_INCLUDED

#include "bdd.h"

typedef struct
{
    unsigned var;             /* the variable index of the node */
    bdd low;                  /* the value if the var is false */
    bdd high;                 /* the value if the var is true */

    /* invariant: low != high */
} node_t;

#endif /* NODE_INCLUDED */
