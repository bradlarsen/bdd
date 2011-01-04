#ifndef BDD_INCLUDED
#define BDD_INCLUDED

#include "boolean.h"

/* A BDD is represented by an unsigned integral id. */
struct bdd {
    unsigned id;
};

static inline boolean
bdd_ptr_equal (struct bdd *b1, struct bdd *b2)
{
    return b1->id == b2->id;
}

static inline unsigned
bdd_ptr_hash (struct bdd *b)
{
    return b->id;
}

#endif /* BDD_INCLUDED */
