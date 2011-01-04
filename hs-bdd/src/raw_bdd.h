#ifndef RAW_BDD_INCLUDED
#define RAW_BDD_INCLUDED

#include "boolean.h"

typedef int raw_bdd_t;
enum {raw_bdd_false = 0, raw_bdd_true = 1};

static inline boolean
raw_bdd_equal (raw_bdd_t b1, raw_bdd_t b2)
{
    return b1 == b2;
}

static inline unsigned
raw_bdd_hash (raw_bdd_t b)
{
    return (unsigned) b;
}

#endif /* RAW_BDD_INCLUDED */
