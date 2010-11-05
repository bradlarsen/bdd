#ifndef BDD_TRIPLE_VECTOR_INCLUDED
#define BDD_TRIPLE_VECTOR_INCLUDED

#include "bdd_triple.h"

struct bdd_triple_vector;

extern void
bdd_triple_vector_create ( struct bdd_triple_vector *vec );

extern void
bdd_triple_vector_destroy ( struct bdd_triple_vector *vec );

extern void
bdd_triple_vector_add (
        struct bdd_triple_vector *vec,
        struct bdd_triple triple
        );

extern struct bdd_triple
bdd_triple_vector_get (
        struct bdd_triple_vector *vec,
        unsigned idx
        );

#endif  /* BDD_TRIPLE_VECTOR_INCLUDED */
