#include "bdd_triple.h"
#include <vector>
#include <cassert>

struct bdd_triple_vector
{
    std::vector<bdd_triple> *impl;
};

extern "C"
void
bdd_triple_vector_create ( struct bdd_triple_vector *vec )
{
    assert (vec != NULL);
    vec->impl = new std::vector<bdd_triple>();
}

extern "C"
void
bdd_triple_vector_destroy ( struct bdd_triple_vector *vec )
{
    if (vec != NULL && vec->impl != NULL)
        delete vec->impl;
}

extern "C"
void
bdd_triple_vector_add (
        struct bdd_triple_vector *vec,
        struct bdd_triple triple
        )
{
    assert (vec != NULL);
    vec->impl->push_back(triple);
}

extern "C"
struct bdd_triple
bdd_triple_vector_get (
        struct bdd_triple_vector *vec,
        unsigned idx
        )
{
    assert (vec != NULL);
    return (*vec->impl)[idx];
}
