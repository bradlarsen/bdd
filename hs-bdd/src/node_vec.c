#include "node_vec.h"

#include <assert.h>
#include <stdlib.h>

static const unsigned INITIAL_CAPACITY = 32;

void
node_vec_create (node_vec_t *vec)
{
    node_vec_create_with_capacity(vec, INITIAL_CAPACITY);
}

void
node_vec_create_with_capacity (node_vec_t *vec, unsigned num_elems)
{
    num_elems = num_elems > 0 ? num_elems : INITIAL_CAPACITY;
    vec->store = (node_t *)
        checked_malloc (num_elems * sizeof(node_t));
    vec->capacity = num_elems;
    vec->num_elems = 0;
}

void
node_vec_destroy (node_vec_t *vec)
{
    free (vec->store);
}

void
node_vec_double_size (node_vec_t *vec)
{
    unsigned new_capacity = vec->capacity * 2;
    vec->store = (node_t *)
        checked_realloc (vec->store, new_capacity * sizeof(node_t));
    vec->capacity = new_capacity;
}
