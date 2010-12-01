#include "node_vector.h"

#include <assert.h>
#include <stdlib.h>

static const unsigned INITIAL_CAPACITY = 512;

node_vector_t *
node_vector_create ()
{
    node_vector_t *vec = (node_vector_t *) malloc (sizeof(node_vector_t));
    vec->store = (node_t *) malloc (INITIAL_CAPACITY * sizeof(node_t));
    vec->capacity = 0;
    vec->size = 0;
    return vec;
}

void
node_vector_destroy (node_vector_t *vec)
{
    if (vec == NULL) return;

    free (vec->store);
    free (vec);
}

unsigned
node_vector_get_size (node_vector_t *vec)
{
    assert (vec != NULL);
    return vec->size;
}

static void
double_vector_size (node_vector_t *vec)
{
    assert (vec);
    unsigned new_capacity = vec->capacity * 2;
    vec->store = (node_t *) realloc (vec->store,
                                     new_capacity * sizeof(node_t));
    vec->capacity = new_capacity;
}

void
node_vector_push_back (node_vector_t *vec, node_t node)
{
    assert (vec != NULL);
    if (vec->size == vec->capacity)
        double_vector_size (vec);
    vec->size += 1;
    node_vector_set (vec, vec->size, node);
}
