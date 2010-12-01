#include "node_vector.h"

#include <assert.h>
#include <stdlib.h>

node_vector_t *
node_vector_create ()
{
    node_vector_t *vec = (node_vector_t *) malloc (sizeof(node_vector_t));
    vec->store = NULL;
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
