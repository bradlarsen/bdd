#include "node_vector.h"

#include <assert.h>
#include <stdlib.h>

static const unsigned INITIAL_CAPACITY = 32;

extern inline void
node_vector_check_invariants (node_vector_t *vec);

extern inline node_t
node_vector_get (node_vector_t *vec, unsigned idx);

extern inline void
node_vector_set (node_vector_t *vec, unsigned idx, node_t val);


node_vector_t *
node_vector_create ()
{
    node_vector_t *vec = (node_vector_t *) malloc (sizeof(node_vector_t));
    vec->store = (node_t *) malloc (INITIAL_CAPACITY * sizeof(node_t));
    vec->capacity = INITIAL_CAPACITY;
    vec->num_elems = 0;
    node_vector_check_invariants (vec);
    return vec;
}

void
node_vector_destroy (node_vector_t *vec)
{
    if (vec == NULL) return;

    node_vector_check_invariants (vec);
    free (vec->store);
    free (vec);
}

unsigned
node_vector_get_num_elems (node_vector_t *vec)
{
    node_vector_check_invariants (vec);
    return vec->num_elems;
}

static void
double_vector_size (node_vector_t *vec)
{
    node_vector_check_invariants (vec);
    unsigned new_capacity = vec->capacity * 2;
    vec->store = (node_t *) realloc (vec->store,
                                     new_capacity * sizeof(node_t));
    vec->capacity = new_capacity;
    node_vector_check_invariants (vec);
}

void
node_vector_push_back (node_vector_t *vec, node_t node)
{
    node_vector_check_invariants (vec);
    if (vec->num_elems == vec->capacity)
        double_vector_size (vec);
    const unsigned idx = vec->num_elems;
    vec->num_elems += 1;
    node_vector_set (vec, idx, node);
    node_vector_check_invariants (vec);
}
