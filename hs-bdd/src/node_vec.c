/* THIS FILE WAS GENERATED FROM A TEMPLATE FILE.  MAKE MODIFICATIONS
TO THE TEMPLATE INSTEAD. */

#include "node_vec.h"

#include <assert.h>
#include <stdlib.h>

static const unsigned INITIAL_CAPACITY = 32;

node_vec_t *
node_vec_create ()
{
    return node_vec_create_with_capacity(INITIAL_CAPACITY);
}

node_vec_t *
node_vec_create_with_capacity (unsigned num_elems)
{
    node_vec_t *vec;
    num_elems = num_elems > 0 ? num_elems : INITIAL_CAPACITY;
    vec = (node_vec_t *)
        malloc (sizeof(node_vec_t));
    vec->store = (node_t *)
        malloc (num_elems * sizeof(node_t));
    vec->capacity = num_elems;
    vec->num_elems = 0;
    node_vec_check_invariants (vec);
    return vec;
}

void
node_vec_destroy (node_vec_t *vec)
{
    if (vec == NULL) return;

    node_vec_check_invariants (vec);
    free (vec->store);
    free (vec);
}

unsigned
node_vec_get_num_elems (node_vec_t *vec)
{
    node_vec_check_invariants (vec);
    return vec->num_elems;
}

unsigned
node_vec_get_capacity (node_vec_t *vec)
{
    node_vec_check_invariants (vec);
    return vec->capacity;
}

static void
double_vector_size (node_vec_t *vec)
{
    unsigned new_capacity = vec->capacity * 2;
    node_vec_check_invariants (vec);
    vec->store = (node_t *)
        realloc (vec->store, new_capacity * sizeof(node_t));
    vec->capacity = new_capacity;
    node_vec_check_invariants (vec);
}

void
node_vec_push_back (node_vec_t *vec, node_t node)
{
    node_vec_check_invariants (vec);
    if (vec->num_elems == vec->capacity)
        double_vector_size (vec);
    vec->num_elems += 1;
    node_vec_set (vec, vec->num_elems - 1, node);
    node_vec_check_invariants (vec);
}
