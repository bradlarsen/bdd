/* THIS FILE WAS GENERATED FROM A TEMPLATE FILE.  MAKE MODIFICATIONS
TO THE TEMPLATE INSTEAD. */

#ifndef NODE_VEC_INCLUDED
#define NODE_VEC_INCLUDED

#include "node.h"

#include <assert.h>
#include <stdlib.h>

typedef struct
{
    /* memory for elements */
    node_t *store;
    /* total number of allocated elements */
    unsigned capacity;
    /* number of used elements, i.e., index of first free element */
    unsigned num_elems;
} node_vec_t;

#define node_vec_check_invariants(vec)                 \
    do {                                                \
        assert (vec);                                   \
        assert (vec->capacity > 0);                     \
        assert (vec->capacity >= vec->num_elems);       \
        assert (vec->store != NULL);                    \
    } while (0)

/* Creates and returns a new node_vec_t. */
extern node_vec_t *
node_vec_create ();

/* Creates and returns a new node_vec_t with the given maximum
 * number of elements. */
extern node_vec_t *
node_vec_create_with_capacity (unsigned num_elems);

/* Frees the memory used by the given node_vec_t.  It is an error
 * to call this procedure more than once on a node_vec_t. */
extern void
node_vec_destroy (node_vec_t *vec);

/* Gets the number of used elements in the vector. */
static inline unsigned
node_vec_get_num_elems (node_vec_t *vec)
{
    node_vec_check_invariants (vec);
    return vec->num_elems;
}

/* Gets the capacity of the vector, i.e., the number of elements for
   which it has allocated space. */
static inline unsigned
node_vec_get_capacity (node_vec_t *vec)
{
    node_vec_check_invariants (vec);
    return vec->capacity;
}

/* Gets the element at the specified index.  The index must be less
 * than the number of used elements of the vector. */
static inline node_t
node_vec_get (node_vec_t *vec, unsigned idx)
{
    node_vec_check_invariants (vec);
    assert (idx < vec->num_elems);
    return vec->store[idx];
}

/* Sets the element at the specified index to the given value.  The
 * index must be less than the number of used elements of the
 * vector. */
static inline void
node_vec_set (node_vec_t *vec, unsigned idx, node_t val)
{
    node_vec_check_invariants (vec);
    assert (idx < vec->num_elems);
    node_check_invariants (val);
    vec->store[idx] = val;
}

/* Appends the given node_t to the vector, doubling the size of the
   vector if it is not large enough. */
extern void
node_vec_push_back (node_vec_t *vec, node_t val);

#endif /* NODE_VEC_INCLUDED */
