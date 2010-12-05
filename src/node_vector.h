#ifndef NODE_VECTOR_INCLUDED
#define NODE_VECTOR_INCLUDED

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
} node_vector_t;

#define node_vector_check_invariants(vec)                 \
    do {                                                \
        assert (vec);                                   \
        assert (vec->capacity > 0);                     \
        assert (vec->capacity >= vec->num_elems);       \
        assert (vec->store != NULL);                    \
    } while (0)

/* Creates and returns a new node_vector_t. */
extern node_vector_t *
node_vector_create ();

/* Creates and returns a new node_vector_t with the given maximum
 * number of elements. */
extern node_vector_t *
node_vector_create_with_capacity (unsigned num_elems);

/* Frees the memory used by the given node_vector_t.  It is an error
 * to call this procedure more than once on a node_vector_t. */
extern void
node_vector_destroy (node_vector_t *vec);

/* Gets the number of used elements in the vector. */
extern unsigned
node_vector_get_num_elems (node_vector_t *vec);

/* Gets the capacity of the vector, i.e., the number of elements for
   which it has allocated space. */
extern unsigned
node_vector_get_capacity (node_vector_t *vec);

/* Gets the element at the specified index.  The index must be less
 * than the number of used elements of the vector. */
inline node_t
node_vector_get (node_vector_t *vec, unsigned idx)
{
    node_vector_check_invariants (vec);
    assert (idx < vec->num_elems);
    return vec->store[idx];
}

/* Sets the element at the specified index to the given value.  The
 * index must be less than the number of used elements of the
 * vector. */
inline void
node_vector_set (node_vector_t *vec, unsigned idx, node_t val)
{
    node_vector_check_invariants (vec);
    assert (idx < vec->num_elems);
    vec->store[idx] = val;
}

/* Appends the given node_t to the vector, doubling the size of the
   vector if it is not large enough. */
extern void
node_vector_push_back (node_vector_t *vec, node_t val);

#endif /* NODE_VECTOR_INCLUDED */
