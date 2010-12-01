#ifndef NODE_VECTOR_INCLUDED
#define NODE_VECTOR_INCLUDED

#include "bdd.h"
#include "node.h"

#include <assert.h>
#include <stdlib.h>

typedef struct
{
    node_t *store;        /* memory for elements */
    unsigned capacity;    /* total number of elements */
    unsigned size;        /* number of used elements,
                             i.e., index of first free element */

    /* invariant: capacity >= used */
} node_vector_t;

/* Creates and returns a new node_vector_t. */
extern node_vector_t *
node_vector_create ();

/* Frees the memory used by the given node_vector_t.  It is an error
 * to call this procedure more than once on a node_vector_t. */
extern void
node_vector_destroy (node_vector_t *vec);

/* Gets the number of used elements in the vector. */
extern unsigned
node_vector_get_size (node_vector_t *vec);

/* Gets the element at the specified index.  The index must be less
 * than the number of used elements of the vector. */
inline node_t
node_vector_get (node_vector_t *vec, unsigned idx)
{
    assert (vec != NULL);
    assert (idx < vec->size);
    return vec->store[idx];
}

/* Sets the element at the specified index to the given value.  The
 * index must be less than the number of used elements of the
 * vector. */
inline void
node_vector_set (node_vector_t *vec, unsigned idx, node_t node)
{
    assert (vec != NULL);
    assert (idx < vec->size);
    vec->store[idx] = node;
}

#endif /* NODE_VECTOR_INCLUDED */
