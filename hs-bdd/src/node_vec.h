#ifndef NODE_VEC_INCLUDED
#define NODE_VEC_INCLUDED

#include "memory.h"
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
/* Invariants:
 *     capacity > 0
 *     capacity >= num_elems
 *     store != NULL
 */

/* Creates and returns a new node_vec_t. */
extern void
node_vec_create (node_vec_t *vec);

/* Creates and returns a new node_vec_t with the given maximum
 * number of elements. */
extern void
node_vec_create_with_capacity (node_vec_t *vec, unsigned num_elems);

/* Frees the memory used by the given node_vec_t.  It is an error
 * to call this procedure more than once on a node_vec_t. */
extern void
node_vec_destroy (node_vec_t *vec);

/* Gets the number of used elements in the vector. */
static inline unsigned
node_vec_get_num_elems (node_vec_t *vec)
{
    return vec->num_elems;
}

/* Gets the capacity of the vector, i.e., the number of elements for
   which it has allocated space. */
static inline unsigned
node_vec_get_capacity (node_vec_t *vec)
{
    return vec->capacity;
}

/* Gets the element at the specified index.  The index must be less
 * than the number of used elements of the vector. */
static inline node_t
node_vec_get (node_vec_t *vec, unsigned idx)
{
    assert (idx < vec->num_elems);
    return vec->store[idx];
}

/* Sets the element at the specified index to the given value.  The
 * index must be less than the number of used elements of the
 * vector. */
static inline void
node_vec_set (node_vec_t *vec, unsigned idx, node_t val)
{
    assert (idx < vec->num_elems);
    vec->store[idx] = val;
}

extern void
node_vec_double_size (node_vec_t *vec);

/* Appends the given node_t to the vector, doubling the size of the
   vector if it is not large enough. */
static inline void
node_vec_push_back (node_vec_t *vec, node_t val)
{
    if (vec->num_elems == vec->capacity)
        node_vec_double_size (vec);
    vec->num_elems += 1;
    node_vec_set (vec, vec->num_elems - 1, val);
}

#endif /* NODE_VEC_INCLUDED */
