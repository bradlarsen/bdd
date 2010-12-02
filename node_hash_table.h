#ifndef NODE_HASH_TABLE_INCLUDED
#define NODE_HASH_TABLE_INCLUDED

#include "bdd.h"
#include "node.h"

typedef struct node_hash_table node_hash_table_t;

/* Creates and returns a new node_hash_table_t. */
extern node_hash_table_t *
node_hash_table_create ();

/* Frees the memory used by the given node_hash_table_t.  It is an
 * error to call this procedure more than once on a
 * node_hash_table_t. */
extern void
node_hash_table_destroy (node_hash_table_t *tab);

/* Gets the number of used elements in the hash table. */
extern unsigned
node_hash_table_get_num_elems (node_hash_table_t *tab);

#endif /* NODE_HASH_TABLE_INCLUDED */
