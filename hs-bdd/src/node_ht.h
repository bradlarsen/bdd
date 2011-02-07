/* THIS FILE WAS GENERATED FROM A TEMPLATE FILE.  MAKE MODIFICATIONS
TO THE TEMPLATE INSTEAD. */

/* This module provides a hash table from node_t to unsigned
 * implemented using separate chaining. */

#ifndef NODE_HT_INCLUDED
#define NODE_HT_INCLUDED

#include "bdd_mgr.h"
#include "memory.h"


/* A hash table implemented using separate chaining. */
typedef struct node_ht_t node_ht_t;

/* Creates and returns a new hash table with a default number of buckets. */
extern node_ht_t *
node_ht_create ();

/* Creates and returns a new hash table with a suggested number of buckets. */
extern node_ht_t *
node_ht_create_with_hint (unsigned num_buckets_hint);

/* Frees the memory used by the given hash table.  It is an error
 * to call this procedure more than once on a hash table. */
extern void
node_ht_destroy (node_ht_t *tab);

/* Gets the number of entries in the hash table. */
extern unsigned
node_ht_get_num_entries (node_ht_t *tab);

/* Gets the number of buckets in the hash table. */
extern unsigned
node_ht_get_num_buckets (node_ht_t *tab);

/* Return the hash table load, defined as the number of entries
 * divided by the number of buckets. */
extern float
node_ht_get_load (node_ht_t *tab);

/* Inserts a binding for the given key and value into the hash table.
 * If there is already an entry with the given key, its value is
 * replaced. */
extern void
node_ht_insert (
    node_ht_t *tab,
    node_t key,
    unsigned val
    );

/* Retrieves a pointer to the value bound to the specified key.  If no
 * such entry exists, NULL is returned.  When any modifying hash table
 * operations are performed upon the table, it is an error to
 * dereference the pointer returned by this function. */
extern unsigned *
node_ht_lookup (
    node_ht_t *tab,
    node_t key
    );

typedef void (* node_ht_map_fun) (void *env,
                                    node_t key,
                                    unsigned *val);

/* Applies a function to each key/value entry. */
extern void
node_ht_map_entries (node_ht_t *tab,
                       void *env,
                       node_ht_map_fun fun);

#endif /* NODE_HT_INCLUDED */
