/* This module provides a hash table from node_t to raw_bdd_t
 * implemented using separate chaining. */

#ifndef NODE_HT_INCLUDED
#define NODE_HT_INCLUDED

#include <assert.h>
#include <stdlib.h>

#include "node.h"
#include "hash_pair.h"
#include "node_ht_bucket.h"

#define NODE_HT_MAX_LOAD 3.00f

/* A hash table implemented using separate chaining. */
typedef struct
{
    /* the number of entries in the table */
    unsigned num_entries;
    /* the number of elems in the buckets array */
    unsigned num_buckets;
    /* the array of buckets */
    node_ht_bucket_t **buckets;
    /* memory pool for buckets */
    node_ht_bucket_pool_t pool;
} node_ht_t;

/* Creates and returns a new hash table with a default number of buckets. */
extern void
node_ht_create (node_ht_t *tab);

/* Creates and returns a new hash table with a suggested number of buckets. */
extern void
node_ht_create_with_hint (node_ht_t *tab, unsigned num_buckets_hint);

/* Frees the memory used by the given hash table.  It is an error
 * to call this procedure more than once on a hash table. */
extern void
node_ht_destroy (node_ht_t *tab);

/* Gets the number of entries in the hash table. */
static inline unsigned
node_ht_get_num_entries (node_ht_t *tab)
{
    return tab->num_entries;
}

/* Gets the number of buckets in the hash table. */
static inline unsigned
node_ht_get_num_buckets (node_ht_t *tab)
{
    return tab->num_buckets;
}

/* Return the hash table load, defined as the number of entries
 * divided by the number of buckets. */
static inline float
node_ht_get_load (node_ht_t *tab)
{
    return
        (float) node_ht_get_num_entries (tab) /
        (float) node_ht_get_num_buckets (tab);
}

/* Hashes the given key and computes the corresponding index for
 * it. */
static inline unsigned
node_ht_get_hash_index (node_ht_t *tab, raw_bdd_t key_low, raw_bdd_t key_high)
{
    unsigned idx = hash_pair (key_low, key_high) % 999999937u % tab->num_buckets;
    assert (idx < tab->num_buckets);
    return idx;
}

/* Eliminates all entries from the table. */
static inline void
node_ht_clear (node_ht_t *tab)
{
    unsigned i;
    node_ht_bucket_pool_reset (&tab->pool);
    tab->num_entries = 0;
    for (i = 0; i < tab->num_buckets; i += 1)
        tab->buckets[i] = NULL;
}

extern void
node_ht_double_num_buckets (node_ht_t *tab);

/* Inserts a binding for the given key and value into the hash table.
 * Existing values are not searched for, so calling this with the same
 * key/value inputs multiple times will result in multiple hash table
 * entries. */
static inline void
node_ht_insert (node_ht_t *tab,
                raw_bdd_t key_low,
                raw_bdd_t key_high,
                raw_bdd_t val)
{
    unsigned b_idx;
    if (node_ht_get_load(tab) >= NODE_HT_MAX_LOAD)
        node_ht_double_num_buckets (tab);
    b_idx = node_ht_get_hash_index (tab, key_low, key_high);
    assert (node_ht_bucket_search (tab->buckets[b_idx], key_low, key_high) == NULL);

    tab->buckets[b_idx] =
        node_ht_bucket_create (&tab->pool,
                               key_low,
                               key_high,
                               val,
                               tab->buckets[b_idx]);
    tab->num_entries += 1;
}

/* Retrieves a pointer to the value bound to the specified key.  If no
 * such entry exists, NULL is returned.  When any modifying hash table
 * operations are performed upon the table, it is an error to
 * dereference the pointer returned by this function. */
static inline raw_bdd_t *
node_ht_lookup (node_ht_t *tab, raw_bdd_t key_low, raw_bdd_t key_high)
{
    unsigned b_idx;
    node_ht_bucket_t *b;

    b_idx = node_ht_get_hash_index (tab, key_low, key_high);
    b = node_ht_bucket_search (tab->buckets[b_idx], key_low, key_high);
    return b != NULL ? &b->value : NULL;
}

#endif /* NODE_HT_INCLUDED */
