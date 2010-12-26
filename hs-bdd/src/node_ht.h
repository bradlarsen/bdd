/* This module provides a hash table from node_t to bdd_t
 * implemented using separate chaining. */

#ifndef NODE_HT_INCLUDED
#define NODE_HT_INCLUDED

#include <assert.h>
#include <stdlib.h>

#include "node.h"
#include "node_ht_bucket.h"

#define NODE_HT_MAX_LOAD 0.75f

/* A hash table implemented using separate chaining. */
typedef struct
{
    /* the number of entries in the table */
    unsigned num_entries;
    /* the number of elems in the buckets array */
    unsigned num_buckets;
    /* the array of buckets */
    ht_bucket_t **buckets;
    /* memory pool for buckets */
    ht_bucket_pool_t pool;
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
node_ht_get_hash_index (node_ht_t *tab, node_t key)
{
    return node_hash (key) % tab->num_buckets;
}

/* Inserts a binding for the given key and value into the hash table.
 * If there is already an entry with the given key, its value is
 * replaced. */
static inline void
node_ht_insert (node_ht_t *tab,
                node_t key,
                bdd_t val)
{
    extern void double_hash_table_num_buckets (node_ht_t *tab);

    unsigned b_idx;
    ht_bucket_t *b;

    if (node_ht_get_load(tab) >= NODE_HT_MAX_LOAD)
        double_hash_table_num_buckets (tab);
    b_idx = node_ht_get_hash_index (tab, key);
    b = ht_bucket_search (tab->buckets[b_idx], key);
    if (b == NULL) {
        tab->buckets[b_idx] =
            ht_bucket_create (&tab->pool, key, val, tab->buckets[b_idx]);
        tab->num_entries += 1;
    }
    else
        b->value = val;
}

/* Retrieves a pointer to the value bound to the specified key.  If no
 * such entry exists, NULL is returned.  When any modifying hash table
 * operations are performed upon the table, it is an error to
 * dereference the pointer returned by this function. */
static inline bdd_t *
node_ht_lookup (node_ht_t *tab, node_t key)
{
    unsigned b_idx;
    ht_bucket_t *b;

    b_idx = node_ht_get_hash_index (tab, key);
    b = ht_bucket_search (tab->buckets[b_idx], key);
    return b != NULL ? &b->value : NULL;
}

#endif /* NODE_HT_INCLUDED */
