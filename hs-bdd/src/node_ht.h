/* This module provides a hash table from node_t to raw_bdd_t
 * implemented using linear probing. */

#ifndef NODE_HT_INCLUDED
#define NODE_HT_INCLUDED

#include <assert.h>
#include <stdlib.h>

#include "node.h"
#include "boolean.h"

#define NODE_HT_MAX_LOAD 0.75f

/* A node_t/raw_bdd_t entry in the following hash table. */
typedef struct
{
    node_t key;
    raw_bdd_t val;
} node_ht_entry_t;

static inline boolean
node_ht_entry_is_unoccupied (node_ht_entry_t *entry)
{
    assert (entry != NULL);
    return entry->key.low == 0 && entry->key.high == 0;
}

/* A hash table implemented using linear probing for collision
 * resolution. */
typedef struct
{
    /* the number of entries in the table */
    unsigned num_entries;
    /* the size in elements of the array */
    unsigned max_num_entries;
    /* the array of entries */
    node_ht_entry_t *store;
} node_ht_t;

/* Creates and returns a new hash table with a default size. */
extern void
node_ht_create (node_ht_t *tab);

/* Creates and returns a new hash table with a suggested size. */
extern void
node_ht_create_with_hint (node_ht_t *tab, unsigned capacity_hint);

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

/* Gets the capacity in number of entries of the hash table. */
static inline unsigned
node_ht_get_max_num_entries (node_ht_t *tab)
{
    return tab->max_num_entries;
}

/* Return the hash table load, defined as the number of entries
 * divided by the number of buckets. */
static inline float
node_ht_get_load (node_ht_t *tab)
{
    return
        (float) node_ht_get_num_entries (tab) /
        (float) node_ht_get_max_num_entries (tab);
}

/* Returns a pointer to the entry where the given key should go. */
static inline node_ht_entry_t *
node_ht_lookup (node_ht_t *tab, node_t key)
{
    unsigned i;
    assert (key.low != key.high);
    i = node_hash (key) % tab->max_num_entries;
    while (1) {
        assert (i < tab->max_num_entries);
        if (node_ht_entry_is_unoccupied(&tab->store[i]))
            return &tab->store[i];
        else if (node_equal(tab->store[i].key, key))
            return &tab->store[i];
        i = (i + 1) % tab->max_num_entries;
    }
}

extern void
node_ht_double_size (node_ht_t *tab);

static inline void
node_ht_insert (node_ht_t *tab,
                node_ht_entry_t *pos,
                node_t key,
                raw_bdd_t val)
{
    assert (key.low != key.high);
    if (node_ht_entry_is_unoccupied(pos)) {
        pos->key = key;
        pos->val = val;
        tab->num_entries += 1;
        if (node_ht_get_load(tab) >= NODE_HT_MAX_LOAD)
            node_ht_double_size (tab);
    }
    else {
        assert( node_equal(pos->key, key) );
        pos->val = val;
    }
}

#endif /* NODE_HT_INCLUDED */
