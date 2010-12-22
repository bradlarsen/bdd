/* This module provides a size-limited cache from bdd_pair_t to bdd_t
 * implemented using hashing. */

#ifndef BDD_PAIR_CACHE_INCLUDED
#define BDD_PAIR_CACHE_INCLUDED

#include "bdd_pair.h"

typedef struct
{
    bdd_pair_t key;
    bdd_t value;
} cache_entry_t;

typedef struct
{
    unsigned num_entries;
    cache_entry_t *entries;
} bdd_pair_cache_t;

/* Creates and returns a new hash table with a suggested number of entries. */
extern bdd_pair_cache_t
bdd_pair_cache_create_with_hint (unsigned num_entries_hint);

/* Frees the memory used by the given cache.  It is an error to call
 * this procedure more than once on a cache. */
extern void
bdd_pair_cache_destroy (bdd_pair_cache_t tab);

/* Gets the size of the cache. */
static inline unsigned
bdd_pair_cache_get_size (bdd_pair_cache_t tab)
{
    return tab.num_entries;
}

/* Retrieves a pointer to the entry that the given key hashes to.  The
 * pointed-to entry may or may not correspond to the given key. */
static inline cache_entry_t *
bdd_pair_cache_lookup (bdd_pair_cache_t tab, bdd_pair_t key)
{
    return &tab.entries[bdd_pair_hash(key) % tab.num_entries];
}

#endif /* BDD_PAIR_CACHE_INCLUDED */
