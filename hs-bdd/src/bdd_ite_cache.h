/* This module provides a size-limited cache from bdd_pair_t to bdd_t
 * implemented using hashing. */

#ifndef BDD_ITE_CACHE_INCLUDED
#define BDD_ITE_CACHE_INCLUDED

#include "boolean.h"
#include "raw_bdd.h"
#include "hash_pair.h"
#include <limits.h>

typedef struct
{
    raw_bdd_t p;
    raw_bdd_t t;
    raw_bdd_t f;
    raw_bdd_t result;
} bdd_ite_cache_entry_t;

typedef struct
{
    unsigned num_entries;
    bdd_ite_cache_entry_t *entries;
} bdd_ite_cache_t;

/* FIXME: the sentinel used here is bogus. */

/* Is the given entry free, i.e., not holding actual data? */
static inline boolean
bdd_ite_cache_entry_is_free (bdd_ite_cache_entry_t *entry)
{
    return entry->p == INT_MAX;
}

/* Initializes a new hash table with a suggested number of entries. */
extern void
bdd_ite_cache_create_with_hint (bdd_ite_cache_t *tab, unsigned num_entries_hint);

/* Frees the memory used by the given cache.  It is an error to call
 * this procedure more than once on a cache. */
extern void
bdd_ite_cache_destroy (bdd_ite_cache_t *tab);

/* Gets the size of the cache. */
static inline unsigned
bdd_ite_cache_get_size (bdd_ite_cache_t *tab)
{
    return tab->num_entries;
}

static inline unsigned
bdd_ite_hash (raw_bdd_t p, raw_bdd_t t, raw_bdd_t f)
{
    return (unsigned) hash_pair (p, hash_pair (t, f)) % 999999937u;
}

/* Retrieves a pointer to the entry that the given key hashes to.  The
 * pointed-to entry may or may not correspond to the given key. */
static inline bdd_ite_cache_entry_t *
bdd_ite_cache_lookup (
    bdd_ite_cache_t *tab,
    raw_bdd_t p,
    raw_bdd_t t,
    raw_bdd_t f
    )
{
    return &tab->entries[bdd_ite_hash(p, t, f) % tab->num_entries];
}

#endif /* BDD_ITE_CACHE_INCLUDED */
