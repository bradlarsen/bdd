/* This module provides a hash-based cache from pairs of unsigned to unsigned. */
#ifndef BDD_ITE_CACHE_INCLUDED
#define BDD_ITE_CACHE_INCLUDED

#include "boolean.h"
#include "hash_pair.h"
#include <limits.h>

typedef struct
{
    unsigned p;
    unsigned t;
    unsigned f;
    unsigned result;
} bdd_ite_cache_entry_t;

typedef struct
{
    unsigned num_entries;       /* always a power of 2 */
    bdd_ite_cache_entry_t *entries;
} bdd_ite_cache_t;

/* FIXME: the sentinel used here is bogus. */

/* Is the given entry free, i.e., not holding actual data? */
static inline boolean
bdd_ite_cache_entry_is_free (bdd_ite_cache_entry_t *entry)
{
    return entry->p == UINT_MAX;
}

/* Initializes a new hash table with a suggested number of entries. */
extern void
bdd_ite_cache_create_with_hint (bdd_ite_cache_t *tab, unsigned num_entries_hint);

/* Frees the memory used by the given cache.  It is an error to call
 * this procedure more than once on a cache. */
extern void
bdd_ite_cache_destroy (bdd_ite_cache_t *tab);

/* Empties the cache, i.e., assigns all entries back to the sentinel value. */
extern void
bdd_ite_cache_clear (bdd_ite_cache_t *tab);

/* Gets the size of the cache. */
static inline unsigned
bdd_ite_cache_get_size (bdd_ite_cache_t *tab)
{
    return tab->num_entries;
}

static inline unsigned
bdd_ite_hash (unsigned p, unsigned t, unsigned f)
{
    return hash_unsigned_pair (p, hash_unsigned_pair (t, f));
}

/* Retrieves a pointer to the entry that the given key hashes to.  The
 * pointed-to entry may or may not correspond to the given key. */
static inline bdd_ite_cache_entry_t *
bdd_ite_cache_lookup (
    bdd_ite_cache_t *tab,
    unsigned p,
    unsigned t,
    unsigned f
    )
{
    return &tab->entries[bdd_ite_hash(p, t, f) & (tab->num_entries - 1)];
}

#endif /* BDD_ITE_CACHE_INCLUDED */
