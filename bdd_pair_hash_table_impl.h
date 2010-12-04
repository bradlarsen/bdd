/* The definition of the bdd_pair hash table types.  This hash table is
 * implemented using separate chaining. */

#ifndef BDD_PAIR_HASH_TABLE_DEF_INCLUDED
#define BDD_PAIR_HASH_TABLE_DEF_INCLUDED

#include "bdd_pair_hash_table.h"
#include "hash_pair.h"
#include <assert.h>
#include <stdlib.h>


typedef struct bdd_pair_bucket
{
    bdd_pair_t key;
    unsigned value;
    struct bdd_pair_bucket *next;
} bdd_pair_bucket_t;

/* Allocates and initializes a bucket with the given parameters. */
extern bdd_pair_bucket_t *
bdd_pair_bucket_create (bdd_pair_t key, unsigned value, bdd_pair_bucket_t *next);

/* Frees the memory used by the list rooted by the bucket. */
extern void
bdd_pair_bucket_free (bdd_pair_bucket_t *bucket);

/* Performs a linear search down a bucket list for the given key.  A
 * pointer to the bucket with the matching key is returned if one
 * exists, and NULL returned otherwise. */
extern bdd_pair_bucket_t *
bdd_pair_bucket_search (bdd_pair_bucket_t *bucket, bdd_pair_t key);


struct bdd_pair_hash_table
{
    unsigned num_entries;    /* the number of entries in the table */
    unsigned num_buckets;    /* the number of elems in the buckets array */
    bdd_pair_bucket_t **buckets;   /* the array of buckets */
};

#define bdd_pair_hash_table_check_invariants(tab)               \
    do {                                                        \
        assert (tab != NULL);                                   \
        assert (tab->buckets != NULL);                          \
        assert (bdd_pair_hash_table_proper_hash_values(tab));   \
        assert (bdd_pair_hash_table_no_duplicate_keys(tab));    \
    } while (0)

/* Returns true if and only if every entry in a bucket hashes to that
 * bucket. */
extern bool
bdd_pair_hash_table_proper_hash_values (bdd_pair_hash_table_t *tab);

/* Returns true if and only if there are no two distinct entries with
 * the same key. */
extern bool
bdd_pair_hash_table_no_duplicate_keys (bdd_pair_hash_table_t *tab);


/* HASHING FUNCTIONS */

inline unsigned
bdd_pair_hash_table_hash_bdd_pair (bdd_pair_t pair)
{
    return hash_pair(pair.first, pair.second) % 15485863;
}

inline unsigned
bdd_pair_hash_table_get_hash_index (bdd_pair_hash_table_t *tab, bdd_pair_t key)
{
    return bdd_pair_hash_table_hash_bdd_pair (key) % tab->num_buckets;
}

#endif /* BDD_PAIR_HASH_TABLE_DEF_INCLUDED */
