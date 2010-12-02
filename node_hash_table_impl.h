/* The definition of the node hash table types.  This hash table is
 * implemented using separate chaining. */

#ifndef NODE_HASH_TABLE_DEF_INCLUDED
#define NODE_HASH_TABLE_DEF_INCLUDED

#include "node_hash_table.h"
#include <assert.h>
#include <stdlib.h>

typedef struct ht_bucket
{
    node_t key;
    unsigned value;
    struct ht_bucket *next;
} ht_bucket_t;

/* Allocates and initializes a bucket with the given parameters. */
extern ht_bucket_t *
ht_bucket_create (node_t key, unsigned value, ht_bucket_t *next);

/* Frees the memory used by the list rooted by the bucket. */
extern void
ht_bucket_free (ht_bucket_t *bucket);

/* Performs a linear search down a bucket list for the given key.  A
 * pointer to the bucket with the matching key is returned if one
 * exists, and NULL returned otherwise. */
extern ht_bucket_t *
ht_bucket_search (ht_bucket_t *bucket, node_t key);

struct node_hash_table
{
    unsigned num_entries;    /* the number of entries in the table */
    unsigned num_buckets;    /* the number of elems in the buckets array */
    ht_bucket_t **buckets;   /* the array of buckets */
};

#define node_hash_table_check_invariants(tab)   \
    do {                                        \
        assert (tab != NULL);                   \
        assert (tab->buckets != NULL);          \
    } while (0)

#endif /* NODE_HASH_TABLE_DEF_INCLUDED */
