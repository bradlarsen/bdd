#ifndef NODE_HT_BUCKET_INCLUDED
#define NODE_HT_BUCKET_INCLUDED

#include "memory.h"
#include "node.h"
#include <stdlib.h>

/***********************************************************************/
/* HASH TABLE BUCKETS                                                  */
/***********************************************************************/
typedef struct node_ht_bucket
{
    raw_bdd_t key_low;
    raw_bdd_t key_high;
    raw_bdd_t value;
    struct node_ht_bucket *next;
} node_ht_bucket_t;

/***********************************************************************/
/* HASH TABLE BUCKET MEMORY POOL                                       */
/***********************************************************************/
typedef struct
{
    unsigned num_entries;
    unsigned capacity;
    node_ht_bucket_t *entries;
} node_ht_bucket_pool_t;

static inline void
node_ht_bucket_pool_create (node_ht_bucket_pool_t *pool, unsigned capacity)
{
    assert (pool != NULL);
    pool->num_entries = 0;
    pool->capacity = capacity;
    pool->entries = (node_ht_bucket_t *)
        checked_malloc (sizeof(node_ht_bucket_t) * capacity);
}

static inline void
node_ht_bucket_pool_destroy (node_ht_bucket_pool_t *pool)
{
    assert (pool != NULL);
    checked_free (pool->entries);
}

static inline node_ht_bucket_t *
node_ht_bucket_pool_malloc (node_ht_bucket_pool_t *pool)
{
    assert (pool != NULL);
    assert (pool->num_entries < pool->capacity);
    pool->num_entries += 1;
    return &pool->entries[pool->num_entries - 1];
}

static inline void
node_ht_bucket_pool_reset (node_ht_bucket_pool_t *pool)
{
    pool->num_entries = 0;
}

static inline node_ht_bucket_t *
node_ht_bucket_create (node_ht_bucket_pool_t *pool,
                       raw_bdd_t key_low,
                       raw_bdd_t key_high,
                       raw_bdd_t value,
                       node_ht_bucket_t *next)
{
    node_ht_bucket_t *bucket = node_ht_bucket_pool_malloc (pool);
    bucket->key_low = key_low;
    bucket->key_high = key_high;
    bucket->value = value;
    bucket->next = next;
    return bucket;
}

/* Performs a linear search down a bucket list for the given key.  A
 * pointer to the bucket with the matching key is returned if one
 * exists, and NULL returned otherwise. */
static inline node_ht_bucket_t *
node_ht_bucket_search (node_ht_bucket_t *bucket,
                       raw_bdd_t key_low,
                       raw_bdd_t key_high)
{
    /* TODO: improve the performance of this hash table.
     * Profiling reveals that some ~50% of the total runtime of the
     * n-queens program is spent searching through node hash table
     * buckets (!?).  Perhaps open addressing (e.g., linear probing)
     * would work better. */
    node_ht_bucket_t *p = bucket;
    while (p != NULL) {
        if (key_low == p->key_low && key_high == p->key_high)
            break;
        p = p->next;
    }
    return p;
}

#endif /* NODE_HT_BUCKET_INCLUDED */
