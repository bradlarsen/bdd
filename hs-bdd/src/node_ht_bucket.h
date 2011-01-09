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
    /* FIXME: make this leaner. */
    /* A pointer into the node vector could be used instead of a
     * node_t.  The only complication will be handling node vector
     * resizing. */
    node_t key;
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
                  node_t key,
                  raw_bdd_t value,
                  node_ht_bucket_t *next)
{
    node_ht_bucket_t *bucket = node_ht_bucket_pool_malloc (pool);
    bucket->key = key;
    bucket->value = value;
    bucket->next = next;
    return bucket;
}

/* Performs a linear search down a bucket list for the given key.  A
 * pointer to the bucket with the matching key is returned if one
 * exists, and NULL returned otherwise. */
static inline node_ht_bucket_t *
node_ht_bucket_search (node_ht_bucket_t *bucket, node_t key)
{
    /* TODO: improve the performance of this hash table.
     * Profiling reveals that some ~50% of the total runtime of the
     * n-queens program is spent searching through node hash table
     * buckets (!?).  Perhaps open addressing (e.g., linear probing)
     * would work better. */
    node_ht_bucket_t *p = bucket;
    while (p != NULL) {
        if (node_equal(p->key, key))
            break;
        p = p->next;
    }
    return p;
}

#endif /* NODE_HT_BUCKET_INCLUDED */
