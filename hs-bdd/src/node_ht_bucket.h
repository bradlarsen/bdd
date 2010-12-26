#ifndef NODE_HT_BUCKET_INCLUDED
#define NODE_HT_BUCKET_INCLUDED

#include "node.h"
#include <stdlib.h>

/***********************************************************************/
/* HASH TABLE BUCKETS                                                  */
/***********************************************************************/
typedef struct ht_bucket
{
    node_t key;
    bdd_t value;
    struct ht_bucket *next;
} ht_bucket_t;

/***********************************************************************/
/* HASH TABLE BUCKET MEMORY POOL                                       */
/***********************************************************************/
typedef struct
{
    unsigned num_entries;
    unsigned capacity;
    ht_bucket_t *entries;
} ht_bucket_pool_t;

static inline void
ht_bucket_pool_create (ht_bucket_pool_t *pool, unsigned capacity)
{
    assert (pool != NULL);
    pool->num_entries = 0;
    pool->capacity = capacity;
    pool->entries = (ht_bucket_t *) malloc (sizeof(ht_bucket_t) * capacity);
}

static inline void
ht_bucket_pool_destroy (ht_bucket_pool_t *pool)
{
    assert (pool != NULL);
    free (pool->entries);
}

static inline ht_bucket_t *
ht_bucket_pool_malloc (ht_bucket_pool_t *pool)
{
    assert (pool != NULL);
    assert (pool->num_entries < pool->capacity);
    pool->num_entries += 1;
    return &pool->entries[pool->num_entries - 1];
}

static inline ht_bucket_t *
ht_bucket_create (ht_bucket_pool_t *pool,
                  node_t key,
                  bdd_t value,
                  ht_bucket_t *next)
{
    ht_bucket_t *bucket = ht_bucket_pool_malloc (pool);
    bucket->key = key;
    bucket->value = value;
    bucket->next = next;
    return bucket;
}

/* Performs a linear search down a bucket list for the given key.  A
 * pointer to the bucket with the matching key is returned if one
 * exists, and NULL returned otherwise. */
static inline ht_bucket_t *
ht_bucket_search (ht_bucket_t *bucket, node_t key)
{
    ht_bucket_t *p = bucket;
    while (p != NULL) {
        if (node_equal(p->key, key))
            break;
        p = p->next;
    }
    return p;
}

#endif /* NODE_HT_BUCKET_INCLUDED */
