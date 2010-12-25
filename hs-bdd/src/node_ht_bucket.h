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

/* Allocates and initializes a bucket with the given parameters. */
static inline ht_bucket_t *
ht_bucket_create (node_t key,
                  bdd_t value,
                  ht_bucket_t *next)
{
    ht_bucket_t * bucket = (ht_bucket_t *) malloc (sizeof(ht_bucket_t));
    bucket->key = key;
    bucket->value = value;
    bucket->next = next;
    return bucket;
}

/* Frees the memory used by the list rooted by the bucket. */
static inline void
ht_bucket_free (ht_bucket_t *bucket)
{
    ht_bucket_t *p = bucket;
    while (p != NULL) {
        ht_bucket_t *prev = p;
        p = p->next;
        free (prev);
    }
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
