#include "node_hash_table_def.h"

ht_bucket_t *
ht_bucket_create (node_t key, unsigned value, ht_bucket_t *next)
{
    ht_bucket_t * bucket = (ht_bucket_t *) malloc (sizeof(ht_bucket_t));
    bucket->key = key;
    bucket->value = value;
    bucket->next = next;
    return bucket;
}

void
ht_bucket_free (ht_bucket_t *bucket)
{
    if (bucket == NULL)
        return;

    ht_bucket_t *p = bucket;
    while (p != NULL) {
        ht_bucket_t *prev = p;
        p = p->next;
        free (prev);
    }
}

ht_bucket_t *
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
