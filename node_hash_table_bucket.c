#include "node_hash_table_impl.h"

node_bucket_t *
node_bucket_create (node_t key, unsigned value, node_bucket_t *next)
{
    node_bucket_t * bucket = (node_bucket_t *) malloc (sizeof(node_bucket_t));
    bucket->key = key;
    bucket->value = value;
    bucket->next = next;
    return bucket;
}

void
node_bucket_free (node_bucket_t *bucket)
{
    if (bucket == NULL)
        return;

    node_bucket_t *p = bucket;
    while (p != NULL) {
        node_bucket_t *prev = p;
        p = p->next;
        free (prev);
    }
}

node_bucket_t *
node_bucket_search (node_bucket_t *bucket, node_t key)
{
    node_bucket_t *p = bucket;
    while (p != NULL) {
        if (node_equal(p->key, key))
            break;
        p = p->next;
    }
    return p;
}
