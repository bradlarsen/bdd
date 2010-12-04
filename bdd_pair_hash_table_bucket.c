#include "bdd_pair_hash_table_impl.h"

bdd_pair_bucket_t *
bdd_pair_bucket_create (bdd_pair_t key, unsigned value, bdd_pair_bucket_t *next)
{
    bdd_pair_bucket_t * bucket = (bdd_pair_bucket_t *) malloc (sizeof(bdd_pair_bucket_t));
    bucket->key = key;
    bucket->value = value;
    bucket->next = next;
    return bucket;
}

void
bdd_pair_bucket_free (bdd_pair_bucket_t *bucket)
{
    if (bucket == NULL)
        return;

    bdd_pair_bucket_t *p = bucket;
    while (p != NULL) {
        bdd_pair_bucket_t *prev = p;
        p = p->next;
        free (prev);
    }
}

bdd_pair_bucket_t *
bdd_pair_bucket_search (bdd_pair_bucket_t *bucket, bdd_pair_t key)
{
    bdd_pair_bucket_t *p = bucket;
    while (p != NULL) {
        if (bdd_pair_equal(p->key, key))
            break;
        p = p->next;
    }
    return p;
}
