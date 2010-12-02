#include "node_hash_table_impl.h"

bool
proper_hash_values (node_hash_table_t *tab)
{
    for (unsigned i = 0; i < tab->num_buckets; i += 1) {
        for (ht_bucket_t *p = tab->buckets[i]; p != NULL; p = p->next) {
            if (get_hash_index(tab, p->key) != i)
                return false;
        }
    }

    return true;
}

bool
no_duplicate_keys (node_hash_table_t *tab)
{
    unsigned num_buckets = tab->num_buckets;
    ht_bucket_t **buckets = tab->buckets;

    for (unsigned i = 0; i < num_buckets; i += 1)
        for (ht_bucket_t *p = buckets[i]; p != NULL; p = p->next)
            for (unsigned j = i; j < num_buckets; j += 1)
                for (ht_bucket_t *q = buckets[j]; q != NULL; q = q->next)
                    if (p != q && node_equal(p->key, q->key))
                        return false;
    return true;
}
