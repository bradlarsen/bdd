#include "bdd_pair_hash_table_impl.h"

bool
bdd_pair_hash_table_proper_hash_values (bdd_pair_hash_table_t *tab)
{
    for (unsigned i = 0; i < tab->num_buckets; i += 1) {
        for (bdd_pair_bucket_t *p = tab->buckets[i]; p != NULL; p = p->next) {
            if (bdd_pair_hash_table_get_hash_index(tab, p->key) != i)
                return false;
        }
    }

    return true;
}

bool
bdd_pair_hash_table_no_duplicate_keys (bdd_pair_hash_table_t *tab)
{
    unsigned num_buckets = tab->num_buckets;
    bdd_pair_bucket_t **buckets = tab->buckets;

    for (unsigned i = 0; i < num_buckets; i += 1)
        for (bdd_pair_bucket_t *p = buckets[i]; p != NULL; p = p->next)
            for (unsigned j = i; j < num_buckets; j += 1)
                for (bdd_pair_bucket_t *q = buckets[j]; q != NULL; q = q->next)
                    if (p != q && bdd_pair_equal(p->key, q->key))
                        return false;
    return true;
}
