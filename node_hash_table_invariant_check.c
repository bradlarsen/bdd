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
