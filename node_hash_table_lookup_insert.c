#include "node_hash_table_impl.h"

bool
node_hash_table_has_key (node_hash_table_t *tab, node_t key)
{
    return node_hash_table_lookup (tab, key) != NULL;
}

void
node_hash_table_insert (node_hash_table_t *tab,
                        node_t key,
                        unsigned val)
{
    node_hash_table_check_invariants (tab);
    const unsigned b_idx = get_hash_index (tab, key);
    ht_bucket_t *b_node = ht_bucket_search (tab->buckets[b_idx], key);
    if (b_node == NULL) {
        tab->buckets[b_idx] = ht_bucket_create (key, val, tab->buckets[b_idx]);
        tab->num_entries += 1;
    }
    else {
        b_node->value = val;
    }
}

unsigned *
node_hash_table_lookup (node_hash_table_t *tab, node_t key)
{
    node_hash_table_check_invariants (tab);
    const unsigned b_idx = get_hash_index (tab, key);
    ht_bucket_t *b_node = ht_bucket_search (tab->buckets[b_idx], key);
    return b_node != NULL ? &b_node->value : NULL;
}
