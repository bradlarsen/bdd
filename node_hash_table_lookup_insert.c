#include "node_hash_table_impl.h"

static void
double_hash_table_num_buckets (node_hash_table_t *tab)
{
    const unsigned old_num_buckets = tab->num_buckets;
    ht_bucket_t **old_buckets = tab->buckets;

    tab->num_buckets *= 2;
    tab->buckets =
        (ht_bucket_t **) malloc (tab->num_buckets * sizeof(ht_bucket_t *));
    for (unsigned i = 0; i < tab->num_buckets; i += 1)
        tab->buckets[i] = NULL;
    tab->num_entries = 0;

    for (unsigned i = 0; i < old_num_buckets; i += 1)
        for (ht_bucket_t *p = old_buckets[i]; p != NULL; p = p->next)
            node_hash_table_insert (tab, p->key, p->value);

    node_hash_table_check_invariants (tab);
}

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
    if (node_hash_table_get_load(tab) > 0.70f)
        double_hash_table_num_buckets (tab);
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
