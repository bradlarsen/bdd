#include "node_hash_table_impl.h"

static unsigned
up_to_next_power_of_two (unsigned n)
{
    unsigned i;
    for (i = 1; i <= n; i *= 2);
    return i;
}

node_hash_table_t *
node_hash_table_create ()
{
    return node_hash_table_create_with_hint (32);
}

node_hash_table_t *
node_hash_table_create_with_hint (unsigned num_buckets_hint)
{
    node_hash_table_t *tab =
        (node_hash_table_t *) malloc (sizeof(node_hash_table_t));
    tab->num_entries = 0;
    const unsigned num_buckets = up_to_next_power_of_two (num_buckets_hint);
    tab->num_buckets = num_buckets;
    tab->buckets =
        (ht_bucket_t **) malloc (num_buckets * sizeof(ht_bucket_t *));
    for (unsigned i = 0; i < num_buckets; i += 1)
        tab->buckets[i] = NULL;
    node_hash_table_check_invariants (tab);
    return tab;
}

void
node_hash_table_destroy (node_hash_table_t *tab)
{
    node_hash_table_check_invariants (tab);
    for (unsigned i = 0; i < tab->num_buckets; i += 1)
        ht_bucket_free (tab->buckets[i]);
    free (tab->buckets);
    free (tab);
}
