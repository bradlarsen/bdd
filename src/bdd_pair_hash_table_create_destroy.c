#include "bdd_pair_hash_table_impl.h"

static unsigned
up_to_next_power_of_two (unsigned n)
{
    unsigned i;
    for (i = 1; i <= n; i *= 2);
    return i;
}

bdd_pair_hash_table_t *
bdd_pair_hash_table_create ()
{
    return bdd_pair_hash_table_create_with_hint (32);
}

bdd_pair_hash_table_t *
bdd_pair_hash_table_create_with_hint (unsigned num_buckets_hint)
{
    bdd_pair_hash_table_t *tab =
        (bdd_pair_hash_table_t *) malloc (sizeof(bdd_pair_hash_table_t));
    tab->num_entries = 0;
    const unsigned num_buckets = up_to_next_power_of_two (num_buckets_hint);
    tab->num_buckets = num_buckets;
    tab->buckets =
        (bdd_pair_bucket_t **) malloc (num_buckets * sizeof(bdd_pair_bucket_t *));
    for (unsigned i = 0; i < num_buckets; i += 1)
        tab->buckets[i] = NULL;
    bdd_pair_hash_table_check_invariants (tab);
    return tab;
}

void
bdd_pair_hash_table_destroy (bdd_pair_hash_table_t *tab)
{
    bdd_pair_hash_table_check_invariants (tab);
    for (unsigned i = 0; i < tab->num_buckets; i += 1)
        bdd_pair_bucket_free (tab->buckets[i]);
    free (tab->buckets);
    free (tab);
}
