#include "bdd_pair_hash_table_impl.h"

unsigned
bdd_pair_hash_table_get_num_entries (bdd_pair_hash_table_t *tab)
{
    bdd_pair_hash_table_check_invariants (tab);
    return tab->num_entries;
}

unsigned
bdd_pair_hash_table_get_num_buckets (bdd_pair_hash_table_t *tab)
{
    bdd_pair_hash_table_check_invariants (tab);
    return tab->num_buckets;
}

float
bdd_pair_hash_table_get_load (bdd_pair_hash_table_t *tab)
{
    bdd_pair_hash_table_check_invariants (tab);
    return
        (float) bdd_pair_hash_table_get_num_entries (tab) /
        (float) bdd_pair_hash_table_get_num_buckets (tab);
}
