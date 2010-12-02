#include "node_hash_table_def.h"

unsigned
node_hash_table_get_num_entries (node_hash_table_t *tab)
{
    node_hash_table_check_invariants (tab);
    return tab->num_entries;
}

unsigned
node_hash_table_get_num_buckets (node_hash_table_t *tab)
{
    node_hash_table_check_invariants (tab);
    return tab->num_buckets;
}

float
node_hash_table_get_load (node_hash_table_t *tab)
{
    node_hash_table_check_invariants (tab);
    return
        (float) node_hash_table_get_num_entries (tab) /
        (float) node_hash_table_get_num_buckets (tab);
}
