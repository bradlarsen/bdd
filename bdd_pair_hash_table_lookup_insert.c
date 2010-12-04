#include "bdd_pair_hash_table_impl.h"

static void
double_hash_table_num_buckets (bdd_pair_hash_table_t *tab)
{
    const unsigned old_num_buckets = tab->num_buckets;
    bdd_pair_bucket_t **old_buckets = tab->buckets;

    tab->num_buckets *= 2;
    tab->buckets =
        (bdd_pair_bucket_t **) malloc (tab->num_buckets * sizeof(bdd_pair_bucket_t *));
    for (unsigned i = 0; i < tab->num_buckets; i += 1)
        tab->buckets[i] = NULL;
    tab->num_entries = 0;

    for (unsigned i = 0; i < old_num_buckets; i += 1) {
        for (bdd_pair_bucket_t *p = old_buckets[i]; p != NULL; p = p->next) {
            bdd_pair_hash_table_insert (tab, p->key, p->value);
        }
        bdd_pair_bucket_free (old_buckets[i]);
    }
    free (old_buckets);

    bdd_pair_hash_table_check_invariants (tab);
}

bool
bdd_pair_hash_table_has_key (bdd_pair_hash_table_t *tab, bdd_pair_t key)
{
    return bdd_pair_hash_table_lookup (tab, key) != NULL;
}

void
bdd_pair_hash_table_insert (bdd_pair_hash_table_t *tab,
                        bdd_pair_t key,
                        unsigned val)
{
    bdd_pair_hash_table_check_invariants (tab);
    if (bdd_pair_hash_table_get_load(tab) > 0.70f)
        double_hash_table_num_buckets (tab);
    const unsigned b_idx = bdd_pair_hash_table_get_hash_index (tab, key);
    bdd_pair_bucket_t *b_bdd_pair = bdd_pair_bucket_search (tab->buckets[b_idx], key);
    if (b_bdd_pair == NULL) {
        tab->buckets[b_idx] = bdd_pair_bucket_create (key, val, tab->buckets[b_idx]);
        tab->num_entries += 1;
    }
    else {
        b_bdd_pair->value = val;
    }
}

unsigned *
bdd_pair_hash_table_lookup (bdd_pair_hash_table_t *tab, bdd_pair_t key)
{
    bdd_pair_hash_table_check_invariants (tab);
    const unsigned b_idx = bdd_pair_hash_table_get_hash_index (tab, key);
    bdd_pair_bucket_t *b_bdd_pair = bdd_pair_bucket_search (tab->buckets[b_idx], key);
    return b_bdd_pair != NULL ? &b_bdd_pair->value : NULL;
}
