#include "node_hash_table_impl.h"

/* The pair hash function from Andersen's ``An introduction to binary
 * decision diagrams''. */
static inline unsigned
hash_pair (unsigned i, unsigned j)
{
    return ((i + j) * (i + j + 1) / 2) + i;
}

/* The node hash function h from Andersen's ``An introduction to
 * binary decision diagrams''. */
static inline unsigned
hash_node (node_t node)
{
    return hash_pair(node.var, hash_pair(node.low, node.high)) % 15485863;
}

/* Function h' from Andersen's ``An introduction to binary decision
 * diagrams''.  Gets the bucket index for the given key. */
static inline unsigned
get_hash_index (node_hash_table_t *tab, node_t key)
{
    return hash_node (key) % tab->num_buckets;
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
