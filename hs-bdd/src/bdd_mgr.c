#include "bdd_impl.h"

static void
init_apply_caches (bdd_mgr_t *mgr)
{
    unsigned i;
    for (i = 0; i < NUM_APPLY_OPS; i += 1)
        mgr->apply_caches[i] = bdd_pair_cache_create_with_hint (1024 * 32);
}

static void
free_apply_caches (bdd_mgr_t *mgr)
{
    unsigned i;
    for (i = 0; i < NUM_APPLY_OPS; i += 1)
        bdd_pair_cache_destroy (mgr->apply_caches[i]);
}

bdd_mgr_t *
bdd_mgr_create (unsigned num_vars)
{
    return bdd_mgr_create_with_hint (num_vars, 1024);
}

bdd_mgr_t *
bdd_mgr_create_with_hint (unsigned num_vars, unsigned capacity_hint)
{
    bdd_mgr_t *mgr = (bdd_mgr_t *) malloc (sizeof(bdd_mgr_t));
    mgr->num_vars = num_vars;
    mgr->nodes_by_idx = node_vec_create_with_capacity (capacity_hint);
    mgr->idxs_by_node = node_ht_create_with_hint (capacity_hint);

    /* Not using make_node here; special case. */
    node_vec_push_back (mgr->nodes_by_idx, get_sentinel_node(mgr));
    node_ht_insert (mgr->idxs_by_node, get_sentinel_node(mgr), 0);
    node_vec_push_back (mgr->nodes_by_idx, get_true_node(mgr));
    node_ht_insert (mgr->idxs_by_node, get_true_node(mgr), 1);

    init_apply_caches (mgr);
    bdd_mgr_check_invariants (mgr);
    return mgr;
}


void
bdd_mgr_destroy (bdd_mgr_t *mgr)
{
    if (mgr == NULL) return;
    bdd_mgr_check_invariants (mgr);
    free_apply_caches (mgr);
    node_ht_destroy (mgr->idxs_by_node);
    node_vec_destroy (mgr->nodes_by_idx);
    free (mgr);
}

unsigned
bdd_mgr_get_num_vars (bdd_mgr_t *mgr)
{
    bdd_mgr_check_invariants (mgr);
    return mgr->num_vars;
}

unsigned
bdd_mgr_get_num_nodes (bdd_mgr_t *mgr)
{
    bdd_mgr_check_invariants (mgr);
    return node_vec_get_num_elems(mgr->nodes_by_idx);
}
