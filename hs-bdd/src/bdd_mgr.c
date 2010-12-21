#include "bdd_impl.h"

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
    make_node (mgr, get_false_node(mgr));
    make_node (mgr, get_true_node(mgr));
    bdd_mgr_check_invariants (mgr);
    return mgr;
}


void
bdd_mgr_destroy (bdd_mgr_t *mgr)
{
    if (mgr == NULL) return;
    bdd_mgr_check_invariants (mgr);
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
