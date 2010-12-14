#include "bdd_impl.h"

static void
init_apply_caches (bdd_mgr_t *mgr)
{
    unsigned i;
    for (i = 0; i < NUM_APPLY_OPS; i += 1)
        mgr->apply_caches[i] = bdd_pair_ht_create ();
}

static void
free_apply_caches (bdd_mgr_t *mgr)
{
    unsigned i;
    for (i = 0; i < NUM_APPLY_OPS; i += 1)
        bdd_pair_ht_destroy (mgr->apply_caches[i]);
}

bdd_mgr_t *
bdd_mgr_create (unsigned num_vars)
{
    bdd_mgr_t *mgr = (bdd_mgr_t *) malloc (sizeof(bdd_mgr_t));
    mgr->num_vars = num_vars;
    mgr->nodes_by_idx = node_vec_create ();
    mgr->idxs_by_node = node_ht_create ();
    make_node (mgr, get_false_node(mgr));
    make_node (mgr, get_true_node(mgr));
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
