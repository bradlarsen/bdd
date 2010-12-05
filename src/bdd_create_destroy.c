#include "bdd_impl.h"

bdd_mgr_t *
bdd_mgr_create (unsigned num_vars)
{
    bdd_mgr_t *mgr = (bdd_mgr_t *) malloc (sizeof(bdd_mgr_t));
    mgr->num_vars = num_vars;
    mgr->nodes_by_idx = node_vector_create ();
    mgr->idxs_by_node = node_hash_table_create ();
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
    node_hash_table_destroy (mgr->idxs_by_node);
    node_vector_destroy (mgr->nodes_by_idx);
    free (mgr);
}
