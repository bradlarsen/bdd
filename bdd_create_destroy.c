#include "bdd_impl.h"

bdd_manager_t *
bdd_manager_create (unsigned num_vars)
{
    bdd_manager_t *mgr = (bdd_manager_t *) malloc (sizeof(bdd_manager_t));
    mgr->num_vars = num_vars;
    mgr->nodes_by_idx = node_vector_create ();
    mgr->idxs_by_node = node_hash_table_create ();
    make_node (mgr, get_false_node(mgr));
    make_node (mgr, get_true_node(mgr));
    bdd_manager_check_invariants (mgr);
    return mgr;
}

void
bdd_manager_destroy (bdd_manager_t *mgr)
{
    if (mgr == NULL) return;
    bdd_manager_check_invariants (mgr);
    node_hash_table_destroy (mgr->idxs_by_node);
    node_vector_destroy (mgr->nodes_by_idx);
    free (mgr);
}
