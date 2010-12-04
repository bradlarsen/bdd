#include "bdd_impl.h"

unsigned
bdd_manager_get_num_vars (bdd_manager_t *mgr)
{
    bdd_manager_check_invariants (mgr);
    return mgr->num_vars;
}

unsigned
bdd_manager_get_num_nodes (bdd_manager_t *mgr)
{
    bdd_manager_check_invariants (mgr);
    return node_vector_get_num_elems(mgr->nodes_by_idx);
}
