#include "bdd_impl.h"

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

unsigned
bdd_get_num_nodes (bdd_mgr_t *mgr, bdd_t b)
{
    if (b == bdd_true || b == bdd_false)
        return 1;
    else {
        const node_t b_node = get_node_by_idx (mgr, b);
        return
            bdd_get_num_nodes (mgr, b_node.low) +
            bdd_get_num_nodes (mgr, b_node.high);
    }
}
