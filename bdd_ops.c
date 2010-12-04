#include "bdd_impl.h"

bdd
bdd_manager_get_ith_var (bdd_manager_t *mgr, unsigned i)
{
    bdd_manager_check_invariants (mgr);
    assert (i < mgr->num_vars);
    const node_t node = {i, bdd_false, bdd_true};
    const bdd ith_var = make_node (mgr, node);
    bdd_manager_check_invariants (mgr);
    return ith_var;
}
