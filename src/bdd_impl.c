#include "bdd_impl.h"

extern node_t
get_true_node (bdd_mgr_t *mgr);

extern node_t
get_false_node (bdd_mgr_t *mgr);

bool
is_robdd (bdd_mgr_t *mgr)
{
    const unsigned vec_len = node_vec_get_num_elems(mgr->nodes_by_idx);
    unsigned i;
    unsigned j;

    for (i = 2; i < vec_len; i += 1) {
        /* skip the false and true terminals */
        const node_t ibdd = get_node_by_idx(mgr, i);
        if (ibdd.low == ibdd.high)
            return false;
        if (get_node_by_idx(mgr, ibdd.low).var <= ibdd.var ||
            get_node_by_idx(mgr, ibdd.high).var <= ibdd.var)
            return false;
    }

    for (i = 0; i < vec_len; i += 1) {
        const node_t ibdd = get_node_by_idx(mgr, i);
        for (j = i + 1; j < vec_len; j += 1)
            if (node_equal(ibdd, get_node_by_idx(mgr, j)))
                return false;
    }

    return true;
}

extern unsigned
make_node_from_parts (bdd_mgr_t *mgr, unsigned var, bdd_t low, bdd_t high);

unsigned
make_node (bdd_mgr_t *mgr, node_t node)
{
    if (node.low == node.high)
        return node.low;
    else {
        const unsigned *existing_idx =
            node_ht_lookup (mgr->idxs_by_node, node);
        if (existing_idx != NULL)
            return *existing_idx;
        else {
            const unsigned idx = node_vec_get_num_elems (mgr->nodes_by_idx);
            node_vec_push_back (mgr->nodes_by_idx, node);
            node_ht_insert (mgr->idxs_by_node, node, idx);
            return idx;
        }
    }
}
