#include "bdd_impl.h"

bool
is_robdd (bdd_mgr_t *mgr)
{
    const unsigned vec_len = node_vec_get_num_elems(mgr->nodes_by_idx);
    unsigned i;
    unsigned j;

    for (i = 1; i < vec_len; i += 1) {
        const node_t ibdd = bdd_get_node(mgr, i);
        if (bdd_is_complement(ibdd.high))
            return false;
        if (ibdd.low == ibdd.high)
            return false;
        if (bdd_to_idx(i) > 1) {
            /* skip the sentinel and true/false terminals */
            if (bdd_get_node(mgr, ibdd.low).var <= ibdd.var ||
                bdd_get_node(mgr, ibdd.high).var <= ibdd.var)
                return false;
        }

        for (j = i + 1; j < vec_len; j += 1)
            if (node_equal(ibdd, bdd_get_node(mgr, j)))
                return false;
    }

    return true;
}

bool
no_sentinel_links (bdd_mgr_t *mgr)
{
    const unsigned vec_len = node_vec_get_num_elems(mgr->nodes_by_idx);
    unsigned i;
    for (i = 1; i < vec_len; i += 1) {
        const node_t ibdd = bdd_get_node (mgr, i);
        if (ibdd.low == 0 || ibdd.high == 0)
            return false;
    }
    return true;
}

bdd_t
make_node (bdd_mgr_t *mgr, node_t node)
{
    if (node.low == node.high)
        return node.low;
    else {
        bdd_t *existing_bdd;
        bool invert;

        invert = false;
        if (bdd_is_complement(node.high)) {
            invert = true;
            node = node_negate (node);
        }
        node_check_invariants (node);

        existing_bdd = node_ht_lookup (mgr->idxs_by_node, node);
        if (existing_bdd != NULL)
            return invert ? -*existing_bdd : *existing_bdd;
        else {
            unsigned idx;
            bdd_t result;
            idx = node_vec_get_num_elems (mgr->nodes_by_idx);
            node_vec_push_back (mgr->nodes_by_idx, node);
            result = invert ? -((int)idx) : (int)idx;
            node_ht_insert (mgr->idxs_by_node, node, result);
            return result;
        }
    }
}
