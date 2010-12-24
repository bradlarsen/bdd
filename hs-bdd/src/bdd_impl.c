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

        if (i > 1) {
            /* skip the sentinel and true terminals */
            const node_t ibdd = bdd_get_node(mgr, i);
            if (ibdd.low == ibdd.high)
                return false;
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

/* FIXME: the BDD package is broken at present. */
/* One idea to fix this: the callees of this function need to be
 * modified to pass in the parent of 'node' as well, so that the
 * correct canonical form can be returned.  This is a half-baked
 * idea. */
bdd_t
make_node (bdd_mgr_t *mgr, node_t node)
{
    if (node.low == node.high)
        return node.low;
    else {
        bdd_t *existing_bdd;
        /* bool invert_parent; */
        bdd_t result;

        /* invert_parent = false; */
        /* if (bdd_is_complement(node.high)) { */
        /*     invert_parent = true; */
        /*     node.low = -node.low; */
        /*     node.high = -node.high; */
        /* } */

        existing_bdd = node_ht_lookup (mgr->idxs_by_node, node);
        if (existing_bdd != NULL)
            result = *existing_bdd;
        else {
            const unsigned idx = node_vec_get_num_elems (mgr->nodes_by_idx);
            node_vec_push_back (mgr->nodes_by_idx, node);
            node_ht_insert (mgr->idxs_by_node, node, idx);
            result = idx;
        }

        /* return invert_parent ? -result : result; */
        return result;
    }
}
