#include "bdd_impl.h"

const bdd bdd_false = 0;
const bdd bdd_true = 1;

extern node_t
get_true_node (bdd_manager_t *mgr);

extern node_t
get_false_node (bdd_manager_t *mgr);

bool
is_robdd (bdd_manager_t *mgr)
{
    const unsigned vec_len = node_vector_get_num_elems(mgr->nodes_by_idx);

    for (unsigned i = 2; i < vec_len; i += 1) {
        /* skip the false and true terminals */
        const node_t ibdd = node_vector_get(mgr->nodes_by_idx, i);
        if (ibdd.low == ibdd.high)
            return false;
        const node_t ibddlow = node_vector_get(mgr->nodes_by_idx, ibdd.low);
        const node_t ibddhigh = node_vector_get(mgr->nodes_by_idx, ibdd.high);
        if (ibddlow.var <= ibdd.var || ibddhigh.var <= ibdd.var)
            return false;
    }

    for (unsigned i = 0; i < vec_len; i += 1) {
        const node_t ibdd = node_vector_get(mgr->nodes_by_idx, i);
        for (unsigned j = i + 1; j < vec_len; j += 1)
            if (node_equal(ibdd, node_vector_get(mgr->nodes_by_idx, j)))
                return false;
    }

    return true;
}

unsigned
make_node (bdd_manager_t *mgr, node_t node)
{
    if (node.low == node.high) return node.low;
    const unsigned *existing_idx =
        node_hash_table_lookup (mgr->idxs_by_node, node);
    if (existing_idx != NULL) return *existing_idx;
    node_vector_push_back (mgr->nodes_by_idx, node);
    const unsigned idx = node_vector_get_num_elems (mgr->nodes_by_idx);
    node_hash_table_insert (mgr->idxs_by_node, node, idx);
    return idx;
}
