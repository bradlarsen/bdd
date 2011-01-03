#include "bdd_impl.h"

/* Gets the node representing T for the given manager. */
static node_t
get_true_node (bdd_mgr_t *mgr)
{
    node_t t;
    t.var = mgr->num_vars;
    t.low = bdd_false;
    t.high = bdd_true;
    return t;
}

/* Gets the node representing F for the given manager. */
static node_t
get_false_node (bdd_mgr_t *mgr)
{
    node_t f;
    f.var = mgr->num_vars;
    f.low = bdd_true;
    f.high = bdd_false;
    return f;
}

bdd_mgr_t *
bdd_mgr_create (unsigned num_vars)
{
    return bdd_mgr_create_with_hint (num_vars, 1024);
}

bdd_mgr_t *
bdd_mgr_create_with_hint (unsigned num_vars, unsigned capacity_hint)
{
    bdd_mgr_t *mgr = (bdd_mgr_t *) checked_malloc (sizeof(bdd_mgr_t));
    mgr->num_vars = num_vars;
    node_vec_create_with_capacity (&mgr->nodes_by_idx, capacity_hint);
    node_ht_create_with_hint (&mgr->idxs_by_node, capacity_hint);
    make_node (mgr, get_false_node(mgr));
    make_node (mgr, get_true_node(mgr));
    /* FIXME: use a more reasonable cache size */
    bdd_ite_cache_create_with_hint (&mgr->ite_cache, 1024 * 32);
    return mgr;
}

void
bdd_mgr_destroy (bdd_mgr_t *mgr)
{
    if (mgr == NULL) return;
    bdd_ite_cache_destroy (&mgr->ite_cache);
    node_ht_destroy (&mgr->idxs_by_node);
    node_vec_destroy (&mgr->nodes_by_idx);
    checked_free (mgr);
}

unsigned
bdd_mgr_get_num_vars (bdd_mgr_t *mgr)
{
    return mgr->num_vars;
}

unsigned
bdd_mgr_get_num_nodes (bdd_mgr_t *mgr)
{
    return node_vec_get_num_elems(&mgr->nodes_by_idx);
}
