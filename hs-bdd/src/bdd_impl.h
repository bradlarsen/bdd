#ifndef BDD_IMPL_INCLUDED
#define BDD_IMPL_INCLUDED

#include <assert.h>
#include <stdlib.h>
#include <math.h>

#include "bdd.h"
#include "bdd_ite_cache.h"
#include "node.h"
#include "node_vec.h"
#include "node_ht.h"

struct bdd_mgr
{
    /* The number of variables. */
    unsigned num_vars;
    /* A vector of nodes.  Node 0 is the F terminal and node 1 is the T
     * terminal. */
    node_vec_t nodes_by_idx;
    /* A hash table from nodes to index.  This field and nodes_by_idx
     * form a one-to-one mapping. */
    node_ht_t idxs_by_node;

    bdd_ite_cache_t ite_cache;
};

/* FIXME: inline is not C89 */
/* Gets the node associated with the given BDD. */
static inline node_t
bdd_get_node (bdd_mgr_t *mgr, bdd_t idx)
{
    return node_vec_get (&mgr->nodes_by_idx, idx);
}

/* Answers whether the BDDs represented by the manager are reduced and
 * ordered. */
extern bool
is_robdd (bdd_mgr_t *mgr);

/* Gets the node representing T for the given manager. */
static inline node_t
get_true_node (bdd_mgr_t *mgr)
{
    node_t t;
    t.var = mgr->num_vars;
    t.low = bdd_false;
    t.high = bdd_true;
    return t;
}

/* Gets the node representing F for the given manager. */
static inline node_t
get_false_node (bdd_mgr_t *mgr)
{
    node_t f;
    f.var = mgr->num_vars;
    f.low = bdd_true;
    f.high = bdd_false;
    return f;
}

/* Retrieves the BDD of the node equal to 'node' if one exists,
 * otherwise creates and returns a new BDD. */
static inline bdd_t
make_node (bdd_mgr_t *mgr, node_t node)
{
    if (node.low == node.high)
        return node.low;
    else {
        bdd_t *existing_bdd;
        existing_bdd = node_ht_lookup (&mgr->idxs_by_node, node);
        if (existing_bdd != NULL)
            return *existing_bdd;
        else {
            unsigned idx;
            idx = node_vec_get_num_elems (&mgr->nodes_by_idx);
            node_vec_push_back (&mgr->nodes_by_idx, node);
            node_ht_insert (&mgr->idxs_by_node, node, idx);
            return idx;
        }
    }
}

/* An alternative, possibly more convenient way to call
 * 'make_node'. */
static inline bdd_t
make_node_from_parts (bdd_mgr_t *mgr, unsigned var, bdd_t low, bdd_t high)
{
    node_t n;
    n.var = var;
    n.low = low;
    n.high = high;
    return make_node (mgr, n);
}

#endif /* BDD_IMPL_INCLUDED */
