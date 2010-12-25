#ifndef BDD_IMPL_INCLUDED
#define BDD_IMPL_INCLUDED

#include <assert.h>
#include <stdlib.h>
#include <math.h>

#include "bdd.h"
#include "bdd_pair_cache.h"
#include "node.h"
#include "node_vec.h"
#include "node_ht.h"

typedef enum {
    BDD_AND = 0,
    BDD_OR,
    BDD_XOR,
    BDD_EQUIV,
    BDD_NAND,
    BDD_IMPLIES
} bdd_apply_op;

enum {NUM_APPLY_OPS = 6};

struct bdd_mgr
{
    /* The number of variables. */
    unsigned num_vars;
    /* A vector of nodes.  Node 0 is the F terminal and node 1 is the T
     * terminal. */
    node_vec_t *nodes_by_idx;
    /* A hash table from nodes to index.  This field and nodes_by_idx
     * form a one-to-one mapping. */
    node_ht_t *idxs_by_node;

    bdd_pair_cache_t apply_caches[NUM_APPLY_OPS];
};

/* FIXME: inline is not C89 */
/* Gets the node associated with the given BDD. */
static inline node_t
bdd_get_node (bdd_mgr_t *mgr, bdd_t idx)
{
    node_t n;
    if (bdd_is_complement(idx)) {
        n = node_vec_get (mgr->nodes_by_idx, -idx);
        node_check_invariants(n);
        n = node_negate (n);
    }
    else {
        n = node_vec_get (mgr->nodes_by_idx, idx);
        node_check_invariants(n);
    }
    return n;
}

#define bdd_mgr_check_invariants(mgr)                                   \
    do {                                                                \
        assert (mgr != NULL);                                           \
        assert (mgr->nodes_by_idx != NULL);                             \
        assert (mgr->idxs_by_node != NULL);                             \
        assert (node_vec_get_num_elems(mgr->nodes_by_idx) >= 2);        \
                                                                        \
        assert (node_equal(bdd_get_node(mgr, 0), get_sentinel_node(mgr))); \
        assert (node_equal(bdd_get_node(mgr, 1), get_true_node(mgr)));  \
                                                                        \
        assert (node_vec_get_num_elems(mgr->nodes_by_idx) ==            \
                node_ht_get_num_entries(mgr->idxs_by_node));            \
        assert (is_robdd(mgr));                                         \
        assert (no_sentinel_links(mgr));                                \
    } while (0)

/* Answers whether the BDDs represented by the manager are reduced and
 * ordered. */
extern bool
is_robdd (bdd_mgr_t *mgr);

/* Answers whether no nodes link to the sentinel node. */
extern bool
no_sentinel_links (bdd_mgr_t *mgr);

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

/* Gets the sentinel node for the given manager. */
static inline node_t
get_sentinel_node (bdd_mgr_t *mgr)
{
    node_t s;
    s.var = mgr->num_vars;
    s.low = 0;
    s.high = 1;
    return s;
}

/* Retrieves the BDD of the node equal to 'node' if one exists,
 * otherwise creates and returns a new BDD. */
extern bdd_t
make_node (bdd_mgr_t *mgr, node_t node);

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
