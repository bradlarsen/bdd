#ifndef BDD_IMPL_INCLUDED
#define BDD_IMPL_INCLUDED

#include <assert.h>
#include <stdlib.h>

#include "bdd.h"
#include "node.h"
#include "node_vector.h"
#include "node_hash_table.h"

struct bdd_manager
{
    /* The number of variables. */
    unsigned num_vars;
    /* A vector of nodes.  Node 0 is the F terminal and node 1 is the T
     * terminal. */
    node_vector_t *nodes_by_idx;
    /* A hash table from nodes to index.  This field and nodes_by_idx
     * should form a one-to-one mapping. */
    node_hash_table_t *idxs_by_node;
};

#define bdd_manager_check_invariants(mgr)                               \
    do {                                                                \
        assert (mgr != NULL);                                           \
        assert (mgr->nodes_by_idx != NULL);                             \
        assert (mgr->idxs_by_node != NULL);                             \
        assert (node_vector_get_num_elems(mgr->nodes_by_idx) >= 2);     \
                                                                        \
        assert (node_equal(node_vector_get(mgr->nodes_by_idx, 0),       \
                           get_false_node(mgr)));                       \
        assert (node_equal(node_vector_get(mgr->nodes_by_idx, 1),       \
                           get_true_node(mgr)));                        \
                                                                        \
        assert (node_vector_get_num_elems(mgr->nodes_by_idx) ==         \
                node_hash_table_get_num_entries(mgr->idxs_by_node));    \
        assert (is_robdd(mgr));                                         \
    } while (0)

/* Answers whether the BDDs represented by the manager are reduced and
 * ordered. */
extern bool
is_robdd (bdd_manager_t *mgr);

/* Gets the node representing T for the given manager. */
inline node_t
get_true_node (bdd_manager_t *mgr)
{
    node_t t = {mgr->num_vars, bdd_false, bdd_true};
    return t;
}

/* Gets the node representing F for the given manager. */
inline node_t
get_false_node (bdd_manager_t *mgr)
{
    node_t f = {mgr->num_vars, bdd_true, bdd_false};
    return f;
}

/* Retrieves an existing node equal to the given one, otherwise creates
 * a new node. */
extern unsigned
make_node (bdd_manager_t *mgr, node_t node);

inline unsigned
make_node_from_parts (bdd_manager_t *mgr, unsigned var, bdd low, bdd high)
{
    node_t n = {var, low, high};
    return make_node (mgr, n);
}


#endif /* BDD_IMPL_INCLUDED */
