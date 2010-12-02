#include <assert.h>
#include <stdlib.h>

#include "bdd.h"
#include "node.h"
#include "node_vector.h"
#include "node_hash_table.h"


/* Answers whether the BDDs represented by the manager are reduced and
 * ordered. */
static bool
is_robdd (bdd_manager_t *mgr);

/* Gets the node representing F for the given manager. */
static node_t
get_false_node (bdd_manager_t *mgr);

/* Gets the node representing T for the given manager. */
static node_t
get_true_node (bdd_manager_t *mgr);

/* Retrieves an existing node equal to the given one, otherwise creates
 * a new node. */
static unsigned
make_node (bdd_manager_t *mgr, node_t node);

const bdd bdd_false = 0;
const bdd bdd_true = 1;

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


static void
bdd_manager_check_invariants (bdd_manager_t *mgr)
{
    assert (mgr != NULL);
    assert (mgr->nodes_by_idx != NULL);
    assert (mgr->idxs_by_node != NULL);
    assert (node_vector_get_num_elems(mgr->nodes_by_idx) >= 2);

    assert (node_equal(node_vector_get(mgr->nodes_by_idx, 0),
                       get_false_node(mgr)));
    assert (node_equal(node_vector_get(mgr->nodes_by_idx, 1),
                       get_true_node(mgr)));

    assert (node_vector_get_num_elems(mgr->nodes_by_idx) ==
            node_hash_table_get_num_entries(mgr->idxs_by_node));
    assert (is_robdd(mgr));
}


static bool
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

bdd_manager_t *
bdd_manager_create (unsigned num_vars)
{
    bdd_manager_t *mgr = (bdd_manager_t *) malloc (sizeof(bdd_manager_t));
    mgr->num_vars = num_vars;
    mgr->nodes_by_idx = node_vector_create ();
    mgr->idxs_by_node = node_hash_table_create ();
    make_node (mgr, get_false_node(mgr));
    make_node (mgr, get_true_node(mgr));
    bdd_manager_check_invariants (mgr);
    return mgr;
}

void
bdd_manager_destroy (bdd_manager_t *mgr)
{
    if (mgr == NULL) return;
    bdd_manager_check_invariants (mgr);
    node_hash_table_destroy (mgr->idxs_by_node);
    node_vector_destroy (mgr->nodes_by_idx);
    free (mgr);
}

unsigned
bdd_manager_get_num_vars (bdd_manager_t *mgr)
{
    bdd_manager_check_invariants (mgr);
    return mgr->num_vars;
}

unsigned
bdd_manager_get_num_nodes (bdd_manager_t *mgr)
{
    bdd_manager_check_invariants (mgr);
    return node_vector_get_num_elems(mgr->nodes_by_idx);
}



static inline node_t
get_true_node (bdd_manager_t *mgr)
{
    node_t t = {mgr->num_vars, 0, 1};
    return t;
}

static inline node_t
get_false_node (bdd_manager_t *mgr)
{
    node_t f = {mgr->num_vars, 1, 0};
    return f;
}

static unsigned
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
