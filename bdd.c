#include <assert.h>
#include <stdlib.h>

#include "bdd.h"
#include "node.h"
#include "node_vector.h"
#include "node_hash_table.h"


struct bdd_manager
{
    unsigned num_vars;                /* the number of variables */
    node_vector_t *nodes_by_idx;      /* a vector of nodes */
    node_hash_table_t *idxs_by_node;  /* a hash table from nodes to index */
};


static inline void
bdd_manager_check_invariants (bdd_manager_t *mgr)
{
    assert (mgr != NULL);
    assert (mgr->nodes_by_idx != NULL);
    assert (mgr->idxs_by_node != NULL);
    assert (node_vector_get_num_elems(mgr->nodes_by_idx) ==
            node_hash_table_get_num_entries(mgr->idxs_by_node));
}


bdd_manager_t *
bdd_manager_create (unsigned num_vars)
{
    bdd_manager_t *mgr = (bdd_manager_t *) malloc (sizeof(bdd_manager_t));
    mgr->num_vars = num_vars;
    mgr->nodes_by_idx = node_vector_create ();
    mgr->idxs_by_node = node_hash_table_create ();
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
