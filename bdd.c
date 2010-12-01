#include <assert.h>
#include <stdlib.h>

#include "bdd.h"
#include "node.h"
#include "node_vector.h"


struct bdd_manager
{
    unsigned num_vars;               /* the number of variables */
    node_vector_t *nodes_by_idx;     /* a vector of nodes */
};


bdd_manager_t *
bdd_manager_create (unsigned num_vars)
{
    bdd_manager_t *mgr = (bdd_manager_t *) malloc(sizeof(bdd_manager_t));
    mgr->num_vars = num_vars;
    mgr->nodes_by_idx = node_vector_create ();
    return mgr;
}

void
bdd_manager_destroy (bdd_manager_t *mgr)
{
    if (mgr == NULL) return;
    node_vector_destroy (mgr->nodes_by_idx);
    free (mgr);
}

unsigned
bdd_manager_get_num_vars (bdd_manager_t *mgr)
{
    assert (mgr);
    return mgr->num_vars;
}

unsigned
bdd_manager_get_num_nodes (bdd_manager_t *mgr)
{
    assert (mgr);
    return node_vector_get_size(mgr->nodes_by_idx);
}
