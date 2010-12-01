#include <cassert>
#include <cstdlib>

struct bdd_manager
{
    unsigned num_vars;        /* the number of variables */
};

typedef struct bdd_manager bdd_manager_t;

bdd_manager *
bdd_manager_create (unsigned num_vars)
{
    bdd_manager *mgr = (bdd_manager *)malloc(sizeof(bdd_manager));
    mgr->num_vars = num_vars;
    return mgr;
}

void
bdd_manager_destroy (bdd_manager_t *mgr)
{
    assert (mgr != NULL);
    free (mgr);
}

unsigned
bdd_manager_get_num_vars (bdd_manager_t *mgr)
{
    assert (mgr);
    return mgr->num_vars;
}
