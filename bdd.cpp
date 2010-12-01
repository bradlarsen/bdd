#include <cassert>
#include <cstdlib>

struct bdd_manager
{
};

bdd_manager *
bdd_manager_create ()
{
    bdd_manager * mgr = (bdd_manager *)malloc(sizeof(bdd_manager));
    return mgr;
}

void
bdd_manager_destroy (bdd_manager *mgr)
{
    assert (mgr != NULL);
    free (mgr);
}
