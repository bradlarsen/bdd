#include "bdd.h"

int
main (int argc, char *argv[])
{
    bdd_manager *mgr = bdd_manager_create ();
    bdd_manager_destroy (mgr);

    return 0;
}
