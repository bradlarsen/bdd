#include "bdd.h"
#include <stdio.h>

int
main (int argc, char *argv[])
{
    bdd_manager_t *mgr = bdd_manager_create (10);
    printf ("%d variables\n", bdd_manager_get_num_vars(mgr));
    bdd_manager_destroy (mgr);

    return 0;
}
