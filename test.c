#include "bdd.h"
#include <stdio.h>
#include <assert.h>

static const unsigned NUM_VARS = 100;

int
main (int argc, char *argv[])
{
    bdd_manager_t *mgr = bdd_manager_create (NUM_VARS);
    printf ("%u variables\n", bdd_manager_get_num_vars(mgr));
    printf ("%u nodes\n", bdd_manager_get_num_nodes(mgr));
    for (unsigned i = 0; i < NUM_VARS; i += 1) {
        bdd ivar = bdd_manager_get_ith_var (mgr, i);
        printf ("the BDD for variable %u has index %u\n", i, ivar);
    }
    const unsigned old_num_nodes = bdd_manager_get_num_nodes(mgr);
    printf ("%u nodes\n", old_num_nodes);
    for (unsigned i = 0; i < NUM_VARS; i += 1) {
        const unsigned idx = NUM_VARS - i - 1;
        bdd ivar = bdd_manager_get_ith_var (mgr, idx);
        printf ("the BDD for variable %u has index %u\n", idx, ivar);
    }
    const unsigned new_num_nodes = bdd_manager_get_num_nodes(mgr);
    printf ("%u nodes\n", new_num_nodes);
    assert (old_num_nodes == new_num_nodes);
    const bdd var5 = bdd_manager_get_ith_var (mgr, 5);
    bdd var5_and_var5 = bdd_apply (mgr, BDD_AND, var5, var5);
    printf ("var 5 has index %u, and index %u when conjoined with itself\n",
            var5, var5_and_var5);
    assert (var5 == var5_and_var5);
    bdd_manager_destroy (mgr);

    return 0;
}
