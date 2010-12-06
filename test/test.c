/* Whitebox tests for the BDD library. */
#include "bdd.h"
#include <stdio.h>
#include <stdlib.h>

/**********************************************************************/
/* TEST INFRASTRUCTURE                                                */
/**********************************************************************/
static char *test_name = NULL;
static unsigned num_tests = 0;

#define ASSERT(cond)                                            \
    do {                                                        \
        if (!cond) {                                            \
            fprintf (stderr, "FAIL\n"                           \
                             "At %s:%u, the condition\n\n"      \
                             "\t%s\n\n"                         \
                             "failed.\n",                       \
                     __FILE__, __LINE__, #cond);                \
            exit(EXIT_FAILURE);                                 \
        }                                                       \
    } while (0)

#define TEST_NAME(str)                          \
    do {                                        \
        if (test_name != NULL)                  \
            fprintf (stderr, "PASS\n");         \
        fprintf (stderr, "%s: ", str);          \
        test_name = str;                        \
        num_tests += 1;                         \
    } while (0)

/**********************************************************************/
/* ENTRY POINT                                                        */
/**********************************************************************/
int
main ()
{
    const unsigned NUM_VARS = 100;

    bdd_mgr_t *mgr = bdd_mgr_create (NUM_VARS);

    TEST_NAME ("proper number of variables");
    ASSERT(bdd_mgr_get_num_vars(mgr) == NUM_VARS);
    TEST_NAME ("proper number of starting nodes");
    ASSERT(bdd_mgr_get_num_nodes(mgr) == 2);

    TEST_NAME ("proper variable indexes");
    for (unsigned i = 0; i < NUM_VARS; i += 1) {
        const bdd_t ivar = bdd_mgr_get_ith_var (mgr, i);
        ASSERT (i + 3 == ivar);
    }
    const unsigned old_num_nodes = bdd_mgr_get_num_nodes(mgr);
    for (unsigned i = 0; i < NUM_VARS; i += 1) {
        const unsigned idx = NUM_VARS - i - 1;
        const bdd_t ivar = bdd_mgr_get_ith_var (mgr, idx);
        ASSERT (idx + 2 == ivar);
    }
    const unsigned new_num_nodes = bdd_mgr_get_num_nodes(mgr);
    TEST_NAME ("proper number of nodes");
    ASSERT (old_num_nodes == new_num_nodes);

    TEST_NAME ("idempotent conjunction");
    const bdd_t var5 = bdd_mgr_get_ith_var (mgr, 5);
    const bdd_t var5_and_var5 = bdd_apply (mgr, BDD_AND, var5, var5);
    ASSERT (var5 == var5_and_var5);

    TEST_NAME ("idempotent disjunction");
    const bdd_t var5_or_var5 = bdd_apply (mgr, BDD_OR, var5, var5);
    ASSERT (var5 == var5_or_var5);

    TEST_NAME ("variable restriction");
    ASSERT (bdd_restrict_var (mgr, var5, 5, true) == bdd_true);
    ASSERT (bdd_restrict_var (mgr, var5, 5, false) == bdd_false);

    bdd_mgr_destroy (mgr);

    fprintf (stderr, "%u tests passed\n", num_tests);
    exit(EXIT_SUCCESS);
}
