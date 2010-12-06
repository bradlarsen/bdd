/**********************************************************************/
/* White-box tests for the BDD library.                               */
/**********************************************************************/
#include <stdio.h>
#include <stdlib.h>

#include "bdd.h"
#include "test_infrastructure.h"
#include "bool_expr.h"

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
        ASSERT (i + 2 == ivar);
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

    TEST_NAME ("expression correctness of x0");
    bool_expr_t *expr = new_bool_expr_t (1);
    bool_expr_print (stderr, expr);
    tt_equal (expr, mgr);
    delete_bool_expr_t (expr);

    TEST_NAME ("expression correctness of (x0 AND x1)");
    expr = new_bool_expr_t (2);
    expr->connectives[0] = BDD_AND;
    bool_expr_print (stderr, expr);
    tt_equal (expr, mgr);
    delete_bool_expr_t (expr);

    TEST_NAME ("expression correctness of ((x0 AND x1) OR x2)");
    expr = new_bool_expr_t (3);
    expr->connectives[0] = BDD_AND;
    expr->connectives[1] = BDD_OR;
    bool_expr_print (stderr, expr);
    tt_equal (expr, mgr);
    delete_bool_expr_t (expr);

    TEST_NAME ("expression correctness of (((x0 AND x1) OR x2) AND x3)");
    expr = new_bool_expr_t (4);
    expr->connectives[0] = BDD_AND;
    expr->connectives[1] = BDD_OR;
    expr->connectives[2] = BDD_AND;
    bool_expr_print (stderr, expr);
    tt_equal (expr, mgr);
    delete_bool_expr_t (expr);

    bdd_mgr_destroy (mgr);

    fprintf (stderr, "PASS\n");
    fprintf (stderr, "\n%u tests passed\n", num_tests);
    exit(EXIT_SUCCESS);
}
