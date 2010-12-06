#include <stdlib.h>
#include "bool_expr.h"

bool_expr_t *
new_bool_expr_t (unsigned num_variables)
{
    bool_expr_t *expr = (bool_expr_t *) malloc (sizeof(bool_expr_t));
    expr->num_variables = num_variables;
    expr->variables = (bool *) calloc (num_variables, sizeof(bool));
    expr->connectives =
        (bdd_apply_binop *) calloc (num_variables, sizeof(bdd_apply_binop));
    return expr;
}

void
delete_bool_expr_t (bool_expr_t *expr)
{
    free(expr->connectives);
    free(expr->variables);
    free(expr);
}

static bool
eval_binop (bdd_apply_binop op, bool b1, bool b2)
{
    bool result;
    switch (op) {
    case BDD_AND:
        result = b1 && b2;
        break;
    case BDD_OR:
        result = b1 || b2;
        break;
    case BDD_XOR:
        result = (b1 && !b2) || (!b1 && b2);
        break;
    case BDD_EQUIV:
        result = (b1 && b2) || (!b1 && !b2);
        break;
    case BDD_NAND:
        result = !(b1 && b2);
        break;
    case BDD_NOR:
        result = !(b1 || b2);
        break;
    case BDD_IMPLIES:
        result = !b1 || b2;
        break;
    default:
        abort ();
        break;
    }
    return result;
}

bool
bool_expr_eval (bool_expr_t *expr)
{
    if (expr->num_variables == 0)
        return true;
    bool result = expr->variables[0];
    for (unsigned i = 0; i < expr->num_variables-1; i += 1)
        result = eval_binop (expr->connectives[i], result, expr->variables[i+1]);
    return result;
}

bdd_t
bool_expr_eval_bdd (bool_expr_t *expr, bdd_mgr_t *mgr)
{
    if (expr->num_variables == 0)
        return bdd_true;

    bdd_t result = bdd_restrict (mgr,
                                 bdd_mgr_get_ith_var (mgr, 0),
                                 0,
                                 expr->variables[0]);

    for (unsigned i = 0; i < expr->num_variables-1; i += 1) {
        const bdd_t nextvar = bdd_restrict (mgr,
                                            bdd_mgr_get_ith_var (mgr, i+1),
                                            i+1,
                                            expr->variables[i+1]);
        result = bdd_apply (mgr,
                            expr->connectives[i],
                            result,
                            nextvar);
    }
    return result;
}

/**********************************************************************/
/* TRUTH TABLE COMPARISON                                             */
/**********************************************************************/
/* Takes a boolean funcion encoded as an array of n variables and an
   array of n-1 binary operations, and checks that the truth table is
   equivalent to the given BDD. */
static bool
tt_equal_terminal (bool_expr_t *expr, bdd_mgr_t *mgr)
{
    const bdd_t b = bool_expr_eval_bdd (expr, mgr);
    const bool eval = bool_expr_eval (expr);
    return ((eval && b == bdd_true) || (!eval && b == bdd_false));
}

static bool
tt_equal_helper (bool_expr_t *expr, bdd_mgr_t *mgr, unsigned i)
{
    if (i == expr->num_variables) {
        return tt_equal_terminal (expr, mgr);
    }
    else {
        expr->variables[i] = true;
        const bool when_true = tt_equal_helper (expr, mgr, i+1);
        expr->variables[i] = false;
        const bool when_false = tt_equal_helper (expr, mgr, i+1);
        return when_true && when_false;
    }
}

/* For the given expression, checks that every line of a truth table
   for it agrees with the BDD resulting from symbolically evaluating
   the expression. */
bool
tt_equal (bool_expr_t *expr, bdd_mgr_t *mgr)
{
    return tt_equal_helper (expr, mgr, 0);
}

void
bool_expr_print (FILE *handle, bool_expr_t *expr)
{
    if (expr->num_variables == 0) {
        fprintf (handle, "FALSE");
        return;
    }

    for (unsigned i = 0; i < expr->num_variables - 1; i += 1)
        fprintf (handle, "(");
    fprintf (handle, "x0");

    for (unsigned i = 0; i < expr->num_variables - 1; i += 1) {
        const char *opstr;
        switch (expr->connectives[i]) {
        case BDD_AND:
            opstr = "AND";
            break;
        case BDD_OR:
            opstr = "OR";
            break;
        case BDD_XOR:
            opstr = "XOR";
            break;
        case BDD_EQUIV:
            opstr = "EQUIV";
            break;
        case BDD_NAND:
            opstr = "NAND";
            break;
        case BDD_NOR:
            opstr = "NOR";
            break;
        case BDD_IMPLIES:
            opstr = "IMPLIES";
            break;
        default:
            abort ();
            break;
        }

        fprintf (handle, " %s x%u)", opstr, i+1);
    }
}
