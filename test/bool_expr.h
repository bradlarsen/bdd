/**********************************************************************/
/* BOOLEAN EXPRESSIONS                                                */
/**********************************************************************/

#ifndef BOOL_EXPR_INCLUDED
#define BOOL_EXPR_INCLUDED

#include <stdio.h>
#include "bdd.h"

/* A Boolean expression, represented as an array of num_variables
   variables, and an array of num_variables - 1 binary connectives, to
   be interpreted as though the connectives were arranged between the
   variables, and the expression was parenthesized most deeply to the
   left. */
typedef struct
{
    unsigned num_variables;
    bool *variables;
    bdd_apply_binop *connectives;
} bool_expr_t;

/* Allocates a new bool_expr_t.  The returned structure is
   uninitialized. */
extern bool_expr_t *
new_bool_expr_t (unsigned num_variables);

/* Frees a bool_expr_t. */
extern void
delete_bool_expr_t (bool_expr_t *expr);

/* Evaluate the given expression. */
extern bool
bool_expr_eval (bool_expr_t *expr);

/* Symbolically evaluate a BDD corresponding to the given expression.
   The BDD manager must have at least as many variables as are used in
   the expression. */
extern bdd_t
bool_expr_eval_bdd (bool_expr_t *expr, bdd_mgr_t *mgr);

/* Returns true if and only if evaluating the expression as a Boolean
   and symbolically evaluating the expression as a BDD come out
   equivalently. */
extern bool
tt_equal (bool_expr_t *expr, bdd_mgr_t *mgr);

/* Pretty-prints the Boolean expression to the given file handle. */
extern void
bool_expr_print (FILE *handle, bool_expr_t *expr);

#endif /* BOOL_EXPR_INCLUDED */
