#ifndef BDD_INCLUDED
#define BDD_INCLUDED

#include <stdbool.h>

/* A manager of the state for multiple BDDs. */
typedef struct bdd_mgr bdd_mgr_t;

/* A BDD is represented by its corresponding node index.  Ideally,
 * this type would be abstract. */
typedef unsigned bdd_t;

/* True and false literals. */
enum {bdd_false = 0, bdd_true = 1};

/* Creates and initializes a new BDD manager with the given number of
 * variables. */
extern bdd_mgr_t *
bdd_mgr_create (unsigned num_vars);

/* Frees the resources used by the BDD manager. */
extern void
bdd_mgr_destroy (bdd_mgr_t *mgr);

/* Returns the number of variables in the given BDD manager. */
extern unsigned
bdd_mgr_get_num_vars (bdd_mgr_t *mgr);

/* Returns the number of BDD nodes in the given manager. */
extern unsigned
bdd_mgr_get_num_nodes (bdd_mgr_t *mgr);

/* Returns a BDD representing the given variable.  The variable must
 * exist in the manager. */
extern bdd_t
bdd_mgr_get_ith_var (bdd_mgr_t *mgr, unsigned i);

/* Returns a BDD representing the negation of the given variable.  The
 * variable must exist in the manager. */
extern bdd_t
bdd_mgr_get_nith_var (bdd_mgr_t *mgr, unsigned i);

/* Binary operations to be used with bdd_apply. */
typedef enum {
    BDD_AND,
    BDD_OR,
    BDD_XOR,
    BDD_EQUIV,
    BDD_NAND,
    BDD_NOR,
    BDD_IMPLIES
} bdd_apply_binop;

/* Applies a binary operation to the two given BDDs. */
extern bdd_t
bdd_apply (bdd_mgr_t *mgr, bdd_apply_binop op, bdd_t b1, bdd_t b2);

/* Restricts the given BDD by assigning a value to a variable. */
extern bdd_t
bdd_restrict (bdd_mgr_t *mgr, bdd_t b, unsigned var, bool val);

#endif  /* BDD_INCLUDED */
