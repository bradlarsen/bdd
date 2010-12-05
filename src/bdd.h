#ifndef BDD_INCLUDED
#define BDD_INCLUDED

#include <stdbool.h>

/* A manager of BDDs.  A manager represents multiple BDDs. */
typedef struct bdd_manager bdd_manager_t;

/* BDDs are represented by the index of a node.  Ideally, this type
 * would be abstract. */
typedef unsigned bdd_t;

/* True and false literals. */
enum {bdd_false = 0, bdd_true = 1};

/* Creates and initializes a new BDD manager with the given number of
 * variables. */
extern bdd_manager_t *
bdd_manager_create (unsigned num_vars);

/* Frees the resources used by the BDD manager. */
extern void
bdd_manager_destroy (bdd_manager_t *mgr);

/* Returns the number of variables in the given BDD manager. */
extern unsigned
bdd_manager_get_num_vars (bdd_manager_t *mgr);

/* Returns the number of BDD nodes in the given manager. */
extern unsigned
bdd_manager_get_num_nodes (bdd_manager_t *mgr);

/* Returns a BDD representing the given variable.  The variable must
 * be less than the number of variables in the manager. */
extern bdd_t
bdd_manager_get_ith_var (bdd_manager_t *mgr, unsigned i);


/* Binary operations on BDDs to be used with bdd_apply. */
typedef enum {BDD_AND, BDD_OR, BDD_EQUIV} bdd_apply_binop;

/* Apply a binary operation to the two given BDDs, which must be valid
 * BDDs for the given manager. */
extern bdd_t
bdd_apply (bdd_manager_t *mgr, bdd_apply_binop op, bdd_t b1, bdd_t b2);

/* Restricts the given BDD by assigning a value to a variable.  The
   variable must be valid for the manager. */
extern bdd_t
bdd_restrict_var (bdd_manager_t *mgr, bdd_t b, unsigned var, bool val);

#endif  /* BDD_INCLUDED */
