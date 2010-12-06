/* A library for binary decision diagrams.
 *
 * Many operations are given with worst-case complexity measures,
 * based on the number of nodes in the BDD arguments.  The function
 * m(f) denotes the number of nodes in the BDD f.
 */

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
 * variables.  O(num_vars) time.  */
extern bdd_mgr_t *
bdd_mgr_create (unsigned num_vars);

/* Frees the resources used by the BDD manager.  O(1) time. */
extern void
bdd_mgr_destroy (bdd_mgr_t *mgr);

/* Returns the number of variables in the given BDD manager.  O(1)
 * time. */
extern unsigned
bdd_mgr_get_num_vars (bdd_mgr_t *mgr);

/* Returns the number of BDD nodes in the given manager.  O(1). */
extern unsigned
bdd_mgr_get_num_nodes (bdd_mgr_t *mgr);

/* Returns a BDD representing the given variable.  The variable must
 * exist in the manager.  O(1) time and space. */
extern bdd_t
bdd_mgr_get_ith_var (bdd_mgr_t *mgr, unsigned i);

/* Returns a BDD representing the negation of the given variable.  The
 * variable must exist in the manager.  O(1) time and space. */
extern bdd_t
bdd_mgr_get_nith_var (bdd_mgr_t *mgr, unsigned i);

/* Binary operations to be used with bdd_apply. */
typedef enum {
    BDD_AND,
    BDD_OR,
    BDD_XOR,
    BDD_EQUIV,
    BDD_NAND,
    BDD_IMPLIES
} bdd_apply_binop;

/* Applies a binary operation to the two given BDDs.  O(m(b1) * m(b2))
 * time and space. */
extern bdd_t
bdd_apply (bdd_mgr_t *mgr, bdd_apply_binop op, bdd_t b1, bdd_t b2);

/* Computes the logical AND of the two BDDs.  O(m(b1) * m(b2)) time
 * and space. */
extern bdd_t
bdd_and (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2);

/* Computes the logical OR of the two BDDs.  O(m(b1) * m(b2)) time and
 * space. */
extern bdd_t
bdd_or (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2);

/* Computes the logical XOR of the two BDDs.  O(m(b1) * m(b2)) time
 * and space. */
extern bdd_t
bdd_xor (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2);

/* Computes the logical EQUIV of the two BDDs.  O(m(b1) * m(b2)) time
 * and space. */
extern bdd_t
bdd_equiv (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2);

/* Computes the logical NAND of the two BDDs.  O(m(b1) * m(b2)) time
 * and space. */
extern bdd_t
bdd_nand (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2);

/* Computes the logical IMPLIES of the two BDDs.  O(m(b1) * m(b2))
 * time and space. */
extern bdd_t
bdd_implies (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2);

/* Computes the logical negation of the given BDD.  O(m(b)) time and
 * space. */
extern bdd_t
bdd_not (bdd_mgr_t *mgr, bdd_t b);

/* Restricts the given BDD by assigning a value to a variable.
 * O(m(b)) time and space. */
extern bdd_t
bdd_restrict (bdd_mgr_t *mgr, bdd_t b, unsigned var, bool val);

/* Performs existential quantification on the given variable and
 * BDD.  O(m(b)^2) time and space.  */
extern bdd_t
bdd_existential (bdd_mgr_t *mgr, unsigned var, bdd_t b);

/* Performs universal quantification on the given variable and
 * BDD.  O(m(b)^2) time and space.  */
extern bdd_t
bdd_universal (bdd_mgr_t *mgr, unsigned var, bdd_t b);

/* Composes two BDDs f and g on variable x, yielding f with x replaced
 * by g.  O(m(f)^2 * m(g)^2) time and space. */
extern bdd_t
bdd_compose (bdd_mgr_t *mgr, bdd_t f, unsigned x, bdd_t g);

#endif  /* BDD_INCLUDED */
