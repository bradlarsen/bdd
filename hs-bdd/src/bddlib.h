/* A library for binary decision diagrams.
 *
 * Many operations are given with worst-case complexity measures,
 * based on the number of nodes in the BDD arguments.  The function
 * m(f) denotes the number of nodes in the BDD f.
 */

#ifndef BDDLIB_INCLUDED
#define BDDLIB_INCLUDED

#include <stdio.h>
#include "boolean.h"

/**********************************************************************/
/* BDD manager type and operations                                    */
/**********************************************************************/
/* A manager of the state for multiple BDDs. */
typedef struct bdd_mgr bdd_mgr_t;

/* Creates and initializes a new BDD manager with the given number of
 * variables.  */
extern bdd_mgr_t *
bdd_mgr_create (unsigned num_vars);

/* Creates and initializes a new BDD manager with the given number of
 * variables and with a hint for initial number of nodes to
 * allocate. */
extern bdd_mgr_t *
bdd_mgr_create_with_hint (unsigned num_vars, unsigned capacity_hint);

/* Frees the resources used by the BDD manager. */
extern void
bdd_mgr_destroy (bdd_mgr_t *mgr);

/* Returns the number of variables in the given manager.  O(1) time
 * and space. */
extern unsigned
bdd_mgr_get_num_vars (bdd_mgr_t *mgr);

/* Returns the number of BDD nodes in the given manager.  O(1) time
 * and space. */
extern unsigned
bdd_mgr_get_num_nodes (bdd_mgr_t *mgr);

/* Returns the number of allocated nodes in the given manager.  O(1)
 * time and space. */
extern unsigned
bdd_mgr_get_num_allocated (bdd_mgr_t *mgr);

/**********************************************************************/
/* Garbage collection-related types and functions                     */
/**********************************************************************/
typedef struct
{
    unsigned usr_bdd_num_copied;
    unsigned usr_bdd_num_collected;
    unsigned raw_bdd_num_copied;
    unsigned raw_bdd_num_collected;
} bdd_gc_stats_t;

/* Pretty-prints the garbage collection statistics to the given file
 * handle. */
extern void
bdd_gc_stats_fprint (FILE *handle, bdd_gc_stats_t stats);

/* Performs a garbage collection on the graph represented by the
 * manager. BDDs that are not referenced directly or indirectly will
 * be freed. */
extern void
bdd_mgr_perform_gc (bdd_mgr_t *mgr);

/**********************************************************************/
/* BDD type, constants, and accessors                                 */
/**********************************************************************/
typedef struct bdd bdd_t;

/* Gets the variable of the root of the BDD. */
extern unsigned
bdd_var (bdd_mgr_t *mgr, bdd_t *b);

/* Gets the low branch of the root of the BDD. */
extern bdd_t *
bdd_low (bdd_mgr_t *mgr, bdd_t *b);

/* Gets the high branch of the root of the BDD. */
extern bdd_t *
bdd_high (bdd_mgr_t *mgr, bdd_t *b);

/* The BDD representing truth. */
extern bdd_t *
bdd_false (bdd_mgr_t *mgr);

/* The BDD representing falsity. */
extern bdd_t *
bdd_true (bdd_mgr_t *mgr);

/**********************************************************************/
/* BDD reference counting operations                                  */
/**********************************************************************/
/* Increases the reference count of the given BDD. */
extern void
bdd_inc_ref (bdd_mgr_t *mgr, bdd_t *b);

/* Decreases the reference count of the given BDD.  The reference
 * count of the given BDD must be positive. */
extern void
bdd_dec_ref (bdd_mgr_t *mgr, bdd_t *b);

/**********************************************************************/
/* BDD operations                                                     */
/**********************************************************************/
/* Returns a BDD representing the given variable.  The variable must
 * exist in the manager.  O(1) time and space. */
extern bdd_t *
bdd_ith_var (bdd_mgr_t *mgr, unsigned i);

/* Computes the logical AND of the two BDDs.  O(m(b1) * m(b2)) time
 * and space. */
extern bdd_t *
bdd_and (bdd_mgr_t *mgr, bdd_t *b1, bdd_t *b2);

/* Computes the logical OR of the two BDDs.  O(m(b1) * m(b2)) time and
 * space. */
extern bdd_t *
bdd_or (bdd_mgr_t *mgr, bdd_t *b1, bdd_t *b2);

/* Computes the logical XOR of the two BDDs.  O(m(b1) * m(b2)) time
 * and space. */
extern bdd_t *
bdd_xor (bdd_mgr_t *mgr, bdd_t *b1, bdd_t *b2);

/* Computes the logical EQUIV of the two BDDs.  O(m(b1) * m(b2)) time
 * and space. */
extern bdd_t *
bdd_equiv (bdd_mgr_t *mgr, bdd_t *b1, bdd_t *b2);

/* Computes the logical NAND of the two BDDs.  O(m(b1) * m(b2)) time
 * and space. */
extern bdd_t *
bdd_nand (bdd_mgr_t *mgr, bdd_t *b1, bdd_t *b2);

/* Computes the logical IMPLIES of the two BDDs.  O(m(b1) * m(b2))
 * time and space. */
extern bdd_t *
bdd_implies (bdd_mgr_t *mgr, bdd_t *b1, bdd_t *b2);

/* Computes the logical negation of the given BDD.  O(m(b)) time and
 * space. */
extern bdd_t *
bdd_not (bdd_mgr_t *mgr, bdd_t *b);

/* Computes if-then-else of the given BDDs. */
extern bdd_t *
bdd_ite (bdd_mgr_t *mgr, bdd_t *p, bdd_t *t, bdd_t *f);

/* Restricts the given BDD by assigning a value to a variable.
 * O(m(b)) time and space. */
extern bdd_t *
bdd_restrict (bdd_mgr_t *mgr, bdd_t *b, unsigned var, boolean val);

/* Performs existential instantiation on the given variable and BDD.
 * O(m(b)^2) time and space.  */
extern bdd_t *
bdd_existential (bdd_mgr_t *mgr, unsigned var, bdd_t *b);

/* Performs universal instantiation on the given variable and BDD.
 * O(m(b)^2) time and space.  */
extern bdd_t *
bdd_universal (bdd_mgr_t *mgr, unsigned var, bdd_t *b);

/* Composes two BDDs f and g on variable x, yielding f with x replaced
 * by g.  O(m(f)^2 * m(g)^2) time and space. */
extern bdd_t *
bdd_compose (bdd_mgr_t *mgr, bdd_t *f, unsigned x, bdd_t *g);

/* Returns the number of satisfying solutions of b. */
extern double
bdd_sat_count (bdd_mgr_t *mgr, bdd_t *b);

/* Returns the number of nodes used by b.  O(m(b)) time, O(log m(b))
 * space. */
extern unsigned
bdd_get_num_nodes (bdd_mgr_t *mgr, bdd_t *b);

#endif  /* BDDLIB_INCLUDED */
