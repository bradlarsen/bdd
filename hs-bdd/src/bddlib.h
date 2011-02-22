/* A library for binary decision diagrams.
 *
 * Many operations are given with worst-case complexity measures,
 * based on the number of nodes in the BDD arguments.  We write '|f|'
 * to denote the number of nodes in BDD f.
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

/* Creates and initializes a new BDD manager with the specified number
 * of variables, which must be positive. */
extern bdd_mgr_t *
bdd_mgr_create (unsigned num_vars);

/* Creates and initializes a new BDD manager with the specified number
 * of variables and with a hint for initial number of nodes to
 * allocate. */
extern bdd_mgr_t *
bdd_mgr_create_with_hint (unsigned num_vars, unsigned capacity_hint);

/* Frees the resources used by the BDD manager. */
extern void
bdd_mgr_destroy (bdd_mgr_t *mgr);

/* Returns the number of variables in the manager.  O(1) time and space. */
extern unsigned
bdd_mgr_get_num_vars (bdd_mgr_t *mgr);

/* Returns the number of BDD nodes in the manager.  O(1) time and space. */
extern unsigned
bdd_mgr_get_num_nodes (bdd_mgr_t *mgr);

/* Returns the number of BDD nodes at the given level in the manager.
 * O(1) time and space. */
extern unsigned
bdd_mgr_get_num_nodes_at_level (bdd_mgr_t *mgr, unsigned lvl);

/* Returns the number of allocated nodes in the manager.  O(1) time
 * and space. */
extern unsigned
bdd_mgr_get_num_allocated (bdd_mgr_t *mgr);

/* Swaps the order of variables indexed by i and i+1.  The value of
 * idx must be less than bdd_mgr_get_num_vars (mgr) - 1. */
extern void
bdd_mgr_swap_variables (bdd_mgr_t *mgr, unsigned idx);

/**********************************************************************/
/* Result cache types and functions                                   */
/**********************************************************************/
typedef struct
{
    unsigned num_lookups;       /* the number of cache lookups */
    unsigned num_hits;          /* the number of cache hits */
    unsigned num_inserts;       /* the number of cache insertions */
} bdd_cache_stats_t;

/* Pretty-prints the cache statistics to the file handle. */
extern void
bdd_cache_stats_fprint (FILE *handle, bdd_cache_stats_t stats);

/* Gets the cache statistics for the manager. */
extern bdd_cache_stats_t
bdd_mgr_get_cache_stats (bdd_mgr_t *mgr);

/**********************************************************************/
/* BDD type, constants, and accessors                                 */
/**********************************************************************/
typedef unsigned bdd_t;
enum {bdd_false = 0, bdd_true = 1};

/* Gets the variable of the root of the BDD. */
extern unsigned
bdd_var (bdd_mgr_t *mgr, bdd_t b);

/* Gets the variable level of the root of the BDD. */
extern unsigned
bdd_level (bdd_mgr_t *mgr, bdd_t b);

/* Gets the low branch of the root of the BDD. */
extern bdd_t 
bdd_low (bdd_mgr_t *mgr, bdd_t b);

/* Gets the high branch of the root of the BDD. */
extern bdd_t 
bdd_high (bdd_mgr_t *mgr, bdd_t b);

/**********************************************************************/
/* BDD reference counting operations                                  */
/**********************************************************************/
/* Increases the reference count of the BDD. */
extern void
bdd_inc_ref (bdd_mgr_t *mgr, bdd_t b);

/* Decreases the reference count of the BDD.  The reference count of
 * the BDD must be positive. */
extern void
bdd_dec_ref (bdd_mgr_t *mgr, bdd_t b);

extern void
bdd_ptr_dec_ref (bdd_mgr_t *mgr, bdd_t *b);

/**********************************************************************/
/* BDD operations                                                     */
/**********************************************************************/
/* Returns a BDD representing the variable 'i', which must be less
 * than the number of variables in the manager.  O(1) time and
 * space. */
extern bdd_t 
bdd_ith_var (bdd_mgr_t *mgr, unsigned i);

/* Computes the logical AND of the two BDDs.  O(|b1| * |b2|) time and
 * space. */
extern bdd_t 
bdd_and (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2);

/* Computes the logical OR of the two BDDs.  O(|b1| * |b2|) time and
 * space. */
extern bdd_t 
bdd_or (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2);

/* Computes the logical XOR of the two BDDs.  O(|b1| * |b2|) time and
 * space. */
extern bdd_t 
bdd_xor (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2);

/* Computes the logical EQUIV of the two BDDs.  O(|b1| * |b2|) time
 * and space. */
extern bdd_t 
bdd_equiv (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2);

/* Computes the logical NAND of the two BDDs.  O(|b1| * |b2|) time and
 * space. */
extern bdd_t 
bdd_nand (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2);

/* Computes the logical IMPLIES of the two BDDs.  O(|b1| * |b2|) time
 * and space. */
extern bdd_t 
bdd_implies (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2);

/* Computes the logical negation of the BDD.  O(|b|) time and
 * space. */
extern bdd_t 
bdd_not (bdd_mgr_t *mgr, bdd_t b);

/* Computes if-then-else of the BDDs. */
extern bdd_t 
bdd_ite (bdd_mgr_t *mgr, bdd_t p, bdd_t t, bdd_t f);

/* Restricts the BDD by assigning a value to a variable.  O(|b|) time
 * and space. */
extern bdd_t 
bdd_restrict (bdd_mgr_t *mgr, bdd_t b, unsigned var, boolean val);

/* Performs existential instantiation on the variable and BDD.
 * O(|b| ^ 2) time and space.  */
extern bdd_t 
bdd_existential (bdd_mgr_t *mgr, unsigned var, bdd_t b);

/* Performs universal instantiation on the variable and BDD.
 * O(|b| ^ 2) time and space.  */
extern bdd_t 
bdd_universal (bdd_mgr_t *mgr, unsigned var, bdd_t b);

/* Composes two BDDs f and g on variable x, yielding f with x replaced
 * by g.  O(|f| ^ 2 * |g| ^ 2) time and space. */
extern bdd_t 
bdd_compose (bdd_mgr_t *mgr, bdd_t f, unsigned x, bdd_t g);

/* Returns the number of satisfying solutions of b. */
extern double
bdd_sat_count (bdd_mgr_t *mgr, bdd_t b);

/* /\* Returns the number of nodes that appear within the given BDD. *\/ */
/* unsigned */
/* bdd_get_num_nodes (bdd_mgr_t *mgr, bdd_t b); */

#endif  /* BDDLIB_INCLUDED */
