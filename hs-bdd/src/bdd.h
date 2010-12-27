/* A library for binary decision diagrams.
 *
 * Many operations are given with worst-case complexity measures,
 * based on the number of nodes in the BDD arguments.  The function
 * m(f) denotes the number of nodes in the BDD f.
 */

#ifndef BDD_INCLUDED
#define BDD_INCLUDED

/* FIXME: this conditional compilation is ugly and not so safe, as the
 * Haskell bindings to this library make assumptions about the
 * representation of 'bool'. */
#ifndef __cplusplus
typedef int bool;
enum {true = 1, false = 0};
#endif

/**********************************************************************/
/* BDD type and operations                                            */
/**********************************************************************/

/* A BDD is represented by its corresponding node index.  Ideally,
 * this type would be abstract. */
typedef int bdd_t;

static inline bool
bdd_equal (bdd_t b1, bdd_t b2)
{
    return b1 == b2;
}

static inline unsigned
bdd_hash (bdd_t b)
{
    return b;
}

/* True and false BDD literals. */
enum {bdd_false = 0, bdd_true = 1};

/**********************************************************************/
/* BDD manager type and operations                                    */
/**********************************************************************/

/* A manager of the state for multiple BDDs. */
typedef struct bdd_mgr bdd_mgr_t;

/* Creates and initializes a new BDD manager with the given number of
 * variables.  O(num_vars) time.  */
extern bdd_mgr_t *
bdd_mgr_create (unsigned num_vars);

/* Creates and initializes a new BDD manager with the given number of
 * variables and with a hint for initial number of nodes to allocate.
 * O(num_vars) time. */
extern bdd_mgr_t *
bdd_mgr_create_with_hint (unsigned num_vars, unsigned capacity_hint);

/* FIXME: these complexity measures are wrong. */
/* Frees the resources used by the BDD manager.  O(1) time and
 * space. */
extern void
bdd_mgr_destroy (bdd_mgr_t *mgr);

/* Returns the number of variables in the given BDD manager.  O(1)
 * time and space. */
extern unsigned
bdd_mgr_get_num_vars (bdd_mgr_t *mgr);

/* Returns the number of BDD nodes in the given manager.  O(1) time
 * and space. */
extern unsigned
bdd_mgr_get_num_nodes (bdd_mgr_t *mgr);

/* Gets the variable of the root of the BDD. */
extern unsigned
bdd_var (bdd_mgr_t *mgr, bdd_t b);

/* Gets the low branch of the root of the BDD. */
extern bdd_t
bdd_low (bdd_mgr_t *mgr, bdd_t b);

/* Gets the high branch of the root of the BDD. */
extern bdd_t
bdd_high (bdd_mgr_t *mgr, bdd_t b);

/* Returns a BDD representing the given variable.  The variable must
 * exist in the manager.  O(1) time and space. */
extern bdd_t
bdd_ith_var (bdd_mgr_t *mgr, unsigned i);

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

/* Computes if-then-else of the given BDDs. */
extern bdd_t
bdd_ite (bdd_mgr_t *mgr, bdd_t p, bdd_t t, bdd_t f);

/* Restricts the given BDD by assigning a value to a variable.
 * O(m(b)) time and space. */
extern bdd_t
bdd_restrict (bdd_mgr_t *mgr, bdd_t b, unsigned var, bool val);

/* Performs existential instantiation on the given variable and BDD.
 * O(m(b)^2) time and space.  */
extern bdd_t
bdd_existential (bdd_mgr_t *mgr, unsigned var, bdd_t b);

/* Performs universal instantiation on the given variable and BDD.
 * O(m(b)^2) time and space.  */
extern bdd_t
bdd_universal (bdd_mgr_t *mgr, unsigned var, bdd_t b);

/* Composes two BDDs f and g on variable x, yielding f with x replaced
 * by g.  O(m(f)^2 * m(g)^2) time and space. */
extern bdd_t
bdd_compose (bdd_mgr_t *mgr, bdd_t f, unsigned x, bdd_t g);

/* Returns the number of satisfying solutions of b. */
extern double
bdd_sat_count (bdd_mgr_t *mgr, bdd_t b);

/* Returns the number of nodes used by b.  O(m(b)) time, O(log m(b))
 * space. */
extern unsigned
bdd_get_num_nodes (bdd_mgr_t *mgr, bdd_t b);

#endif  /* BDD_INCLUDED */
