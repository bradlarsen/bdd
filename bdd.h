#ifndef BDD_INCLUDED
#define BDD_INCLUDED

/* A manager of the state used to implement BDDs.  A manager
 * represents multiple BDDs, stored in the same graph. */
typedef struct bdd_manager bdd_manager_t;

/* BDDs are represented by the index of a node.  Ideally, this type
 * would be abstract. */
typedef unsigned bdd;

/* True and false literals. */
extern const bdd bdd_true;
extern const bdd bdd_false;

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

/* Returns a BDD representing the given variable. */
extern bdd
bdd_manager_get_ith_var (bdd_manager_t *mgr, unsigned i);


/* Binary operations on BDDs to be used with bdd_apply. */
typedef enum {BDD_AND, BDD_OR, BDD_EQUIV} bdd_apply_binop;

/* Apply a binary operation to the two given BDDs. */
extern bdd
bdd_apply (bdd_manager_t *mgr, bdd_apply_binop op, bdd b1, bdd b2);


#endif  /* BDD_INCLUDED */
