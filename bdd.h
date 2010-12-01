#ifndef BDD_INCLUDED
#define BDD_INCLUDED

typedef struct bdd_manager bdd_manager_t;

extern bdd_manager_t *
bdd_manager_create (unsigned num_vars);

extern void
bdd_manager_destroy (bdd_manager_t *mgr);

extern unsigned
bdd_manager_get_num_vars (bdd_manager_t *mgr);

extern unsigned
bdd_manager_get_num_nodes (bdd_manager_t *mgr);


typedef unsigned bdd;

/* typedef enum {BDD_AND, BDD_OR, BDD_EQUIV} bdd_binop; */

/* extern bdd */
/* bdd_apply (bdd_manager *mgr, bdd_binop op, bdd b1, bdd b2); */

#endif  /* BDD_INCLUDED */
