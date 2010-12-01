#ifndef BDD_INCLUDED
#define BDD_INCLUDED

typedef struct bdd_manager bdd_manager;
/* typedef int bdd; */
/* typedef enum {BDD_AND, BDD_OR, BDD_EQUIV} bdd_binop; */

extern bdd_manager *
bdd_manager_create ();

extern void
bdd_manager_destroy (bdd_manager *mgr);

/* extern bdd */
/* bdd_apply (bdd_manager *mgr, bdd_binop op, bdd b1, bdd b2); */

#endif  /* BDD_INCLUDED */
