#include "bdd_mgr.h"

static inline void
swap_unsigned (unsigned *i, unsigned *j)
{
    unsigned tmp = *i;
    *i = *j;
    *j = tmp;
}

/* static void */
/* bdd_mgr_var_order_fprint (bdd_mgr_t *mgr, FILE *handle) */
/* { */
/*     unsigned i; */
/*     fprintf (handle, "%u", mgr->lvl_to_var[0]); */
/*     for (i = 1; i < mgr->num_vars; i += 1) */
/*         fprintf (handle, " %u", mgr->lvl_to_var[i]); */
/* } */

void
bdd_mgr_swap_variables (bdd_mgr_t *mgr, unsigned idx)
{
    assert (0);
}
