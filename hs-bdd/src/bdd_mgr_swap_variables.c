#include "bdd_mgr.h"

static inline void
swap_unsigned (unsigned *i, unsigned *j)
{
    unsigned tmp = *i;
    *i = *j;
    *j = tmp;
}

static void
bdd_mgr_var_order_fprint (bdd_mgr_t *mgr, FILE *handle)
{
    unsigned i;
    fprintf (handle, "%u", mgr->idx_to_var[0]);
    for (i = 1; i < mgr->num_vars; i += 1)
        fprintf (handle, " %u", mgr->idx_to_var[i]);
}

static void
swap_one_node (bdd_mgr_t *mgr, int idx, node_t *f)
{
    node_t f_low, f_high;
    raw_bdd_t new_low, new_high;
    f_low = mgr->nodes[f->low];
    f_high = mgr->nodes[f->high];

    if (f_low.idx > idx + 1 && f_high.idx > idx + 1) {
        new_low = f->low;
        new_high = f->high;
    }
    else if (f_low.idx == idx + 1 && f_high.idx > idx + 1) {
        new_low = _bdd_make_node (mgr, idx + 1, f_low.low, f->high);
        new_high = _bdd_make_node (mgr, idx + 1, f_low.high, f->high);
    }
    else if (f_low.idx > idx + 1 && f_high.idx == idx + 1) {
        new_low = _bdd_make_node (mgr, idx + 1, f->low, f_high.low);
        new_high = _bdd_make_node (mgr, idx + 1, f->low, f_high.high);
    }
    else {
        new_low = _bdd_make_node (mgr, idx + 1, f_low.low, f_high.low);
        new_high = _bdd_make_node (mgr, idx + 1, f_low.high, f_high.high);
    }

    /* FIXME: bug here! */
    /* In-place update sometimes result in duplicated nodes. */
    /* In-place update invalidates the node hashes. */
    assert (new_low != new_high);
    f->low = new_low;
    f->high = new_high;
}

/* FIXME: modify so only nodes at given level need be examined */
/* FIXME: modify so out-of-nodes during swapping can be handled */
static void
real_bdd_mgr_swap_variables (bdd_mgr_t *mgr, unsigned idx)
{
    unsigned i;
    assert (idx < mgr->num_vars - 1);

    fprintf (stderr, "!!! swapping %u and %u\n", idx, idx + 1);
    fprintf (stderr, "!!! %u / %u / %u (live / deleted / capacity)\n",
             mgr->num_nodes, mgr->num_deleted_nodes, mgr->capacity);
    fprintf (stderr, "!!! old var order: ");
    bdd_mgr_var_order_fprint (mgr, stderr);
    fprintf (stderr, "\n");

    for (i = 0; i < mgr->capacity; i += 1)
        if (node_is_live (mgr->nodes[i]) && (unsigned)mgr->nodes[i].idx == idx) {
            fprintf (stderr, "!!! swapping node %u: (%d %u %u) -> ", i,
                     mgr->nodes[i].idx, mgr->nodes[i].low, mgr->nodes[i].high);
            swap_one_node (mgr, idx, &mgr->nodes[i]);
            fprintf (stderr, "(%d %u %u)\n",
                     mgr->nodes[i].idx, mgr->nodes[i].low, mgr->nodes[i].high);
            /* _bdd_mgr_check_invariants (mgr); */
        }

    swap_unsigned (&mgr->var_to_idx[mgr->idx_to_var[idx]],
                   &mgr->var_to_idx[mgr->idx_to_var[idx + 1]]);
    swap_unsigned (&mgr->idx_to_var[idx], &mgr->idx_to_var[idx + 1]);

    fprintf (stderr, "!!! new var order: ");
    bdd_mgr_var_order_fprint (mgr, stderr);
    fprintf (stderr, "\n");

    /* FIXME: patch up cache instead of clearing it */
    bdd_ite_cache_clear (&mgr->ite_cache);
    fprintf (stderr, "!!! done swapping %u and %u\n", idx, idx + 1);
    fprintf (stderr, "!!! %u / %u / %u (live / deleted / capacity)\n",
             mgr->num_nodes, mgr->num_deleted_nodes, mgr->capacity);
    _bdd_mgr_check_invariants (mgr);
}


void
bdd_mgr_swap_variables (bdd_mgr_t *mgr, unsigned idx)
{
    while (_bdd_catch_out_of_nodes (mgr)) {
        fprintf (stderr, "ERROR: out of nodes during variable swapping\n");
        exit (EXIT_FAILURE);
    }
    real_bdd_mgr_swap_variables (mgr, idx);
}
