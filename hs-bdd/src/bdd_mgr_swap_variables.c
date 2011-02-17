#include "bdd_mgr.h"

static inline void
_swap_unsigned (unsigned *i, unsigned *j)
{
    unsigned tmp = *i;
    *i = *j;
    *j = tmp;
}

static void
_bdd_mgr_var_order_fprint (bdd_mgr_t *mgr, FILE *handle)
{
    unsigned i;
    fprintf (handle, "variable order: %u", mgr->lvl_to_var[0]);
    for (i = 1; i < mgr->num_vars; i += 1)
        fprintf (handle, " %u", mgr->lvl_to_var[i]);
    fprintf (handle, "\n");
}

static void
_swap_node_in_place (bdd_mgr_t *mgr, unsigned idx0, bdd_t f)
{
    unsigned idx1 = idx0 + 1;
    bdd_t f0 = mgr->nodes[f].low;
    bdd_t f1 = mgr->nodes[f].high;
    bdd_t f00 = mgr->nodes[f0].low;
    bdd_t f01 = mgr->nodes[f0].high;
    bdd_t f10 = mgr->nodes[f1].low;
    bdd_t f11 = mgr->nodes[f1].high;
    bdd_t new_f0, new_f1;
    assert (f00 != f01);
    assert (f10 != f11);
    new_f0 = _bdd_make_node (mgr, idx1, f00, f10);
    assert (new_f0 != f);
    if (new_f0 != f0) {
        _bdd_inc_ref (mgr, new_f0);
        _bdd_dec_ref_rec (mgr, f0);
    }
    new_f1 = _bdd_make_node (mgr, idx1, f01, f11);
    assert (new_f1 != f);
    if (new_f1 != f1) {
        _bdd_inc_ref (mgr, new_f1);
        _bdd_dec_ref_rec (mgr, f1);
    }
    assert (new_f0 != new_f1);
    _node_ht_delete (mgr, f);
    mgr->nodes[f].low = new_f0;
    mgr->nodes[f].high = new_f1;
    _node_ht_insert (mgr, f);
}

static void
print_node (bdd_mgr_t *mgr, bdd_t b, char *prefix, char *suffix)
{
    fprintf (stderr, "%s%u@(ref_cnt: %u, lvl: %u, low: %u, high: %u)%s",
             prefix,
             b,
             mgr->nodes[b].ref_cnt,
             mgr->nodes[b].lvl,
             mgr->nodes[b].low,
             mgr->nodes[b].high,
             suffix);
}

void
bdd_mgr_swap_variables (bdd_mgr_t *mgr, unsigned idx)
{
    unsigned i;
    assert (idx < mgr->num_vars - 1);
    fprintf (stderr, "*** begin swap %u ***\n", idx);
    _bdd_mgr_var_order_fprint (mgr, stderr);

    for (i = 0; i < mgr->capacity; i += 1)
        if (node_is_live (mgr->nodes[i]) && mgr->nodes[i].lvl == idx) {
            fprintf (stderr, "!!! swapping node %u:\n", i);
            print_node (mgr, i, "    ", "\n");
            print_node (mgr, mgr->nodes[i].low, "        ", "\n");
            print_node (mgr, mgr->nodes[i].high, "        ", "\n");
            _swap_node_in_place (mgr, idx, i);
            print_node (mgr, i, "    ", "\n");
            print_node (mgr, mgr->nodes[i].low, "        ", "\n");
            print_node (mgr, mgr->nodes[i].high, "        ", "\n");
        }

    _swap_unsigned (&mgr->var_to_lvl[mgr->lvl_to_var[idx]],
                    &mgr->var_to_lvl[mgr->lvl_to_var[idx + 1]]);
    _swap_unsigned (&mgr->lvl_to_var[idx], &mgr->lvl_to_var[idx + 1]);

    _bdd_mgr_var_order_fprint (mgr, stderr);
    _bdd_mgr_check_invariants (mgr);
    fprintf (stderr, "*** end swap %u ***\n", idx);
}
