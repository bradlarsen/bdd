#include "bdd_mgr.h"

#ifndef NDEBUG
#include "node_ht.h"

static boolean
_is_power_of_two (unsigned n)
{
    unsigned i;
    for (i = 1; i < n; i *= 2) {}
    return i == n;
}

static void
_check_no_duplicate_nodes (bdd_mgr_t *mgr)
{
    node_ht_t *seen = node_ht_create ();
    unsigned i;

    for (i = 0; i < mgr->capacity; i += 1) {
        if (node_is_live (mgr->nodes[i])) {
            assert (node_ht_lookup (seen, mgr->nodes[i]) == NULL);
            node_ht_insert (seen, mgr->nodes[i], 1);
        }
    }

    node_ht_destroy (seen);
}

static void
_check_node_values (bdd_mgr_t *mgr)
{
    unsigned _i;
    assert (mgr->nodes[0].lvl == mgr->num_vars);
    assert (mgr->nodes[0].low == 1);
    assert (mgr->nodes[0].high == 0);
    assert (mgr->nodes[1].lvl == mgr->num_vars);
    assert (mgr->nodes[1].low == 0);
    assert (mgr->nodes[1].high == 1);
    for (_i = 2; _i < mgr->capacity; _i += 1) {
        node_t _n = mgr->nodes[_i];
        if (node_is_live (_n)) {
            node_t _n_low, _n_high;
            assert (_n.lvl < mgr->num_vars);
            assert (_n.low != _n.high);

            assert (_n.low < mgr->capacity);
            _n_low = mgr->nodes[_n.low];
            assert (_n_low.lvl > _n.lvl);
            assert (node_is_live (_n_low));

            assert (_n.high < mgr->capacity);
            _n_high = mgr->nodes[_n.high];
            assert (_n_high.lvl > _n.lvl);
            assert (node_is_live (_n_high));
        }
    }
}

static void
_check_proper_lvl_var_mapping (bdd_mgr_t *mgr)
{
    unsigned _i;
    for (_i = 0; _i < mgr->num_vars; _i += 1) {
        assert (mgr->lvl_to_var[mgr->var_to_lvl[_i]] == _i);
        assert (mgr->var_to_lvl[mgr->lvl_to_var[_i]] == _i);
    }
}

void
_bdd_mgr_check_invariants(bdd_mgr_t *mgr)
{
    assert (mgr != NULL);
    assert (mgr->num_vars > 0);
    assert (mgr->capacity >= 2);
    assert (_is_power_of_two (mgr->capacity));
    assert (mgr->free_hash_entry_idx > 0);
    assert (mgr->free_hash_entry_idx < mgr->capacity);
    _check_node_values (mgr);
    _check_proper_lvl_var_mapping (mgr);
    _check_no_duplicate_nodes (mgr);
}

#endif /* NDEBUG */
