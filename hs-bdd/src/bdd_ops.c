#include "bdd_mgr.h"

unsigned
bdd_var (bdd_mgr_t *mgr, bdd_t b)
{
    return mgr->lvl_to_var[bdd_level (mgr, b)];
}

unsigned
bdd_level (bdd_mgr_t *mgr, bdd_t b)
{
    return bdd_to_node(mgr, b).lvl;
}

bdd_t 
bdd_low (bdd_mgr_t *mgr, bdd_t b)
{
    return bdd_to_node(mgr, b).low;
}

bdd_t 
bdd_high (bdd_mgr_t *mgr, bdd_t b)
{
    return bdd_to_node(mgr, b).high;
}

bdd_t 
bdd_ith_var (bdd_mgr_t *mgr, unsigned i)
{
    assert (i < mgr->num_vars);
    return _bdd_make_node (mgr,
                           mgr->var_to_lvl[i],
                           bdd_false,
                           bdd_true);
}


bdd_t 
bdd_and (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2)
{
    return bdd_ite (mgr, b1, b2, bdd_false);
}

bdd_t 
bdd_or (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2)
{
    return bdd_ite (mgr, b1, bdd_true, b2);
}

bdd_t 
bdd_xor (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2)
{
    bdd_t not_b2 = bdd_not (mgr, b2);
    return bdd_ite (mgr, b1, not_b2, b2);
}

bdd_t 
bdd_equiv (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2)
{
    bdd_t not_b2 = bdd_not (mgr, b2);
    return bdd_ite (mgr, b1, b2, not_b2);
}

bdd_t 
bdd_nand (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2)
{
    bdd_t not_b2 = bdd_not (mgr, b2);
    return bdd_ite (mgr, b1, not_b2, bdd_true);
}

bdd_t 
bdd_implies (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2)
{
    return bdd_ite (mgr, b1, b2, bdd_true);
}

bdd_t 
bdd_not (bdd_mgr_t *mgr, bdd_t b)
{
    return bdd_ite (mgr, b, bdd_false, bdd_true);
}

static inline unsigned
lvl_min3 (unsigned x, unsigned y, unsigned z)
{
    if (x <= y && x <= z)
        return x;
    else if (y <= x && y <= z)
        return y;
    else {
        assert (z <= x && z <= y);
        return z;
    }
}

typedef struct
{
    bdd_t low;
    bdd_t high;
} quick_restrict_res_t;

/* Given a BDD index, its corresponding node, and a level no greater
 * than its level, restricts the BDD with the variable at 'lvl'
 * assigned both false and true. */
static inline quick_restrict_res_t
_quick_restrict (bdd_t b, node_t b_n, unsigned lvl)
{
    quick_restrict_res_t result;
    assert (lvl <= b_n.lvl);
    if (lvl < b_n.lvl) {
        result.low = b;
        result.high = b;
    }
    else {
        result.low = b_n.low;
        result.high = b_n.high;
    }
    return result;
}

static bdd_t
_real_bdd_ite (bdd_mgr_t *mgr, bdd_t p, bdd_t t, bdd_t f)
{
    bdd_ite_cache_entry_t *cache_val;

    assert (bdd_is_valid_and_live (mgr, p));
    assert (bdd_is_valid_and_live (mgr, t));
    assert (bdd_is_valid_and_live (mgr, f));

    /* put arguments into ``standard triple'' form */
    /* FIXME: there are many more cases that require complement edges */
    if (p == t)
        t = bdd_true;
    if (p == f)
        f = bdd_false;

    /* terminal cases */
    if (p == bdd_true)
        return t;
    else if (p == bdd_false)
        return f;
    else if (t == bdd_true && f == bdd_false)
        return p;

    /* check cache, recur if needed */
    cache_val = bdd_ite_cache_lookup (&mgr->ite_cache, p, t, f);
    mgr->ite_cache_stats.num_lookups += 1;
    if (cache_val->p == p && cache_val->t == t && cache_val->f == f) {
        mgr->ite_cache_stats.num_hits += 1;
        return cache_val->result;
    }
    else {
        node_t p_n = bdd_to_node (mgr, p);
        node_t t_n = bdd_to_node (mgr, t);
        node_t f_n = bdd_to_node (mgr, f);
        unsigned top_lvl = lvl_min3 (p_n.lvl, t_n.lvl, f_n.lvl);
        quick_restrict_res_t p_v = _quick_restrict (p, p_n, top_lvl);
        quick_restrict_res_t t_v = _quick_restrict (t, t_n, top_lvl);
        quick_restrict_res_t f_v = _quick_restrict (f, f_n, top_lvl);
        bdd_t low = _real_bdd_ite (mgr, p_v.low, t_v.low, f_v.low);
        bdd_inc_ref (mgr, low);
        bdd_t high = _real_bdd_ite (mgr, p_v.high, t_v.high, f_v.high);
        bdd_inc_ref (mgr, high);
        bdd_t result = _bdd_make_node (mgr, top_lvl, low, high);
        bdd_dec_ref (mgr, high);
        bdd_dec_ref (mgr, low);

        bdd_inc_ref (mgr, p);
        bdd_inc_ref (mgr, t);
        bdd_inc_ref (mgr, f);
        bdd_inc_ref (mgr, result);

        mgr->ite_cache_stats.num_inserts += 1;
        if (!bdd_ite_cache_entry_is_free (cache_val)) {
            bdd_dec_ref (mgr, cache_val->p);
            bdd_dec_ref (mgr, cache_val->t);
            bdd_dec_ref (mgr, cache_val->f);
            bdd_dec_ref (mgr, cache_val->result);
            mgr->ite_cache_stats.num_replacements += 1;
        }

        cache_val->p = p;
        cache_val->t = t;
        cache_val->f = f;
        cache_val->result = result;

        return result;
    }
}

bdd_t 
bdd_ite (bdd_mgr_t *mgr, bdd_t p, bdd_t t, bdd_t f)
{
    _bdd_mgr_check_invariants (mgr);
    return _real_bdd_ite (mgr, p, t, f);
}

static bdd_t
bdd_res_rec (bdd_mgr_t *mgr,
             unsigned lvl,
             boolean val,
             bdd_t b)
{
    bdd_t result;
    node_t n = bdd_to_node (mgr, b);
    if (n.lvl > lvl)
        result = b;
    else if (n.lvl == lvl)
        result = val ? n.high : n.low;
    else {
        bdd_t low, high;
        low = bdd_res_rec (mgr, lvl, val, n.low);
        bdd_inc_ref (mgr, low);
        high = bdd_res_rec (mgr, lvl, val, n.high);
        bdd_inc_ref (mgr, high);
        result = _bdd_make_node (mgr, n.lvl, low, high);
        bdd_dec_ref (mgr, high);
        bdd_dec_ref (mgr, low);
    }
    return result;
}

bdd_t 
bdd_restrict (bdd_mgr_t *mgr, bdd_t b, unsigned var, boolean val)
{
    return bdd_res_rec (mgr, mgr->var_to_lvl[var], val, b);
}

bdd_t 
bdd_existential (bdd_mgr_t *mgr, unsigned var, bdd_t b)
{
    bdd_t low, high;
    low = bdd_restrict (mgr, b, var, 0);
    high = bdd_restrict (mgr, b, var, 1);
    return bdd_or (mgr, low, high);
}

bdd_t 
bdd_universal (bdd_mgr_t *mgr, unsigned var, bdd_t b)
{
    bdd_t low, high;
    low = bdd_restrict (mgr, b, var, 0);
    high = bdd_restrict (mgr, b, var, 1);
    return bdd_and (mgr, low, high);
}

bdd_t 
bdd_compose (bdd_mgr_t *mgr, bdd_t f, unsigned x, bdd_t g)
{
    bdd_t ite_t, ite_f;
    ite_t = bdd_restrict (mgr, f, x, 1);
    ite_f = bdd_restrict (mgr, f, x, 0);
    return bdd_ite (mgr, g, ite_t, ite_f);
}

static double
bdd_sat_count_rec (bdd_mgr_t *mgr, bdd_t b)
{
    if (b == bdd_false || b == bdd_true)
        return b;
    else {
        node_t b_node = bdd_to_node(mgr, b);
        double lhs = bdd_sat_count_rec (mgr, b_node.low);
        double rhs = bdd_sat_count_rec (mgr, b_node.high);
        lhs *= pow (2.0, bdd_to_node(mgr, b_node.low).lvl - b_node.lvl - 1);
        rhs *= pow (2.0, bdd_to_node(mgr, b_node.high).lvl - b_node.lvl - 1);
        return lhs + rhs;
    }
}

double
bdd_sat_count (bdd_mgr_t *mgr, bdd_t b)
{
    double count = bdd_sat_count_rec (mgr, b);
    return count * pow (2, bdd_to_node(mgr, b).lvl);
}

unsigned
bdd_get_num_nodes (bdd_mgr_t *mgr, bdd_t b)
{
    /* FIXME: reimplement */
    (void) mgr;
    (void) b;
    assert (0);
}
