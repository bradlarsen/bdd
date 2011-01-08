#include "bdd_impl.h"
#include "bdd_ite_cache.h"
#include "bdd_double_ht.h"
#include "bdd_ht.h"
#include "bdd_rtu_ht.h"
#include "usr_bdd_ht.h"

unsigned
bdd_var (bdd_mgr_t *mgr, bdd_t *b)
{
    return raw_bdd_to_node(mgr, usr_to_raw (mgr, b)).var;
}

bdd_t *
bdd_low (bdd_mgr_t *mgr, bdd_t *b)
{
    return raw_to_usr (mgr, raw_bdd_to_node(mgr, usr_to_raw (mgr, b)).low);
}

bdd_t *
bdd_high (bdd_mgr_t *mgr, bdd_t *b)
{
    return raw_to_usr (mgr, raw_bdd_to_node(mgr, usr_to_raw (mgr, b)).high);
}

void
bdd_inc_ref (bdd_mgr_t *mgr, bdd_t *b)
{
    usr_bdd_entry_t *entry = usr_bdd_ht_lookup (mgr->usr_bdd_map, b);
    assert (entry != NULL);
    if (entry->ref_cnt == 0) {
        assert (mgr->num_unreferenced_bdds > 0);
        mgr->num_unreferenced_bdds -= 1;
    }
    entry->ref_cnt += 1;
}

static float
usr_bdd_dead_fraction (bdd_mgr_t *mgr)
{
    return
        (float) mgr->num_unreferenced_bdds /
        (float) usr_bdd_ht_get_num_entries (mgr->usr_bdd_map);
}

void
bdd_dec_ref (bdd_mgr_t *mgr, bdd_t *b)
{
    usr_bdd_entry_t *entry = usr_bdd_ht_lookup (mgr->usr_bdd_map, b);
    assert (entry != NULL);
    assert (entry->ref_cnt > 0);
    entry->ref_cnt -= 1;
    if (entry->ref_cnt == 0)
        mgr->num_unreferenced_bdds += 1;

    /* FIXME: what is a reasonable value for this fraction? */
    if (usr_bdd_dead_fraction (mgr) >= 0.75f)
        bdd_mgr_perform_gc (mgr);
}

bdd_t *
bdd_ith_var (bdd_mgr_t *mgr, unsigned i)
{
    raw_bdd_t res;
    assert (i < mgr->num_vars);
    res = make_node (mgr, i, raw_bdd_false, raw_bdd_true);
    return raw_to_usr(mgr, res);
}


bdd_t *
bdd_and (bdd_mgr_t *mgr, bdd_t *b1, bdd_t *b2)
{
    return bdd_ite (mgr, b1, b2, bdd_false(mgr));
}

bdd_t *
bdd_or (bdd_mgr_t *mgr, bdd_t *b1, bdd_t *b2)
{
    return bdd_ite (mgr, b1, bdd_true(mgr), b2);
}

bdd_t *
bdd_xor (bdd_mgr_t *mgr, bdd_t *b1, bdd_t *b2)
{
    bdd_t *not_b2 = bdd_not (mgr, b2);
    return bdd_ite (mgr, b1, not_b2, b2);
}

bdd_t *
bdd_equiv (bdd_mgr_t *mgr, bdd_t *b1, bdd_t *b2)
{
    bdd_t *not_b2 = bdd_not (mgr, b2);
    return bdd_ite (mgr, b1, b2, not_b2);
}

bdd_t *
bdd_nand (bdd_mgr_t *mgr, bdd_t *b1, bdd_t *b2)
{
    bdd_t *not_b2 = bdd_not (mgr, b2);
    return bdd_ite (mgr, b1, not_b2, bdd_true(mgr));
}

bdd_t *
bdd_implies (bdd_mgr_t *mgr, bdd_t *b1, bdd_t *b2)
{
    return bdd_ite (mgr, b1, b2, bdd_true(mgr));
}

bdd_t *
bdd_not (bdd_mgr_t *mgr, bdd_t *b)
{
    return bdd_ite (mgr, b, bdd_false(mgr), bdd_true(mgr));
}

static inline unsigned
var_min3 (unsigned x, unsigned y, unsigned z)
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
    raw_bdd_t low;
    raw_bdd_t high;
} quick_restrict_res_t;

/* Given a BDD index, its corresponding node, and a variable no
 * greater than its variable, restricts the BDD with 'var' assigned
 * both false and true. */
static inline quick_restrict_res_t
quick_restrict (raw_bdd_t b, node_t b_n, unsigned var)
{
    quick_restrict_res_t result;
    assert (var <= b_n.var);
    if (var < b_n.var) {
        result.low = b;
        result.high = b;
    }
    else {
        result.low = b_n.low;
        result.high = b_n.high;
    }
    return result;
}

static raw_bdd_t
raw_bdd_ite (bdd_mgr_t *mgr, raw_bdd_t p, raw_bdd_t t, raw_bdd_t f)
{
    bdd_ite_cache_entry_t *cache_val;

    assert (0 <= p && (unsigned) p < bdd_mgr_get_num_nodes(mgr));
    assert (0 <= t && (unsigned) t < bdd_mgr_get_num_nodes(mgr));
    assert (0 <= f && (unsigned) f < bdd_mgr_get_num_nodes(mgr));

    /* put arguments into ``standard triple'' form */
    /* FIXME: there are many more cases that require complement edges */
    if (p == t)
        t = raw_bdd_true;
    if (p == f)
        f = raw_bdd_false;

    /* terminal cases */
    if (p == raw_bdd_true)
        return t;
    else if (p == raw_bdd_false)
        return f;
    else if (t == raw_bdd_true && f == raw_bdd_false)
        return p;

    /* check cache, recur if needed */
    cache_val = bdd_ite_cache_lookup (&mgr->ite_cache, p, t, f);
    mgr->ite_cache_stats.num_lookups += 1;
    if (cache_val->p == p && cache_val->t == t && cache_val->f == f) {
        mgr->ite_cache_stats.num_hits += 1;
        return cache_val->result;
    }
    else {
        unsigned top_var;
        node_t p_n, t_n, f_n;
        quick_restrict_res_t p_v, t_v, f_v; /* p, t, and f restricted with
                                             * top_var and !top_var */
        raw_bdd_t low, high, result;

        p_n = raw_bdd_to_node (mgr, p);
        t_n = raw_bdd_to_node (mgr, t);
        f_n = raw_bdd_to_node (mgr, f);
        top_var = var_min3 (p_n.var, t_n.var, f_n.var);
        p_v = quick_restrict (p, p_n, top_var);
        t_v = quick_restrict (t, t_n, top_var);
        f_v = quick_restrict (f, f_n, top_var);

        low = raw_bdd_ite (mgr, p_v.low, t_v.low, f_v.low);
        high = raw_bdd_ite (mgr, p_v.high, t_v.high, f_v.high);
        result = make_node (mgr, top_var, low, high);

        if (!bdd_ite_cache_entry_is_free (cache_val))
            mgr->ite_cache_stats.num_replacements += 1;
        mgr->ite_cache_stats.num_inserts += 1;
        cache_val->p = p;
        cache_val->t = t;
        cache_val->f = f;
        cache_val->result = result;

        return result;
    }
}

bdd_t *
bdd_ite (bdd_mgr_t *mgr, bdd_t *p, bdd_t *t, bdd_t *f)
{
    raw_bdd_t p_raw, t_raw, f_raw;
    p_raw = usr_to_raw (mgr, p);
    t_raw = usr_to_raw (mgr, t);
    f_raw = usr_to_raw (mgr, f);
    return raw_to_usr (mgr, raw_bdd_ite (mgr, p_raw, t_raw, f_raw));
}

/* This largely follows the pseudocode from Andersen's ``An
 * Introduction to Binary Decision Diagrams'', but with the addition
 * of memoization. */
static raw_bdd_t
raw_bdd_res_rec (bdd_mgr_t *mgr,
                 const unsigned var,
                 const boolean val,
                 bdd_ht_t *cache,
                 raw_bdd_t b)
{
    const raw_bdd_t *cache_val = bdd_ht_lookup (cache, b);
    if (cache_val != NULL)
        return *cache_val;
    else {
        raw_bdd_t result;
        const node_t n = raw_bdd_to_node (mgr, b);
        if (n.var > var)
            result = b;
        else if (n.var == var)
            result = val ? n.high : n.low;
        else /* n.var < var */
            result = make_node (
                mgr,
                n.var,
                raw_bdd_res_rec (mgr, var, val, cache, n.low),
                raw_bdd_res_rec (mgr, var, val, cache, n.high)
                );
        bdd_ht_insert (cache, b, result);
        return result;
    }
}

bdd_t *
bdd_restrict (bdd_mgr_t *mgr, bdd_t *b, unsigned var, boolean val)
{
    bdd_ht_t *cache;
    raw_bdd_t b_raw;
    raw_bdd_t res;
    b_raw = usr_to_raw (mgr, b);
    cache = bdd_ht_create ();
    res = raw_bdd_res_rec (mgr, var, val, cache, b_raw);
    bdd_ht_destroy (cache);
    return raw_to_usr (mgr, res);
}

bdd_t *
bdd_existential (bdd_mgr_t *mgr, unsigned var, bdd_t *b)
{
    return bdd_or (mgr,
                   bdd_restrict (mgr, b, var, bfalse),
                   bdd_restrict (mgr, b, var, btrue));
}

bdd_t *
bdd_universal (bdd_mgr_t *mgr, unsigned var, bdd_t *b)
{
    return bdd_and (mgr,
                    bdd_restrict (mgr, b, var, bfalse),
                    bdd_restrict (mgr, b, var, btrue));
}

bdd_t *
bdd_compose (bdd_mgr_t *mgr, bdd_t *f, unsigned x, bdd_t *g)
{
    bdd_t *ite_t, *ite_f;
    ite_t = bdd_restrict (mgr, f, x, btrue);
    ite_f = bdd_restrict (mgr, f, x, bfalse);
    return bdd_ite (mgr, g, ite_t, ite_f);
}

static double
raw_bdd_sat_count_rec (bdd_mgr_t *mgr, bdd_double_ht_t *cache, raw_bdd_t b)
{
    if (b == raw_bdd_false)
        return 0;
    else if (b == raw_bdd_true)
        return 1;
    else {
        const double *result = bdd_double_ht_lookup (cache, b);
        if (result != NULL)
            return *result;
        else {
            const node_t b_node = raw_bdd_to_node(mgr, b);
            const node_t b_low = raw_bdd_to_node(mgr, b_node.low);
            const node_t b_high = raw_bdd_to_node(mgr, b_node.high);
            const double lhs = pow (2.0, b_low.var - b_node.var - 1) *
                raw_bdd_sat_count_rec (mgr, cache, b_node.low);
            const double rhs = pow (2.0, b_high.var - b_node.var - 1) *
                raw_bdd_sat_count_rec (mgr, cache, b_node.high);
            const double res = lhs + rhs;
            bdd_double_ht_insert (cache, b, res);
            return res;
        }
    }
}

double
bdd_sat_count (bdd_mgr_t *mgr, bdd_t *b)
{
    raw_bdd_t b_raw;
    bdd_double_ht_t *cache;
    double result;

    b_raw = usr_to_raw (mgr, b);
    cache = bdd_double_ht_create ();
    result =
        pow (2, raw_bdd_to_node(mgr, b_raw).var) *
        raw_bdd_sat_count_rec (mgr, cache, b_raw);
    bdd_double_ht_destroy (cache);
    return result;
}

static unsigned
raw_bdd_get_num_nodes (bdd_mgr_t *mgr, raw_bdd_t b)
{
    if (b == raw_bdd_true || b == raw_bdd_false)
        return 1;
    else {
        const node_t b_node = raw_bdd_to_node (mgr, b);
        return
            raw_bdd_get_num_nodes (mgr, b_node.low) +
            raw_bdd_get_num_nodes (mgr, b_node.high);
    }
}


unsigned
bdd_get_num_nodes (bdd_mgr_t *mgr, bdd_t *b)
{
    return raw_bdd_get_num_nodes (mgr, usr_to_raw (mgr, b));
}
