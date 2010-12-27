#include "bdd_impl.h"
#include "bdd_pair.h"
#include "bdd_ite_cache.h"
#include "bdd_double_ht.h"
#include "bdd_ht.h"

unsigned
bdd_var (bdd_mgr_t *mgr, bdd_t b)
{
    return bdd_get_node(mgr, b).var;
}

bdd_t
bdd_low (bdd_mgr_t *mgr, bdd_t b)
{
    return bdd_get_node(mgr, b).low;
}

bdd_t
bdd_high (bdd_mgr_t *mgr, bdd_t b)
{
    return bdd_get_node(mgr, b).high;
}


bdd_t
bdd_ith_var (bdd_mgr_t *mgr, unsigned i)
{
    bdd_t ith_var;
    bdd_mgr_check_invariants (mgr);
    assert (i < mgr->num_vars);

    ith_var = make_node_from_parts (mgr, i, bdd_false, bdd_true);
    bdd_mgr_check_invariants (mgr);
    return ith_var;
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

static bdd_t
bdd_not_rec (bdd_mgr_t *mgr, bdd_ht_t *cache, bdd_t b)
{
    if (b == bdd_true)
        return bdd_false;
    else if (b == bdd_false)
        return bdd_true;
    else {
        const bdd_t *cache_val = bdd_ht_lookup (cache, b);
        if (cache_val == NULL) {
            const node_t n = bdd_get_node (mgr, b);
            const bdd_t r = make_node_from_parts (mgr,
                                                  n.var,
                                                  bdd_not (mgr, n.low),
                                                  bdd_not (mgr, n.high));
            bdd_ht_insert (cache, b, r);
            return r;
        }
        else
            return *cache_val;
    }
}

bdd_t
bdd_not (bdd_mgr_t *mgr, bdd_t b)
{
    bdd_ht_t *cache = bdd_ht_create ();
    const bdd_t r = bdd_not_rec (mgr, cache, b);
    bdd_ht_destroy (cache);
    return r;
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

/* Given a BDD index, its corresponding node, and a variable no
 * greater than its variable, restricts the BDD with 'var' assigned
 * both false and true.  The first element of the result contains 'b'
 * restricted with 'var' assigned false, the second element contains
 * 'b' restricted with 'var' assigned true. */
static inline bdd_pair_t
quick_restrict (bdd_t b, node_t b_n, unsigned var)
{
    bdd_pair_t result;
    assert (var <= b_n.var);
    if (var < b_n.var) {
        result.first = b;
        result.second = b;
    }
    else {
        result.first = b_n.low;
        result.second = b_n.high;
    }
    return result;
}

bdd_t
bdd_ite (bdd_mgr_t *mgr, bdd_t p, bdd_t t, bdd_t f)
{
    bdd_ite_cache_entry_t *cache_val;

    assert (0 <= p && (unsigned) p < bdd_mgr_get_num_nodes(mgr));
    assert (0 <= t && (unsigned) t < bdd_mgr_get_num_nodes(mgr));
    assert (0 <= f && (unsigned) f < bdd_mgr_get_num_nodes(mgr));

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
    if (cache_val->p == p && cache_val->t == t && cache_val->f == f)
        return cache_val->result;
    else {
        unsigned top_var;
        node_t p_n, t_n, f_n;
        bdd_pair_t p_v, t_v, f_v; /* p, t, and f restricted with
                                   * top_var and !top_var */
        bdd_t low, high, result;

        p_n = bdd_get_node (mgr, p);
        t_n = bdd_get_node (mgr, t);
        f_n = bdd_get_node (mgr, f);
        top_var = var_min3 (p_n.var, t_n.var, f_n.var);
        p_v = quick_restrict (p, p_n, top_var);
        t_v = quick_restrict (t, t_n, top_var);
        f_v = quick_restrict (f, f_n, top_var);

        low = bdd_ite (mgr, p_v.first, t_v.first, f_v.first);
        high = bdd_ite (mgr, p_v.second, t_v.second, f_v.second);
        result = make_node_from_parts (mgr, top_var, low, high);

        cache_val->p = p;
        cache_val->t = t;
        cache_val->f = f;
        cache_val->result = result;

        return result;
    }
}

/* This largely follows the pseudocode from Andersen's ``An
 * Introduction to Binary Decision Diagrams'', but with the addition
 * of memoization. */
static bdd_t
bdd_res_rec (bdd_mgr_t *mgr,
             const unsigned var,
             const bool val,
             bdd_ht_t *cache,
             bdd_t b)
{
    const bdd_t *cache_val = bdd_ht_lookup (cache, b);
    if (cache_val != NULL)
        return *cache_val;
    else {
        bdd_t result;
        const node_t n = bdd_get_node (mgr, b);
        if (n.var > var)
            result = b;
        else if (n.var == var)
            result = val ? bdd_res_rec (mgr, var, val, cache, n.high)
                         : bdd_res_rec (mgr, var, val, cache, n.low);
        else /* n.var < var */
            result = make_node_from_parts (
                mgr,
                n.var,
                bdd_res_rec (mgr, var, val, cache, n.low),
                bdd_res_rec (mgr, var, val, cache, n.high)
                );
        bdd_ht_insert (cache, b, result);
        return result;
    }
}

bdd_t
bdd_restrict (bdd_mgr_t *mgr, bdd_t b, unsigned var, bool val)
{
    bdd_ht_t *cache = bdd_ht_create ();
    const bdd_t r = bdd_res_rec (mgr, var, val, cache, b);
    bdd_ht_destroy (cache);
    return r;
}

bdd_t
bdd_existential (bdd_mgr_t *mgr, unsigned var, bdd_t b)
{
    return bdd_or (mgr,
                   bdd_restrict (mgr, b, var, false),
                   bdd_restrict (mgr, b, var, true));
}

bdd_t
bdd_universal (bdd_mgr_t *mgr, unsigned var, bdd_t b)
{
    return bdd_and (mgr,
                    bdd_restrict (mgr, b, var, false),
                    bdd_restrict (mgr, b, var, true));
}

bdd_t
bdd_compose (bdd_mgr_t *mgr, bdd_t f, unsigned x, bdd_t g)
{
    bdd_t not_g = bdd_not (mgr, g);
    return bdd_or (mgr,
                   bdd_and (mgr, not_g, bdd_restrict (mgr, f, x, false)),
                   bdd_and (mgr, g, bdd_restrict (mgr, f, x, true)));
}

static double
bdd_sat_count_rec (bdd_mgr_t *mgr, bdd_double_ht_t *cache, bdd_t b)
{
    if (b == bdd_false)
        return 0;
    else if (b == bdd_true)
        return 1;
    else {
        const double *result = bdd_double_ht_lookup (cache, b);
        if (result != NULL)
            return *result;
        else {
            const node_t b_node = bdd_get_node(mgr, b);
            const node_t b_low = bdd_get_node(mgr, b_node.low);
            const node_t b_high = bdd_get_node(mgr, b_node.high);
            const double result = 
                pow (2.0, b_low.var - b_node.var - 1) *
                bdd_sat_count_rec (mgr, cache, b_node.low) +
                pow (2.0, b_high.var - b_node.var - 1) *
                bdd_sat_count_rec (mgr, cache, b_node.high);
            bdd_double_ht_insert (cache, b, result);
            return result;
        }
    }
}

double
bdd_sat_count (bdd_mgr_t *mgr, bdd_t b)
{
    bdd_double_ht_t *cache;
    double result;

    bdd_mgr_check_invariants (mgr);
    cache = bdd_double_ht_create ();
    result =
        pow (2, bdd_get_node(mgr, b).var) *
        bdd_sat_count_rec (mgr, cache, b);
    bdd_double_ht_destroy (cache);
    return result;
}

unsigned
bdd_get_num_nodes (bdd_mgr_t *mgr, bdd_t b)
{
    if (b == bdd_true || b == bdd_false)
        return 1;
    else {
        const node_t b_node = bdd_get_node (mgr, b);
        return
            bdd_get_num_nodes (mgr, b_node.low) +
            bdd_get_num_nodes (mgr, b_node.high);
    }
}
