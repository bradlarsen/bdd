#include "bdd_mgr.h"
#include "bdd_ite_cache.h"
#include "bdd_double_ht.h"
#include "bdd_ht.h"
#include "bdd_rtu_ht.h"
#include "usr_bdd_ht.h"

/* Interns the raw BDD index, mapping it to a new user-level BDD
 * index with a reference count of 0. */
static bdd_t *
intern_raw_bdd (bdd_mgr_t *mgr, raw_bdd_t raw)
{
    bdd_t *usr;
    usr_bdd_entry_t entry;
    assert (raw_bdd_is_valid_and_live (mgr, raw));
    assert (bdd_rtu_ht_lookup (mgr->raw_bdd_map, raw) == NULL);
    usr = (bdd_t *) checked_malloc (sizeof(bdd_t));
    usr->id = mgr->new_usr_id;
    assert (usr_bdd_ht_lookup (mgr->usr_bdd_map, usr) == NULL);
    mgr->new_usr_id += 1;
    bdd_rtu_ht_insert (mgr->raw_bdd_map, raw, usr);
    entry.raw_bdd = raw;
    entry.ref_cnt = 0;
    usr_bdd_ht_insert (mgr->usr_bdd_map, usr, entry);
    mgr->num_unreferenced_bdds += 1;
    return usr;
}

/* Converts a raw BDD index to a user-level BDD index.  Creates a
 * binding between the two if none exists. */
static bdd_t *
raw_to_usr (bdd_mgr_t *mgr, raw_bdd_t raw)
{
    bdd_t **res;
    assert (raw_bdd_is_valid_and_live (mgr, raw));
    res = bdd_rtu_ht_lookup (mgr->raw_bdd_map, raw);
    if (res == NULL)
        return intern_raw_bdd (mgr, raw);
    else
        return *res;
}

/* Converts a user-level BDD index to a raw BDD index. */
static raw_bdd_t
usr_to_raw (bdd_mgr_t *mgr, bdd_t *usr)
{
    usr_bdd_entry_t *res = usr_bdd_ht_lookup (mgr->usr_bdd_map, usr);
    assert (res != NULL);
    assert (raw_bdd_is_valid_and_live (mgr, res->raw_bdd));
    return res->raw_bdd;
}





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

bdd_t *
bdd_false (bdd_mgr_t *mgr)
{
    return raw_to_usr (mgr, raw_bdd_false);
}

bdd_t *
bdd_true (bdd_mgr_t *mgr)
{
    return raw_to_usr (mgr, raw_bdd_true);
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

void
bdd_dec_ref (bdd_mgr_t *mgr, bdd_t *b)
{
    usr_bdd_entry_t *entry = usr_bdd_ht_lookup (mgr->usr_bdd_map, b);
    assert (entry != NULL);
    assert (entry->ref_cnt > 0);
    entry->ref_cnt -= 1;
    if (entry->ref_cnt == 0)
        mgr->num_unreferenced_bdds += 1;
}

bdd_t *
bdd_ith_var (bdd_mgr_t *mgr, unsigned i)
{
    raw_bdd_t res;
    volatile boolean already_interrupted_for_gc;
    already_interrupted_for_gc = bfalse;
    assert (i < mgr->num_vars);
    while (_bdd_catch_out_of_nodes (mgr)) {
        if (!already_interrupted_for_gc) {
            fprintf (stderr, "*** in bdd_ith_var: garbage collecting\n");
            if (mgr->num_unreferenced_bdds > 0)
                bdd_mgr_perform_gc (mgr);
            if (mgr->num_nodes > 0.3 * mgr->capacity) {
                fprintf (stderr, "*** in bdd_ith_var: doubling after insufficient GC\n");

                bdd_mgr_resize (mgr, mgr->capacity * 2);
            }
            else
                already_interrupted_for_gc = btrue;
        }
        else {
            fprintf (stderr, "*** in bdd_ith_var: doubling\n");
            bdd_mgr_resize (mgr, mgr->capacity * 2);
            already_interrupted_for_gc = bfalse;
        }
    }
    res = _bdd_make_node (mgr, i, raw_bdd_false, raw_bdd_true);
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
quick_restrict (raw_bdd_t b, node_t b_n, int var)
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

    assert (raw_bdd_is_valid_and_live (mgr, p));
    assert (raw_bdd_is_valid_and_live (mgr, t));
    assert (raw_bdd_is_valid_and_live (mgr, f));

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
        result = _bdd_make_node (mgr, top_var, low, high);

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
    raw_bdd_t p_raw, t_raw, f_raw, res_raw;
    volatile boolean already_interrupted_for_gc;
    already_interrupted_for_gc = bfalse;
    while (_bdd_catch_out_of_nodes (mgr)) {
        if (!already_interrupted_for_gc) {
            fprintf (stderr, "*** in bdd_ite: garbage collecting\n");
            if (mgr->num_unreferenced_bdds > 0) {
                bdd_inc_ref (mgr, p);
                bdd_inc_ref (mgr, t);
                bdd_inc_ref (mgr, f);
                bdd_mgr_perform_gc (mgr);
                bdd_dec_ref (mgr, f);
                bdd_dec_ref (mgr, t);
                bdd_dec_ref (mgr, p);
            }

            if (mgr->num_nodes > 0.3 * mgr->capacity) {
                fprintf (stderr, "*** in bdd_ite: doubling after insufficient GC\n");
                bdd_mgr_resize (mgr, mgr->capacity * 2);
            }
            else
                already_interrupted_for_gc = btrue;
        }
        else {
            fprintf (stderr, "*** in bdd_ite: doubling\n");
            bdd_mgr_resize (mgr, mgr->capacity * 2);
            already_interrupted_for_gc = bfalse;
        }
    }
    p_raw = usr_to_raw (mgr, p);
    t_raw = usr_to_raw (mgr, t);
    f_raw = usr_to_raw (mgr, f);
    res_raw = raw_bdd_ite (mgr, p_raw, t_raw, f_raw);
    return raw_to_usr (mgr, res_raw);
}

/* This largely follows the pseudocode from Andersen's ``An
 * Introduction to Binary Decision Diagrams'', but with the addition
 * of memoization. */
/* FIXME: use a better cache */
static raw_bdd_t
raw_bdd_res_rec (bdd_mgr_t *mgr,
                 const int var,
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
            result = _bdd_make_node (
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
    volatile boolean already_interrupted_for_gc;
    cache = NULL;
    already_interrupted_for_gc = bfalse;
    while (_bdd_catch_out_of_nodes (mgr)) {
        if (cache != NULL)
            bdd_ht_destroy (cache);
        if (!already_interrupted_for_gc) {
            fprintf (stderr, "*** in bdd_restrict: garbage collecting\n");
            if (mgr->num_unreferenced_bdds > 0) {
                bdd_inc_ref (mgr, b);
                bdd_mgr_perform_gc (mgr);
                bdd_dec_ref (mgr, b);
            }

            if (mgr->num_nodes > 0.3 * mgr->capacity) {
                fprintf (stderr, "*** in bdd_restrict: doubling after insufficient GC\n");

                bdd_mgr_resize (mgr, mgr->capacity * 2);
            }
            else
                already_interrupted_for_gc = btrue;
        }
        else {
            fprintf (stderr, "*** in bdd_restrict: doubling\n");
            bdd_mgr_resize (mgr, mgr->capacity * 2);
            already_interrupted_for_gc = bfalse;
        }
    }
    b_raw = usr_to_raw (mgr, b);
    cache = bdd_ht_create ();
    res = raw_bdd_res_rec (mgr, var, val, cache, b_raw);
    bdd_ht_destroy (cache);
    return raw_to_usr (mgr, res);
}

bdd_t *
bdd_existential (bdd_mgr_t *mgr, unsigned var, bdd_t *b)
{
    bdd_t *low, *high;
    low = bdd_restrict (mgr, b, var, bfalse);
    high = bdd_restrict (mgr, b, var, btrue);
    return bdd_or (mgr, low, high);
}

bdd_t *
bdd_universal (bdd_mgr_t *mgr, unsigned var, bdd_t *b)
{
    bdd_t *low, *high;
    low = bdd_restrict (mgr, b, var, bfalse);
    high = bdd_restrict (mgr, b, var, btrue);
    return bdd_and (mgr, low, high);
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
    if (raw_bdd_is_terminal (b))
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
