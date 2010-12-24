#include "bdd_impl.h"
#include "bdd_pair.h"
#include "bdd_pair_cache.h"
#include "bdd_double_ht.h"
#include "bdd_ht.h"

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

/* The type of an optional BDD. */
typedef struct maybe_bdd
{
    bdd_t value;
    bool has_value;
} maybe_bdd_t;

/* Create an optional BDD with no value. */
static inline maybe_bdd_t
maybe_bdd_nothing ()
{
    maybe_bdd_t m;
    m.value = 0;
    m.has_value = false;
    return m;
}

/* Create an optional BDD with the given value. */
static inline maybe_bdd_t
maybe_bdd_just (bdd_t v)
{
    maybe_bdd_t m;
    m.value = v;
    m.has_value = true;
    return m;
}

/* The type of operator-specific functions passed to bdd_apply.  A
 * bdd_apply_op_fun looks at its two BDD arguments and possibly
 * returns a BDD result.  Given two terminal BDDs, such a function
 * must return a result; in the other case, the function maybe return
 * a result if identies apply.  This allows the same recursive BDD
 * apply code to be reused, as only the operator-specific code
 * changes. */
typedef maybe_bdd_t (* bdd_apply_op_fun) (bdd_t b1, bdd_t b2);

static bdd_t
bdd_apply_rec (bdd_mgr_t *mgr,
               bdd_pair_cache_t cache,
               bdd_apply_op_fun op,
               bdd_pair_t p)
{
    cache_entry_t *cache_val = bdd_pair_cache_lookup (cache, p);
    if (bdd_pair_equal(cache_val->key, p))
        return cache_val->value;
    else {
        const bdd_t b1 = p.first;
        const node_t n1 = bdd_get_node (mgr, b1);
        const bdd_t b2 = p.second;
        const node_t n2 = bdd_get_node (mgr, b2);
        const maybe_bdd_t mresult = op (b1, b2);
        bdd_t result;
        if (mresult.has_value)
            result = mresult.value;
        else {
            bdd_pair_t p1, p2;
            unsigned var;
            if (n1.var == n2.var) {
                var = n1.var;
                p1.first = n1.low;
                p1.second = n2.low;
                p2.first = n1.high;
                p2.second = n2.high;
            }
            else if (n1.var < n2.var) {
                var = n1.var;
                p1.first = n1.low;
                p1.second = b2;
                p2.first = n1.high;
                p2.second = b2;
            }
            else {
                assert (n1.var > n2.var);
                var = n2.var;
                p1.first = b1;
                p1.second = n2.low;
                p2.first = b1;
                p2.second = n2.high;
            }

            result = make_node_from_parts (
                mgr,
                var,
                bdd_apply_rec (mgr, cache, op, p1),
                bdd_apply_rec (mgr, cache, op, p2)
                );
        }
        cache_val->key = p;
        cache_val->value = result;
        return result;
    }
}

static bdd_t
bdd_apply (
    bdd_mgr_t *mgr,
    bdd_pair_cache_t cache,
    bdd_apply_op_fun op,
    bdd_t b1,
    bdd_t b2
    )
{
    bdd_pair_t p;
    bdd_t result;

    bdd_mgr_check_invariants (mgr);
    assert (bdd_to_idx(b1) < bdd_mgr_get_num_nodes(mgr));
    assert (bdd_to_idx(b2) < bdd_mgr_get_num_nodes(mgr));

    p.first = b1;
    p.second = b2;
    result = bdd_apply_rec (mgr, cache, op, p);

    bdd_mgr_check_invariants (mgr);
    return result;
}

static maybe_bdd_t
bdd_and_fun (bdd_t b1, bdd_t b2)
{
    if (b1 == b2)
        return maybe_bdd_just (b1);
    if (b1 == bdd_true && b2 == bdd_true)
        return maybe_bdd_just (bdd_true);
    else if (b1 == bdd_true)
        return maybe_bdd_just (b2);
    else if (b2 == bdd_true)
        return maybe_bdd_just (b1);
    else if (b1 == bdd_false || b2 == bdd_false)
        return maybe_bdd_just (bdd_false);
    else
        return maybe_bdd_nothing ();
}

bdd_t
bdd_and (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2)
{
    return bdd_apply (mgr, mgr->apply_caches[BDD_AND], bdd_and_fun, b1, b2);
}

static maybe_bdd_t
bdd_or_fun (bdd_t b1, bdd_t b2)
{
    if (b1 == b2)
        return maybe_bdd_just (b1);
    else if (b1 == bdd_true || b2 == bdd_true)
        return maybe_bdd_just (bdd_true);
    else if (b1 == bdd_false)
        return maybe_bdd_just (b2);
    else if (b2 == bdd_false)
        return maybe_bdd_just (b1);
    else if (b1 == bdd_false && b2 == bdd_false)
        return maybe_bdd_just (bdd_false);
    else
        return maybe_bdd_nothing ();
}

bdd_t
bdd_or (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2)
{
    return bdd_apply (mgr, mgr->apply_caches[BDD_OR], bdd_or_fun, b1, b2);
}

static maybe_bdd_t
bdd_xor_fun (bdd_t b1, bdd_t b2)
{
    if (b1 == b2)
        return maybe_bdd_just (bdd_false);
    else if (b1 == bdd_false)
        return maybe_bdd_just (b2);
    else if (b2 == bdd_false)
        return maybe_bdd_just (b1);
    else
        return maybe_bdd_nothing ();
}

bdd_t
bdd_xor (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2)
{
    return bdd_apply (mgr, mgr->apply_caches[BDD_XOR], bdd_xor_fun, b1, b2);
}

static maybe_bdd_t
bdd_equiv_fun (bdd_t b1, bdd_t b2)
{
    if (b1 == b2)
        return maybe_bdd_just (bdd_true);
    else if (b1 == bdd_true)
        return maybe_bdd_just (b2);
    else if (b2 == bdd_true)
        return maybe_bdd_just (b1);
    else
        return maybe_bdd_nothing ();
}

bdd_t
bdd_equiv (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2)
{
    return bdd_apply (mgr, mgr->apply_caches[BDD_EQUIV], bdd_equiv_fun, b1, b2);
}

static maybe_bdd_t
bdd_nand_fun (bdd_t b1, bdd_t b2)
{
    if (b1 == bdd_false || b2 == bdd_false)
        return maybe_bdd_just (bdd_true);
    else if (b1 == bdd_true && b2 == bdd_true)
        return maybe_bdd_just (bdd_false);
    else
        return maybe_bdd_nothing ();
}

bdd_t
bdd_nand (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2)
{
    return bdd_apply (mgr, mgr->apply_caches[BDD_NAND], bdd_nand_fun, b1, b2);
}

static maybe_bdd_t
bdd_implies_fun (bdd_t b1, bdd_t b2)
{
    if (b1 == b2)
        return maybe_bdd_just (bdd_true);
    else if (b1 == bdd_false || b2 == bdd_true)
        return maybe_bdd_just (bdd_true);
    else if (b1 == bdd_true)
        return maybe_bdd_just (b2);
    else
        return maybe_bdd_nothing ();
}

bdd_t
bdd_implies (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2)
{
    return bdd_apply (mgr, mgr->apply_caches[BDD_IMPLIES], bdd_implies_fun, b1, b2);
}

/* Since we use complement arcs (implemented by using negative values
 * to denote negated BDDs), BDD negation is implemented simply by
 * integer negation. */
bdd_t
bdd_not (bdd_mgr_t *mgr, bdd_t b)
{
    return -b;
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
