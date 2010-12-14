#include "bdd_impl.h"
#include "bdd_pair.h"
#include "bdd_pair_ht.h"
#include "bdd_double_ht.h"
#include "bdd_ht.h"

bdd_t
bdd_mgr_get_ith_var (bdd_mgr_t *mgr, unsigned i)
{
    node_t node;
    bdd_t ith_var;

    bdd_mgr_check_invariants (mgr);
    assert (i < mgr->num_vars);

    node.var = i;
    node.low = bdd_false;
    node.high = bdd_true;
    ith_var = make_node (mgr, node);
    bdd_mgr_check_invariants (mgr);
    assert (node_equal (node, get_node_by_idx (mgr, ith_var)));
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
               bdd_pair_ht_t *cache,
               bdd_apply_op_fun op,
               bdd_pair_t p)
{
    const unsigned *cache_val = bdd_pair_ht_lookup (cache, p);
    if (cache_val != NULL) {
        return *cache_val;
    }
    else {
        const bdd_t b1 = p.first;
        const node_t n1 = get_node_by_idx (mgr, b1);
        const bdd_t b2 = p.second;
        const node_t n2 = get_node_by_idx (mgr, b2);
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
        bdd_pair_ht_insert (cache, p, result);
        return result;
    }
}

static bdd_t
bdd_apply (bdd_mgr_t *mgr, bdd_apply_op_fun op, bdd_t b1, bdd_t b2)
{
    bdd_pair_ht_t *cache;
    bdd_pair_t p;
    bdd_t result;

    bdd_mgr_check_invariants (mgr);
    assert (b1 < bdd_mgr_get_num_nodes(mgr));
    assert (b2 < bdd_mgr_get_num_nodes(mgr));

    cache = bdd_pair_ht_create ();
    p.first = b1;
    p.second = b2;
    result = bdd_apply_rec (mgr, cache, op, p);
    bdd_pair_ht_destroy (cache);

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
    return bdd_apply (mgr, bdd_and_fun, b1, b2);
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
    return bdd_apply (mgr, bdd_or_fun, b1, b2);
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
    return bdd_apply (mgr, bdd_xor_fun, b1, b2);
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
    return bdd_apply (mgr, bdd_equiv_fun, b1, b2);
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
    return bdd_apply (mgr, bdd_nand_fun, b1, b2);
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
    return bdd_apply (mgr, bdd_implies_fun, b1, b2);
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
            const node_t n = get_node_by_idx (mgr, b);
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

/* Negation of a BDD is implemented by traversing it and switching
 * terminal nodes. */
bdd_t
bdd_not (bdd_mgr_t *mgr, bdd_t b)
{
    bdd_ht_t *cache = bdd_ht_create ();
    const bdd_t r = bdd_not_rec (mgr, cache, b);
    bdd_ht_destroy (cache);
    return r;
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
        const node_t n = get_node_by_idx (mgr, b);
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
            const node_t b_node = get_node_by_idx(mgr, b);
            const node_t b_low = get_node_by_idx(mgr, b_node.low);
            const node_t b_high = get_node_by_idx(mgr, b_node.high);
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
    bdd_mgr_check_invariants (mgr);
    bdd_double_ht_t *cache = bdd_double_ht_create ();
    double result =
        pow (2, get_node_by_idx(mgr, b).var) *
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
        const node_t b_node = get_node_by_idx (mgr, b);
        return
            bdd_get_num_nodes (mgr, b_node.low) +
            bdd_get_num_nodes (mgr, b_node.high);
    }
}
