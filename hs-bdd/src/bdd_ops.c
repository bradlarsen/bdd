#include "bdd_impl.h"
#include "bdd_pair.h"
#include "bdd_pair_ht.h"
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

/* Binary operations to be used with bdd_apply. */
typedef enum {
    BDD_AND,
    BDD_OR,
    BDD_XOR,
    BDD_EQUIV,
    BDD_NAND,
    BDD_IMPLIES
} bdd_apply_binop;

/* Applies a binary operation to the two given BDDs.  O(m(b1) * m(b2))
 * time and space. */
static bdd_t
bdd_apply (bdd_mgr_t *mgr, bdd_apply_binop op, bdd_t b1, bdd_t b2);

static bdd_t
bdd_eval_op_on_terminals (bdd_apply_binop op, bdd_t b1, bdd_t b2)
{
    bool result;

    assert (b1 <= 1);
    assert (b2 <= 1);

    switch (op) {
    case BDD_AND:
        result = b1 && b2;
        break;
    case BDD_OR:
        result = b1 || b2;
        break;
    case BDD_XOR:
        result = (b1 && !b2) || (!b1 && b2);
        break;
    case BDD_EQUIV:
        result = (b1 && b2) || (!b1 && !b2);
        break;
    case BDD_NAND:
        result = !(b1 && b2);
        break;
    case BDD_IMPLIES:
        result = !b1 || b2;
        break;
    default:
        abort ();
        break;
    }

    return result ? bdd_true : bdd_false;
}

static bdd_t
bdd_apply_rec (bdd_mgr_t *mgr,
               bdd_pair_ht_t *cache,
               bdd_apply_binop op,
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
        bdd_t result;
        if (b1 <= 1 && b2 <= 1) {
            result = bdd_eval_op_on_terminals (op, b1, b2);
        }
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

bdd_t
bdd_apply (bdd_mgr_t *mgr, bdd_apply_binop op, bdd_t b1, bdd_t b2)
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

bdd_t
bdd_and (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2)
{
    return bdd_apply (mgr, BDD_AND, b1, b2);
}

bdd_t
bdd_or (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2)
{
    return bdd_apply (mgr, BDD_OR, b1, b2);
}

bdd_t
bdd_xor (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2)
{
    return bdd_apply (mgr, BDD_XOR, b1, b2);
}

bdd_t
bdd_equiv (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2)
{
    return bdd_apply (mgr, BDD_EQUIV, b1, b2);
}

bdd_t
bdd_nand (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2)
{
    return bdd_apply (mgr, BDD_NAND, b1, b2);
}

bdd_t
bdd_implies (bdd_mgr_t *mgr, bdd_t b1, bdd_t b2)
{
    return bdd_apply (mgr, BDD_IMPLIES, b1, b2);
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
    return bdd_apply (mgr, BDD_OR,
                      bdd_restrict (mgr, b, var, false),
                      bdd_restrict (mgr, b, var, true));
}

bdd_t
bdd_universal (bdd_mgr_t *mgr, unsigned var, bdd_t b)
{
    return bdd_apply (mgr, BDD_AND,
                      bdd_restrict (mgr, b, var, false),
                      bdd_restrict (mgr, b, var, true));
}

bdd_t
bdd_compose (bdd_mgr_t *mgr, bdd_t f, unsigned x, bdd_t g)
{
    return bdd_apply (mgr, BDD_OR,
                      bdd_apply (mgr, BDD_AND,
                                 bdd_not (mgr, g),
                                 bdd_restrict (mgr, f, x, false)),
                      bdd_apply (mgr, BDD_AND,
                                 g,
                                 bdd_restrict (mgr, f, x, true)));
}

static double
bdd_sat_count_rec (bdd_mgr_t *mgr, bdd_t b)
{
    if (b == bdd_false)
        return 0;
    else if (b == bdd_true)
        return 1;
    else {
        const node_t b_node = get_node_by_idx(mgr, b);
        const node_t b_low = get_node_by_idx(mgr, b_node.low);
        const node_t b_high = get_node_by_idx(mgr, b_node.high);
        return
            pow (2.0, b_low.var - b_node.var - 1) *
            bdd_sat_count_rec (mgr, b_node.low) +
            pow (2.0, b_high.var - b_node.var - 1) *
            bdd_sat_count_rec (mgr, b_node.high);
    }
}

double
bdd_sat_count (bdd_mgr_t *mgr, bdd_t b)
{
    bdd_mgr_check_invariants (mgr);
    return pow (2, get_node_by_idx(mgr, b).var) * bdd_sat_count_rec (mgr, b);
}
