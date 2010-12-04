#include "bdd_impl.h"
#include "bdd_pair.h"
#include "bdd_pair_hash_table.h"

bdd
bdd_manager_get_ith_var (bdd_manager_t *mgr, unsigned i)
{
    bdd_manager_check_invariants (mgr);
    assert (i < mgr->num_vars);
    const node_t node = {i, bdd_false, bdd_true};
    const bdd ith_var = make_node (mgr, node);
    bdd_manager_check_invariants (mgr);
    assert (node_equal (node, node_vector_get (mgr->nodes_by_idx, ith_var)));
    return ith_var;
}

static bdd
bdd_eval_op_on_terminals (bdd_apply_binop op, bdd b1, bdd b2)
{
    assert (b1 <= 1);
    assert (b2 <= 1);

    bool result;

    switch (op) {
    case BDD_AND:
        result = b1 && b2;
        break;
    case BDD_OR:
        result = b1 || b2;
        break;
    case BDD_EQUIV:
        result = (b1 && b2) || (!b1 && !b2);
        break;
    default:
        abort ();
        break;
    }

    return result ? bdd_true : bdd_false;
}

static bdd
bdd_apply_rec (bdd_manager_t *mgr,
               bdd_pair_hash_table_t *cache,
               bdd_apply_binop op,
               bdd_pair_t p)
{
    const unsigned *cache_val = bdd_pair_hash_table_lookup (cache, p);
    if (cache_val != NULL) {
        return *cache_val;
    }
    else {
        const bdd b1 = p.first;
        const node_t n1 = node_vector_get(mgr->nodes_by_idx, b1);
        const bdd b2 = p.second;
        const node_t n2 = node_vector_get(mgr->nodes_by_idx, b2);
        bdd result;
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
            const bdd low = bdd_apply_rec (mgr, cache, op, p1);
            const bdd high = bdd_apply_rec (mgr, cache, op, p2);

            result = make_node_from_parts (
                mgr,
                var,
                low,
                high
                );
        }
        bdd_pair_hash_table_insert (cache, p, result);
        return result;
    }
}

bdd
bdd_apply (bdd_manager_t *mgr, bdd_apply_binop op, bdd b1, bdd b2)
{
    bdd_manager_check_invariants (mgr);
    assert (b1 < bdd_manager_get_num_nodes(mgr));
    assert (b2 < bdd_manager_get_num_nodes(mgr));
    bdd_pair_hash_table_t *cache = bdd_pair_hash_table_create ();
    const bdd_pair_t p = {b1, b2};
    const bdd result = bdd_apply_rec (mgr, cache, op, p);
    bdd_pair_hash_table_destroy (cache);
    bdd_manager_check_invariants (mgr);
    return result;
}


/* This is the naive implementation from Andersen's ``An Introduction
 * to Binary Decision Diagrams''.  This implementation has worst-case
 * exponential time complexity due to the two recursive calls in the
 * second case.  This can be improved by memoization / dynamic
 * programming. */
static bdd
bdd_res_rec (bdd_manager_t *mgr, const unsigned var, const bool val, bdd b)
{
    const node_t n = node_vector_get (mgr->nodes_by_idx, b);
    if (n.var > var)
        return b;
    else if (n.var < var) {
        const bdd low = bdd_res_rec (mgr, var, val, n.low);
        const bdd high = bdd_res_rec (mgr, var, val, n.high);
        return make_node_from_parts (mgr, n.var, low, high);
    }
    else if (!val)
        return bdd_res_rec (mgr, var, val, n.low);
    else
        return bdd_res_rec (mgr, var, val, n.high);
}

bdd
bdd_restrict_var (bdd_manager_t *mgr, bdd b, unsigned var, bool val)
{
    return bdd_res_rec (mgr, var, val, b);
}
