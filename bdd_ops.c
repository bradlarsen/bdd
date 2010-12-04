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
    return ith_var;
}

static bdd
bdd_eval_op_on_terminals (bdd_apply_binop op, bdd b1, bdd b2)
{
    assert (b1 <= 1);
    assert (b2 <= 1);

    switch (op) {
    case BDD_AND:
        return b1 && b2;
    case BDD_OR:
        return b1 || b2;
    case BDD_EQUIV:
        return (b1 && b2) || (!b1 && !b2);
    default:
        abort ();
    }
}

static bdd
bdd_apply_rec (bdd_manager_t *mgr,
               bdd_pair_hash_table_t *cache,
               bdd_apply_binop op,
               bdd_pair_t p)
{
    const unsigned *cache_val = bdd_pair_hash_table_lookup (cache, p);
    if (cache_val != NULL)
        return *cache_val;
    else {
        const bdd b1 = p.first;
        const node_t n1 = node_vector_get(mgr->nodes_by_idx, b1);
        const bdd b2 = p.second;
        const node_t n2 = node_vector_get(mgr->nodes_by_idx, b2);
        bdd result;
        if (b1 <= 1 && b2 <= 1)
            result = bdd_eval_op_on_terminals (op, b1, b2);
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
        bdd_pair_hash_table_insert (cache, p, result);
        return result;
    }
}

bdd
bdd_apply (bdd_manager_t *mgr, bdd_apply_binop op, bdd b1, bdd b2)
{
    bdd_manager_check_invariants (mgr);
    bdd_pair_hash_table_t *cache = bdd_pair_hash_table_create ();
    const bdd_pair_t p = {b1, b2};
    const bdd result = bdd_apply_rec (mgr, cache, op, p);
    bdd_pair_hash_table_destroy (cache);
    bdd_manager_check_invariants (mgr);
    return result;
}
