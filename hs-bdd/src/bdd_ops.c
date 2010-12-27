#include "bdd_impl.h"
#include "bdd_pair.h"
#include "bdd_pair_cache.h"
#include "bdd_double_ht.h"
#include "bdd_ht.h"
#include <stdio.h>

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

typedef enum {make, apply} apply_command_type;

typedef struct
{
    apply_command_type ctype;
    union {
        struct {
            unsigned var;
            bdd_pair_t input;
        } make_input;
        bdd_pair_t apply_input;
    };
} apply_command_t;

static inline apply_command_t
make_node_cmd (unsigned var, bdd_pair_t input)
{
    apply_command_t cmd;
    cmd.ctype = make;
    cmd.make_input.var = var;
    cmd.make_input.input = input;
    return cmd;
}

static inline apply_command_t
apply_cmd (bdd_pair_t input)
{
    apply_command_t cmd;
    cmd.ctype = apply;
    cmd.apply_input = input;
    return cmd;
}

static void
fprint_command (FILE *f, apply_command_t cmd)
{
    if (cmd.ctype == make)
        fprintf (
            f, "MAKE %u %d %d\n",
            cmd.make_input.var,
            cmd.make_input.input.first,
            cmd.make_input.input.second
            );
    else
        fprintf (
            f, "APPLY %d %d\n",
            cmd.apply_input.first,
            cmd.apply_input.second
            );
}

#define STACK_PUSH(stk, val) (stk)[++(stk ## _idx)] = (val)
#define STACK_POP(stk) (stk)[(stk ## _idx)--]
#define STACK_EMPTY(stk) ((stk ## _idx) == -1)

static void
do_make_cmd (
    bdd_mgr_t *mgr,
    bdd_pair_cache_t cache,
    bdd_t *res_stk,
    int *res_stk_idx_ptr,
    apply_command_t cmd
    )
{
    bdd_t low, high, res;
    cache_entry_t *cache_val;
    int res_stk_idx = *res_stk_idx_ptr;
    /* fprintf (stderr, "!!! do_make_cmd\n"); */
    low = STACK_POP (res_stk);
    high = STACK_POP (res_stk);
    res = make_node_from_parts (mgr, cmd.make_input.var, low, high);
    STACK_PUSH (res_stk, res);
    cache_val = bdd_pair_cache_lookup (cache, cmd.make_input.input);
    cache_val->key = cmd.make_input.input;
    cache_val->value = res;
    *res_stk_idx_ptr = res_stk_idx;
}

static void
do_apply_cmd (
    bdd_mgr_t *mgr,
    bdd_pair_cache_t cache,
    bdd_apply_op_fun op,
    apply_command_t *cmd_stk,
    int *cmd_stk_idx_ptr,
    bdd_t *res_stk,
    int *res_stk_idx_ptr,
    apply_command_t cmd
    )
{
    cache_entry_t *cache_val;
    int cmd_stk_idx;
    int res_stk_idx;

    /* fprintf (stderr, "!!! do_apply_cmd\n"); */
    cmd_stk_idx = *cmd_stk_idx_ptr;
    res_stk_idx = *res_stk_idx_ptr;

    cache_val = bdd_pair_cache_lookup (cache, cmd.apply_input);
    if (bdd_pair_equal (cache_val->key, cmd.apply_input))
        STACK_PUSH (res_stk, cache_val->value);
    else {
        const bdd_t b1 = cmd.apply_input.first;
        const node_t n1 = bdd_get_node (mgr, b1);
        const bdd_t b2 = cmd.apply_input.second;
        const node_t n2 = bdd_get_node (mgr, b2);
        const maybe_bdd_t mresult = op (b1, b2);
        if (mresult.has_value)
            STACK_PUSH (res_stk, mresult.value);
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
            STACK_PUSH (cmd_stk, make_node_cmd (var, cmd.apply_input));
            /* fprintf (stderr, "!!! push "); */
            /* fprint_command (stderr, make_node_cmd (var, cmd.apply_input)); */
            STACK_PUSH (cmd_stk, apply_cmd (p1));
            /* fprintf (stderr, "!!! push "); */
            /* fprint_command (stderr, apply_cmd (p1)); */
            STACK_PUSH (cmd_stk, apply_cmd (p2));
            /* fprintf (stderr, "!!! push "); */
            /* fprint_command (stderr, apply_cmd (p2)); */
        }
    }

    /* fprintf (stderr, "!!! *res_stk_idx_ptr is %d\n", *res_stk_idx_ptr); */
    /* fprintf (stderr, "!!! res_stk_idx is %d\n", res_stk_idx); */
    /* fprintf (stderr, "!!! *cmd_stk_idx_ptr is %d\n", *cmd_stk_idx_ptr); */
    /* fprintf (stderr, "!!! cmd_stk_idx is %d\n", cmd_stk_idx); */
    assert ((*res_stk_idx_ptr + 1 == res_stk_idx) !=
            (*cmd_stk_idx_ptr + 3 == cmd_stk_idx));

    *res_stk_idx_ptr = res_stk_idx;
    *cmd_stk_idx_ptr = cmd_stk_idx;
}

static bdd_t
bdd_apply_nonrec (bdd_mgr_t *mgr,
                  bdd_pair_cache_t cache,
                  bdd_apply_op_fun op,
                  bdd_pair_t start_pair)
{
    apply_command_t cmd_stk[1000];    /* stack for commands */
    int cmd_stk_idx;
    bdd_t res_stk[1000];              /* stack for results */
    int res_stk_idx;
    bdd_t result;                /* final result */

    cmd_stk_idx = -1;
    STACK_PUSH (cmd_stk, apply_cmd (start_pair));
    /* fprintf (stderr, "!!! push "); */
    /* fprint_command (stderr, apply_cmd (start_pair)); */
    assert (cmd_stk_idx == 0);
    res_stk_idx = -1;

    while (!STACK_EMPTY(cmd_stk)) {
        apply_command_t cmd = STACK_POP (cmd_stk);
        /* fprint_command (stderr, cmd); */
        if (cmd.ctype == make) {
            do_make_cmd (mgr, cache, res_stk, &res_stk_idx, cmd);
        }
        else {
            do_apply_cmd (mgr, cache, op,
                          cmd_stk, &cmd_stk_idx,
                          res_stk, &res_stk_idx,
                          cmd);
        }
    }

    /* fprintf (stderr, "!!! cmd_stk_idx is %d\n", cmd_stk_idx); */
    /* fprintf (stderr, "!!! res_stk_idx is %d\n", res_stk_idx); */
    assert (cmd_stk_idx == -1);
    assert (res_stk_idx == 0);
    result = STACK_POP (res_stk);

    return result;
}

#undef STACK_EMPTY
#undef STACK_POP
#undef STACK_PUSH


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
    assert (0 <= b1);
    assert (0 <= b2);
    assert ((unsigned)b1 < bdd_mgr_get_num_nodes(mgr));
    assert ((unsigned)b2 < bdd_mgr_get_num_nodes(mgr));

    p.first = b1;
    p.second = b2;
    result = bdd_apply_nonrec (mgr, cache, op, p);

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
