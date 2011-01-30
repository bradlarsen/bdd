#ifndef BDD_MGR_INCLUDED
#define BDD_MGR_INCLUDED

#include <assert.h>
#include <limits.h>
#include <math.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bdd_ite_cache.h"
#include "bddlib.h"
#include "boolean.h"
#include "hash_pair.h"
#include "memory.h"

typedef struct
{
    unsigned lvl;              /* level of the node in the DAG */
    bdd_t low;                 /* value if the variable is false */
    bdd_t high;                /* value if the variable is true */
    unsigned hash_next;        /* index of next node on hash chain */
    unsigned level_next;       /* index of next node on level chain */
} node_t;

struct bdd_mgr
{
    node_t *nodes;                     /* all the nodes */

    bdd_ite_cache_t ite_cache;         /* cache to memoize if-then-else op. */
    bdd_cache_stats_t ite_cache_stats; /* stats about 'ite_cache' */

    unsigned num_vars;                 /* number of variables */
    unsigned *lvl_to_var;              /* mapping from var index to var */
    unsigned *var_to_lvl;              /* mapping from var to var index */
    /* lvl_to_var and var_to_lvl are permutations of [0..num_vars) and
     * are inverses of each other. */

    unsigned capacity;                 /* number of allocated nodes, a
                                        * power of 2 */
    unsigned num_nodes;                /* number of used nodes */

    jmp_buf out_of_nodes_cxt;          /* for out-of-nodes exception */
};

static inline boolean
node_is_empty (node_t n)
{
    return n.lvl == UINT_MAX;
}

static inline void
set_node_empty (node_t *n)
{
    n->lvl = UINT_MAX;
}

static inline boolean
node_is_live (node_t n)
{
    return !node_is_empty (n);
}

static inline boolean
node_equal (node_t n1, node_t n2)
{
    return n1.lvl == n2.lvl && n1.low == n2.low && n1.high == n2.high;
}

static inline boolean
bdd_is_valid_and_live (bdd_mgr_t *mgr, bdd_t raw)
{
    return raw < mgr->capacity && node_is_live (mgr->nodes[raw]);
}

/* Gets the node associated with the given BDD. */
static inline node_t
bdd_to_node (bdd_mgr_t *mgr, bdd_t b)
{
    assert (bdd_is_valid_and_live (mgr, b));
    return mgr->nodes[b];
}

/* Adjusts the storage of the given manager so that room for
 * 'new_capacity' nodes will be allocated.  The number of used nodes
 * must be less than 3/4 * 'new_capacity_hint'. */
extern void
_bdd_mgr_resize (bdd_mgr_t *mgr, unsigned new_capacity_hint);

/* FIXME: better document what's going on with exceptions */
/* Retrieves the BDD of the node equal to the node with the given
 * components if one exists, otherwise creates and returns a new BDD.
 * This function can raise an out-of-space exception, and so callers
 * must set up a handling context. */
extern bdd_t
_bdd_make_node (
    bdd_mgr_t *mgr,
    unsigned lvl,
    bdd_t low,
    bdd_t high
    );

#define _bdd_catch_out_of_nodes(mgr) setjmp (mgr->out_of_nodes_cxt)

#ifndef NDEBUG
static inline boolean
_is_power_of_two (unsigned n)
{
    unsigned i;
    for (i = 1; i < n; i *= 2) {}
    return i == n;
}

#define _bdd_mgr_check_invariants(mgr)                                  \
do {                                                                    \
    unsigned _i, _j;                                                    \
    assert (mgr != NULL);                                               \
    assert (mgr->num_vars > 0);                                         \
    assert (mgr->capacity >= 2);                                        \
    assert (_is_power_of_two (mgr->capacity));                          \
    assert (mgr->nodes[0].lvl > 0);                                     \
    assert (mgr->nodes[0].lvl == mgr->num_vars);                        \
    assert (mgr->nodes[0].low == 1);                                    \
    assert (mgr->nodes[0].high == 0);                                   \
    assert (mgr->nodes[1].lvl > 0);                                     \
    assert (mgr->nodes[1].lvl == mgr->num_vars);                        \
    assert (mgr->nodes[1].low == 0);                                    \
    assert (mgr->nodes[1].high == 1);                                   \
    for (_i = 2; _i < mgr->capacity; _i += 1) {                         \
        node_t _n = mgr->nodes[_i];                                     \
        if (node_is_live (_n)) {                                        \
            node_t _n_low, _n_high;                                     \
            assert (_n.lvl >= 0);                                       \
            assert (_n.lvl < mgr->num_vars);                            \
            assert (_n.low != _n.high);                                 \
                                                                        \
            assert (_n.low < mgr->capacity);                            \
            _n_low = mgr->nodes[_n.low];                                \
            assert (_n_low.lvl > _n.lvl);                               \
            assert (node_is_live (_n_low));                             \
                                                                        \
            assert (_n.high < mgr->capacity);                           \
            _n_high = mgr->nodes[_n.high];                              \
            assert (_n_high.lvl > _n.lvl);                              \
            assert (node_is_live (_n_high));                            \
        }                                                               \
    }                                                                   \
    for (_i = 0; _i < mgr->num_vars; _i += 1) {                         \
        assert (mgr->lvl_to_var[mgr->var_to_lvl[_i]] == _i);            \
        assert (mgr->var_to_lvl[mgr->lvl_to_var[_i]] == _i);            \
    }                                                                   \
    for (_i = 0; _i < mgr->capacity; _i += 1)                           \
        for (_j = _i + 1; _j < mgr->capacity; _j += 1)                  \
            if (node_is_live (mgr->nodes[_i]) &&                        \
                node_is_live (mgr->nodes[_j]))                          \
                assert (!node_equal (mgr->nodes[_i], mgr->nodes[_j]));  \
} while (0)
#else
#define _bdd_mgr_check_invariants(mgr) do {} while (0)
#endif

#endif /* BDD_MGR_INCLUDED */
