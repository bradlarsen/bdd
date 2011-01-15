#ifndef BDD_MGR_INCLUDED
#define BDD_MGR_INCLUDED

#include <assert.h>
#include <limits.h>
#include <math.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bdd.h"
#include "bdd_ite_cache.h"
#include "bdd_rtu_ht.h"
#include "bddlib.h"
#include "boolean.h"
#include "memory.h"
#include "raw_bdd.h"
#include "usr_bdd_ht.h"

typedef struct
{
    int idx;                           /* variable index of the node */
    raw_bdd_t low;                     /* value if the variable is false */
    raw_bdd_t high;                    /* value if the variable is true */
} node_t;

struct bdd_mgr
{
    node_t *nodes;                     /* all the nodes */

    bdd_ite_cache_t ite_cache;         /* cache to memoize if-then-else op. */
    bdd_cache_stats_t ite_cache_stats; /* stats about 'ite_cache' */

    unsigned num_vars;                 /* number of variables */

    unsigned capacity;                 /* number of allocated nodes */
    unsigned num_nodes;                /* number of used nodes */
    unsigned num_deleted_nodes;        /* number of deleted nodes */

    usr_bdd_ht_t *usr_bdd_map;         /* user BDD -> raw BDD/ref count map */
    bdd_rtu_ht_t *raw_bdd_map;         /* raw BDD -> user BDD map */
    /* 'usr_bdd_map' and 'raw_bdd_map' form a one-to-one mapping */

    unsigned new_usr_id;               /* an unused user-level BDD id */

    /* the next two fields are garbage collection-related */
    unsigned next_gc_at_node_count;    /* next node count to GC at */
    unsigned num_unreferenced_bdds;    /* number of dead user-level BDDs */

    jmp_buf out_of_nodes_cxt;          /* to handle out-of-nodes */
    boolean gc_interrupted_already;

#ifndef NDEBUG
    unsigned *hash_histo;              /* histogram of number of reprobes */
#endif
};

static inline boolean
node_is_empty (node_t n)
{
    return n.idx == INT_MAX;
}

static inline boolean
node_is_deleted (node_t n)
{
    return n.idx == INT_MAX - 1;
}

static inline void
delete_node (node_t *n)
{
    n->idx = INT_MAX - 1;
}

static inline boolean
raw_bdd_is_valid_and_live (bdd_mgr_t *mgr, raw_bdd_t raw)
{
    return raw < mgr->capacity && !node_is_empty(mgr->nodes[raw]);
}

/* Gets the node associated with the given BDD. */
static inline node_t
raw_bdd_to_node (bdd_mgr_t *mgr, raw_bdd_t b)
{
    assert (raw_bdd_is_valid_and_live (mgr, b));
    return mgr->nodes[b];
}

/* Adjusts the storage of the given manager so that room for
 * 'new_capacity' nodes will be allocated.  The number of used nodes
 * must be less than 3/4 * 'new_capacity_hint'. */
extern void
bdd_mgr_resize (bdd_mgr_t *mgr, unsigned new_capacity_hint);

/* FIXME: better document what's going on with exceptions */
/* Retrieves the BDD of the node equal to the node with the given
 * components if one exists, otherwise creates and returns a new BDD.
 * This function can raise an out-of-space exception, and so callers
 * must set up a handling context. */
extern raw_bdd_t
_bdd_make_node (
    bdd_mgr_t *mgr,
    int idx,
    raw_bdd_t low,
    raw_bdd_t high
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
    unsigned _i;                                                        \
    assert (mgr != NULL);                                               \
    assert (mgr->capacity >= 2);                                        \
    assert (_is_power_of_two (mgr->capacity));                          \
    assert (mgr->num_nodes <= 0.75 * mgr->capacity);                    \
    assert (mgr->num_nodes + mgr->num_deleted_nodes <= mgr->capacity);  \
    assert (mgr->nodes[0].idx > 0);                                     \
    assert ((unsigned)mgr->nodes[0].idx == mgr->num_vars);              \
    assert (mgr->nodes[0].low == 1);                                    \
    assert (mgr->nodes[0].high == 0);                                   \
    assert (mgr->nodes[1].idx > 0);                                     \
    assert ((unsigned)mgr->nodes[1].idx == mgr->num_vars);              \
    assert (mgr->nodes[1].low == 0);                                    \
    assert (mgr->nodes[1].high == 1);                                   \
    for (_i = 2; _i < mgr->capacity; _i += 1) {                         \
        node_t _n = mgr->nodes[_i];                                     \
        if (!node_is_empty(_n) && !node_is_deleted(_n)) {               \
            node_t _n_low, _n_high;                                     \
            assert (_n.idx >= 0);                                       \
            assert ((unsigned)_n.idx < mgr->num_vars);                  \
            assert (_n.low != _n.high);                                 \
                                                                        \
            assert (_n.low < mgr->capacity);                            \
            _n_low = mgr->nodes[_n.low];                                \
            assert (_n_low.idx > _n.idx);                               \
            assert (!node_is_empty(_n_low));                            \
            assert (!node_is_deleted(_n_low));                          \
                                                                        \
            assert (_n.high < mgr->capacity);                           \
            _n_high = mgr->nodes[_n.high];                              \
            assert (_n_high.idx > _n.idx);                              \
            assert (!node_is_empty(_n_high));                           \
            assert (!node_is_deleted(_n_high));                         \
        }                                                               \
    }                                                                   \
} while (0)
#else
#define _bdd_mgr_check_invariants(mgr) do {} while (0)
#endif

#endif /* BDD_MGR_INCLUDED */
