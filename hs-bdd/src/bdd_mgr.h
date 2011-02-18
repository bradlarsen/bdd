#ifndef BDD_MGR_INCLUDED
#define BDD_MGR_INCLUDED

#include <assert.h>
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bdd_ite_cache.h"
#include "bddlib.h"
#include "boolean.h"
#include "hash_pair.h"
#include "memory.h"

#define _BDD_MAX_REF_CNT UINT_MAX
#define _BDD_MAX_NUM_VARS UINT_MAX

typedef struct
{
    unsigned ref_cnt;     /* num refs from user formulae, other nodes,
                             and ITE cache */
    unsigned lvl;         /* level of the node in the DAG */
    bdd_t low;            /* value if the variable is false */
    bdd_t high;           /* value if the variable is true */
    unsigned hash_next;   /* one plus index of next node on hash
                           * chain, 0 means end-of-chain */
    unsigned lvl_prev;    /* one plus index of previous node on level
                           * chain, 0 means end-of-chain */
    unsigned lvl_next;    /* one plus index of next node on level
                           * chain, 0 means end-of-chain */
} node_t;

struct bdd_mgr
{
    node_t *nodes;                     /* all the nodes */
    unsigned last_used_alloc_idx;      /* index last used for a new node */

    unsigned *nodes_hash;              /* hash table mapping node to
                                        * index, values are indexes
                                        * into nodes plus 1, 0 used as
                                        * end-of-chain value */

    bdd_ite_cache_t ite_cache;         /* cache to memoize if-then-else op. */
    bdd_cache_stats_t ite_cache_stats; /* stats about 'ite_cache' */

    unsigned num_vars;                 /* number of variables */
    unsigned *lvl_to_var;              /* mapping from var index to var */
    unsigned *var_to_lvl;              /* mapping from var to var index */
    /* lvl_to_var and var_to_lvl are permutations of [0..num_vars) and
     * are inverses of each other. */

    unsigned *lvl_chain_roots;         /* list of nodes, 1 per level,
                                        * lvl_chain_roots[i] is one
                                        * plus index of first node on
                                        * list for level i */
    unsigned *nodes_at_level;          /* count of nodes at each level */

    unsigned capacity;                 /* number of allocated nodes, a
                                        * power of 2 */
    unsigned num_nodes;                /* number of used nodes */
};

static inline unsigned
_bdd_mgr_num_hash_buckets (bdd_mgr_t *mgr)
{
    return mgr->capacity / 2;
}

static inline boolean
node_equal (node_t n1, node_t n2)
{
    return n1.lvl == n2.lvl && n1.low == n2.low && n1.high == n2.high;
}

static inline unsigned
node_hash (node_t n)
{
    return hash_unsigned_pair (n.lvl, hash_unsigned_pair(n.low, n.high));
}

static inline boolean
node_is_empty (node_t n)
{
    return n.ref_cnt == 0;
}

static inline boolean
node_is_live (node_t n)
{
    return !node_is_empty (n);
}

static inline boolean
bdd_is_valid_and_live (bdd_mgr_t *mgr, bdd_t b)
{
    return b < mgr->capacity && node_is_live(mgr->nodes[b]);
}

/* Gets the node associated with the given BDD. */
static inline node_t
bdd_to_node (bdd_mgr_t *mgr, bdd_t b)
{
    assert (bdd_is_valid_and_live (mgr, b));
    return mgr->nodes[b];
}

/* Retrieves the BDD of the node equal to the node with the given
 * components if one exists, otherwise creates and returns a new BDD.
 * This function can raise an out-of-space exception, and so callers
 * must set up a handling context.
 */
extern bdd_t
_bdd_make_node (
    bdd_mgr_t *mgr,
    unsigned lvl,
    bdd_t low,
    bdd_t high
    );

static inline void
_bdd_inc_ref (bdd_mgr_t *mgr, bdd_t b)
{
    assert (b < mgr->capacity);
    assert (mgr->nodes[mgr->nodes[b].low].ref_cnt > 0);
    assert (mgr->nodes[mgr->nodes[b].high].ref_cnt > 0);
    if (mgr->nodes[b].ref_cnt < _BDD_MAX_REF_CNT)
        mgr->nodes[b].ref_cnt += 1;
}

static inline void
_bdd_dec_ref (bdd_mgr_t *mgr, bdd_t b)
{
    assert (mgr->nodes[b].ref_cnt > 0);
    if (mgr->nodes[b].ref_cnt < _BDD_MAX_REF_CNT)
        mgr->nodes[b].ref_cnt -= 1;
}

extern void
_bdd_dec_ref_rec (bdd_mgr_t *mgr, bdd_t b);

extern void
_node_ht_bucket_dump (bdd_mgr_t *mgr, char *prefix, unsigned bucket_idx);

static inline void
_node_ht_bucket_insert (bdd_mgr_t *mgr, unsigned node_idx, unsigned bucket_idx)
{
    assert (bucket_idx < _bdd_mgr_num_hash_buckets (mgr));
    mgr->nodes[node_idx].hash_next = mgr->nodes_hash[bucket_idx];
    mgr->nodes_hash[bucket_idx] = node_idx + 1;
}

extern void
_node_ht_bucket_delete (bdd_mgr_t *mgr, unsigned node_idx, unsigned bucket_idx);

static inline unsigned
_node_hash (bdd_mgr_t *mgr, unsigned lvl, bdd_t low, bdd_t high)
{
    unsigned mask = _bdd_mgr_num_hash_buckets (mgr) - 1;
    return hash_unsigned_pair (lvl, hash_unsigned_pair(low, high)) & mask;
}

static inline void
_node_ht_insert (bdd_mgr_t *mgr, unsigned node_idx)
{
    _node_ht_bucket_insert (mgr, node_idx,
                            _node_hash (mgr,
                                        mgr->nodes[node_idx].lvl,
                                        mgr->nodes[node_idx].low,
                                        mgr->nodes[node_idx].high));
}

static inline void
_node_ht_delete (bdd_mgr_t *mgr, unsigned node_idx)
{
    _node_ht_bucket_delete (mgr, node_idx,
                            _node_hash (mgr,
                                        mgr->nodes[node_idx].lvl,
                                        mgr->nodes[node_idx].low,
                                        mgr->nodes[node_idx].high));
}

#ifndef NDEBUG
void
_bdd_mgr_check_invariants(bdd_mgr_t *mgr);
#else
#define _bdd_mgr_check_invariants(mgr) do {} while (0)
#endif /* NDEBUG */

#endif /* BDD_MGR_INCLUDED */
