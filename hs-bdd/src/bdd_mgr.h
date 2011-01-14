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
#include "hash_pair.h"
#include "memory.h"
#include "raw_bdd.h"
#include "usr_bdd_ht.h"

typedef struct
{
    int var;                           /* variable index of the node */
    raw_bdd_t low;                     /* value if the variable is false */
    raw_bdd_t high;                    /* value if the variable is true */
} node_t;

struct bdd_mgr
{
    unsigned num_vars;                 /* number of variables */

    unsigned capacity;                 /* number of allocated nodes */
    unsigned num_nodes;                /* number of used nodes */
    unsigned num_deleted_nodes;        /* number of deleted nodes */
    node_t *nodes;                     /* all the nodes */

#ifndef NDEBUG
    unsigned *hash_histo;
#endif

    usr_bdd_ht_t *usr_bdd_map;         /* user BDD -> raw BDD/ref count map */
    bdd_rtu_ht_t *raw_bdd_map;         /* raw BDD -> user BDD map */
    /* 'usr_bdd_map' and 'raw_bdd_map' form a one-to-one mapping */

    unsigned new_usr_id;               /* an unused user-level BDD id */

    /* the next two fields are garbage collection-related */
    unsigned next_gc_at_node_count;    /* next node count to GC at */
    unsigned num_unreferenced_bdds;    /* number of dead user-level BDDs */

    bdd_ite_cache_t ite_cache;         /* cache to memoize if-then-else op. */
    bdd_cache_stats_t ite_cache_stats; /* stats about 'ite_cache' */

    jmp_buf out_of_nodes_cxt;          /* to handle GC/resize/reorder */
};

static inline boolean
node_is_empty (node_t n)
{
    return n.var == INT_MAX;
}

static inline boolean
node_is_deleted (node_t n)
{
    return n.var == INT_MAX - 1;
}

static inline void
delete_node (node_t *n)
{
    n->var = INT_MAX - 1;
}

static inline boolean
raw_bdd_is_valid_and_live (bdd_mgr_t *mgr, raw_bdd_t raw)
{
    return raw < mgr->capacity && !node_is_empty(mgr->nodes[raw]);
}

/* Adjusts the storage of the given manager so that room for
 * 'new_capacity' nodes will be allocated.  The number of used nodes
 * must be less than 3/4 * 'new_capacity_hint'. */
extern void
bdd_mgr_resize (bdd_mgr_t *mgr, unsigned new_capacity_hint);

/* Interns the raw BDD index, mapping it to a new user-level BDD
 * index with a reference count of 0. */
extern bdd_t *
intern_raw_bdd (bdd_mgr_t *mgr, raw_bdd_t raw);

/* Converts a raw BDD index to a user-level BDD index.  Creates a
 * binding between the two if none exists. */
extern bdd_t *
raw_to_usr (bdd_mgr_t *mgr, raw_bdd_t raw);

/* Converts a user-level BDD index to a raw BDD index. */
extern raw_bdd_t
usr_to_raw (bdd_mgr_t *mgr, bdd_t *usr);

/* Gets the node associated with the given BDD. */
static inline node_t
raw_bdd_to_node (bdd_mgr_t *mgr, raw_bdd_t b)
{
    assert (raw_bdd_is_valid_and_live (mgr, b));
    return mgr->nodes[b];
}

/* FIXME: better document what's going on with exceptions */
/* Retrieves the BDD of the node equal to the node with the given
 * components if one exists, otherwise creates and returns a new BDD.
 * This function can raise an out-of-space exception, and so callers
 * must set up a handling context. */
extern raw_bdd_t
_bdd_make_node (
    bdd_mgr_t *mgr,
    int var,
    raw_bdd_t low,
    raw_bdd_t high
    );

#define _bdd_catch_out_of_nodes(mgr) setjmp (mgr->out_of_nodes_cxt)

#endif /* BDD_MGR_INCLUDED */