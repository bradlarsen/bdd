#ifndef BDD_MGR_INCLUDED
#define BDD_MGR_INCLUDED

#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

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
    unsigned var;             /* the variable index of the node */
    raw_bdd_t low;            /* the value if the variable is false */
    raw_bdd_t high;           /* the value if the variable is true */
} node_t;

struct bdd_mgr
{
    unsigned num_vars;                 /* number of variables */

    unsigned capacity;                 /* number of allocated nodes */
    unsigned num_nodes;                /* number of used nodes */
    node_t *nodes;                     /* all the nodes */

    unsigned *hash_histo;

    usr_bdd_ht_t *usr_bdd_map;         /* user BDD -> raw BDD/ref count map */
    bdd_rtu_ht_t *raw_bdd_map;         /* raw BDD -> user BDD map */
    /* 'usr_bdd_map' and 'raw_bdd_map' form a one-to-one mapping */

    unsigned new_usr_id;               /* an unused user-level BDD id */

    /* the next two fields are garbage collection-related */
    unsigned num_unreferenced_bdds;    /* number of dead user-level BDDs*/
    unsigned next_gc_at_node_count;    /* next node count to GC at */

    bdd_ite_cache_t ite_cache;         /* cache to memoize if-then-else op. */
    bdd_cache_stats_t ite_cache_stats; /* stats about 'ite_cache' */
};

static inline boolean
node_is_empty (node_t n)
{
    return n.var == UINT_MAX;
}

static inline boolean
raw_bdd_is_valid_and_live (bdd_mgr_t *mgr, raw_bdd_t raw)
{
    return (0 <= raw && (unsigned)raw < mgr->capacity &&
            !node_is_empty(mgr->nodes[(unsigned)raw]));
}

/* Adjusts the storage of the given manager so that room for
 * 'new_capacity' nodes will be allocated.  The number of used nodes
 * must be less than 3/4 * 'new_capacity_hint'. */
extern void
bdd_mgr_resize (bdd_mgr_t *mgr, unsigned new_capacity_hint);

/* Interns the raw BDD index, mapping it to a new user-level BDD
 * index with a reference count of 0. */
static inline bdd_t *
intern_raw_bdd (bdd_mgr_t *mgr, raw_bdd_t raw)
{
    bdd_t *usr;
    usr_bdd_entry_t entry;
    assert (raw_bdd_is_valid_and_live (mgr, raw));
    assert (bdd_rtu_ht_lookup (mgr->raw_bdd_map, raw) == NULL);
    usr = (bdd_t *) checked_malloc (sizeof(bdd_t));
    usr->id = mgr->new_usr_id;
    assert (usr_bdd_ht_lookup (mgr->usr_bdd_map, usr) == NULL);
    mgr->new_usr_id += 1;
    bdd_rtu_ht_insert (mgr->raw_bdd_map, raw, usr);
    entry.raw_bdd = raw;
    entry.ref_cnt = 0;
    usr_bdd_ht_insert (mgr->usr_bdd_map, usr, entry);
    mgr->num_unreferenced_bdds += 1;
    return usr;
}

/* Converts a raw BDD index to a user-level BDD index.  Creates a
 * binding between the two if none exists. */
static inline bdd_t *
raw_to_usr (bdd_mgr_t *mgr, raw_bdd_t raw)
{
    bdd_t **res;
    assert (raw_bdd_is_valid_and_live (mgr, raw));
    res = bdd_rtu_ht_lookup (mgr->raw_bdd_map, raw);
    if (res == NULL)
        return intern_raw_bdd (mgr, raw);
    else
        return *res;
}

/* Converts a user-level BDD index to a raw BDD index. */
static inline raw_bdd_t
usr_to_raw (bdd_mgr_t *mgr, bdd_t *usr)
{
    usr_bdd_entry_t *res = usr_bdd_ht_lookup (mgr->usr_bdd_map, usr);
    assert (res != NULL);
    assert (raw_bdd_is_valid_and_live (mgr, res->raw_bdd));
    return res->raw_bdd;
}

/* Gets the node associated with the given BDD. */
static inline node_t
raw_bdd_to_node (bdd_mgr_t *mgr, raw_bdd_t b)
{
    assert (raw_bdd_is_valid_and_live (mgr, b));
    return mgr->nodes[(unsigned) b];
}

static inline unsigned
node_hash (unsigned var, raw_bdd_t low, raw_bdd_t high)
{
    return hash_pair(var, hash_pair(low, high)) % 999999937;
}

/* Retrieves the BDD of the node equal to the node with the given
 * components if one exists, otherwise creates and returns a new
 * BDD. */
static inline raw_bdd_t
make_node (
    bdd_mgr_t *mgr,
    unsigned var,
    raw_bdd_t low,
    raw_bdd_t high
    )
{
    if (low == high)
        return low;
    else {
        /* try to find existing node */
        unsigned loop_count;
        unsigned idx = node_hash (var, low, high) % mgr->capacity;
        loop_count = 0;
        while (!node_is_empty(mgr->nodes[idx])) {
            node_t n = mgr->nodes[idx];
            loop_count += 1;
            if (n.var == var && n.low == low && n.high == high) {
                mgr->hash_histo[loop_count] += 1;
                return (raw_bdd_t)idx;
            }
            idx = (idx + 1) % mgr->capacity;
        }
        mgr->hash_histo[loop_count] += 1;

        /* add a new node; grow if needed */
        if (mgr->num_nodes >= 0.75 * mgr->capacity)
            bdd_mgr_resize (mgr, mgr->capacity * 2);

        assert (mgr->num_nodes < 0.75 * mgr->capacity);
        mgr->nodes[idx].var = var;
        mgr->nodes[idx].low = low;
        mgr->nodes[idx].high = high;
        mgr->num_nodes += 1;
        return (raw_bdd_t)idx;
    }
}

#endif /* BDD_MGR_INCLUDED */
