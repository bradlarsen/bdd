#ifndef BDD_IMPL_INCLUDED
#define BDD_IMPL_INCLUDED

#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "bddlib.h"
#include "bdd_rtu_ht.h"
#include "bdd_ite_cache.h"
#include "memory.h"
#include "node.h"
#include "node_vec.h"
#include "node_ht.h"
#include "usr_bdd_ht.h"
#include "bdd.h"
#include "raw_bdd.h"

struct bdd_mgr
{
    unsigned num_vars;                 /* number of variables */

    node_vec_t nodes_by_idx;           /* index -> node map */
    node_ht_t idxs_by_node;            /* node -> index map */
    /* 'nodes_by_idx' and 'idxs_by_node' form a one-to-one mapping */

    usr_bdd_ht_t *usr_bdd_map;         /* user BDD -> raw BDD/ref count map */
    bdd_rtu_ht_t *raw_bdd_map;         /* raw BDD -> user BDD map */
    /* 'usr_bdd_map' and 'raw_bdd_map' form a one-to-one mapping */

    unsigned new_usr_id;               /* an unused user-level BDD id */

    /* the next three fields are garbage collection-related */
    unsigned num_unreferenced_bdds;    /* number of dead user-level BDDs*/
    node_vec_t old_nodes_by_idx;       /* alternate index -> node map */

    bdd_ite_cache_t ite_cache;         /* cache to memoize if-then-else op. */
    bdd_cache_stats_t ite_cache_stats; /* stats about 'ite_cache' */
};

/* FIXME: inline is not C89 */
/* Gets the node associated with the given BDD. */
static inline node_t
raw_bdd_to_node (bdd_mgr_t *mgr, raw_bdd_t b)
{
    return node_vec_get (&mgr->nodes_by_idx, (unsigned) b);
}

/* Interns the raw BDD index, mapping it to a new user-level BDD
 * index with a reference count of 0. */
static inline bdd_t *
intern_raw_bdd (bdd_mgr_t *mgr, raw_bdd_t raw)
{
    bdd_t *usr;
    usr_bdd_entry_t entry;
    assert ((unsigned)raw < bdd_mgr_get_num_nodes (mgr));
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
    assert ((unsigned)raw < bdd_mgr_get_num_nodes (mgr));
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
    assert ((unsigned)res->raw_bdd < bdd_mgr_get_num_nodes (mgr));
    return res->raw_bdd;
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
        raw_bdd_t *existing_bdd;
        node_t node;
        node.var = var;
        node.low = low;
        node.high = high;
        existing_bdd = node_ht_lookup (&mgr->idxs_by_node, node);
        if (existing_bdd != NULL)
            return *existing_bdd;
        else {
            unsigned idx;
            idx = node_vec_get_num_elems (&mgr->nodes_by_idx);
            node_vec_push_back (&mgr->nodes_by_idx, node);
            node_ht_insert (&mgr->idxs_by_node, node, (raw_bdd_t)idx);
            return (raw_bdd_t)idx;
        }
    }
}

#endif /* BDD_IMPL_INCLUDED */
