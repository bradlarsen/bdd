/* BDD garbage collection code. */

#include "bdd_mgr.h"

void
bdd_gc_stats_fprint (FILE *handle, bdd_gc_stats_t stats)
{
    double n_usr_nodes, n_raw_nodes, np_usr_nodes, np_raw_nodes;

    n_usr_nodes = stats.usr_bdd_num_copied + stats.usr_bdd_num_collected;
    np_usr_nodes = stats.usr_bdd_num_collected;

    n_raw_nodes = stats.raw_bdd_num_copied + stats.raw_bdd_num_collected;
    np_raw_nodes = stats.raw_bdd_num_collected;

    fprintf (handle, "user: %.0f/%.0f (%.2f%%) collected\n",
             np_usr_nodes, n_usr_nodes, np_usr_nodes / n_usr_nodes * 100);
    fprintf (handle, "internal: %.0f/%.0f (%.2f%%) collected\n",
             np_raw_nodes, n_raw_nodes, np_raw_nodes / n_raw_nodes * 100);
}

static bdd_gc_stats_t
make_bdd_gc_stats_t ()
{
    bdd_gc_stats_t res;
    res.usr_bdd_num_copied = 0;
    res.usr_bdd_num_collected = 0;
    res.raw_bdd_num_copied = 0;
    res.raw_bdd_num_collected = 0;
    return res;
}

typedef struct
{
    bdd_mgr_t *mgr;
    bdd_gc_stats_t stats;
} bdd_gc_env_t;

static inline boolean
node_is_marked (node_t n)
{
    return n.var < 0;
}

static inline void
mark_node (bdd_mgr_t *mgr, node_t *n)
{
    assert (!node_is_marked (*n));
    assert (!node_is_deleted (*n));
    assert (!node_is_empty (*n));
    n->var = n->var - mgr->num_vars - 1;
    assert (node_is_marked (*n));
    assert (!node_is_deleted (*n));
    assert (!node_is_empty (*n));
}

static inline void
unmark_node (bdd_mgr_t *mgr, node_t *n)
{
    if (n->var < 0)
        n->var += mgr->num_vars + 1;
    assert (!node_is_marked (*n));
}

/* Mark all the nodes under the given root by making their 'var'
 * fields negative. */
static void
mark_bdd_rec (bdd_mgr_t *mgr, raw_bdd_t root)
{
    node_t *root_node = &mgr->nodes[root];
    assert (!node_is_empty (*root_node));
    assert (!node_is_deleted (*root_node));
    if (!node_is_marked(*root_node)) {
        mark_bdd_rec (mgr, root_node->low);
        mark_bdd_rec (mgr, root_node->high);
        mark_node (mgr, root_node);
        assert (!node_is_empty (*root_node));
        assert (!node_is_deleted (*root_node));
        assert (node_is_marked (*root_node));
    }
}

static void
gc_usr_bdd (void *env, bdd_t *key, usr_bdd_entry_t *val)
{
    bdd_gc_env_t *gc_env = (bdd_gc_env_t *)env;
    if (raw_bdd_is_terminal(val->raw_bdd) || val->ref_cnt > 0) {
        gc_env->stats.usr_bdd_num_copied += 1;
        usr_bdd_ht_insert (gc_env->mgr->usr_bdd_map, key, *val);
        bdd_rtu_ht_insert (gc_env->mgr->raw_bdd_map, val->raw_bdd, key);
        mark_bdd_rec (gc_env->mgr, val->raw_bdd);
    }
    else {
        gc_env->stats.usr_bdd_num_collected += 1;
        checked_free (key);
    }
}

bdd_gc_stats_t
bdd_mgr_perform_gc (bdd_mgr_t *mgr)
{
    bdd_gc_env_t env;
    usr_bdd_ht_t *old_usr_bdd_map;
    bdd_rtu_ht_t *old_raw_bdd_map;
    unsigned old_num_nodes;
    unsigned i;
    static boolean collecting = bfalse;

    assert (!collecting);
    collecting = btrue;
    fprintf (stderr, "******** BEGIN GC\n");
    env.mgr = mgr;
    env.stats = make_bdd_gc_stats_t ();

    old_num_nodes = mgr->num_nodes;

    /* FIXME: use a better hash table */
    /* It should allow for easy iteration & deletion, so that all this
     * copying is not needed */
    old_usr_bdd_map = mgr->usr_bdd_map;
    old_raw_bdd_map = mgr->raw_bdd_map;
    mgr->usr_bdd_map = usr_bdd_ht_create ();
    mgr->raw_bdd_map = bdd_rtu_ht_create ();
    mark_node (mgr, &mgr->nodes[raw_bdd_false]);
    mark_node (mgr, &mgr->nodes[raw_bdd_true]);
    usr_bdd_ht_map_entries (old_usr_bdd_map, (void *)&env, gc_usr_bdd);
    bdd_rtu_ht_destroy (old_raw_bdd_map);
    usr_bdd_ht_destroy (old_usr_bdd_map);

    for (i = 0; i < mgr->capacity; i += 1) {
        node_t *n = &mgr->nodes[i];
        if (!node_is_empty(*n) && !node_is_deleted(*n) && !node_is_marked(*n)) {
            mgr->num_nodes -= 1;
            mgr->num_deleted_nodes += 1;
            delete_node (n);
            env.stats.raw_bdd_num_collected += 1;
        }
        else
            env.stats.raw_bdd_num_copied += 1;
    }

    for (i = 0; i < mgr->capacity; i += 1)
        unmark_node (mgr, &mgr->nodes[i]);

    for (i = 0; i < mgr->capacity; i += 1)
        assert (!node_is_marked (mgr->nodes[i]));

    /* FIXME: patch up the cache instead of clearing it */
    bdd_ite_cache_clear (&mgr->ite_cache);

    bdd_gc_stats_fprint (stderr, env.stats);
    fprintf (stderr, "******** END GC\n");

    collecting = bfalse;
    return env.stats;
}
