#include "bdd_impl.h"
#include "bdd_ht.h"

/* FIXME: rewrite the garbage collection code.
 *
 * There are several ways it could be improved.  First, this collector
 * more-or-less follows Cheney's algorithm (1960), in which the heap
 * is split into two equal-sized components, with the active heap
 * switching sides at each collection.  I could improve performance a
 * bit by keeping these two heaps (i.e., node vector and node hash
 * table) within each manager.  Second, I could modify the hash tables
 * for user-level BDDs so that values can be deleted, so that the
 * entire table need not be copied.  Third, I need to better deal with
 * copying the terminal nodes.  Fourth, I should bring back the
 * manager invariant checking code to help smoke out bugs.  Finally, I
 * should rewrite this code so that it is obvious there are now
 * bugs---this will require better stating my invariants.
 */

typedef struct
{
    bdd_mgr_t *src_mgr;         /* source */
    bdd_mgr_t *dst_mgr;         /* destination */
    bdd_gc_stats_t stats;       /* collection statistics */
} bdd_gc_env_t;

/* An important note: The source manager is trashed in the process of
 * garbage collection.  In order to avoid allocating additional memory
 * during collection, the node vector of the source manager is used to
 * map raw_bdd_t from the old manager to raw_bdd_t of the new manager.
 * This is done as follows: given a node n at index i in the source
 * manager, if n.var == UINT_MAX, then the node at index i was copied
 * to index n.low in the destination manager.
 */

/* Copies the BDD subgraph with the given root from the source to the
 * destination manager, returning the new root. */
static raw_bdd_t
copy_bdd_rec (
    bdd_gc_env_t *env,
    raw_bdd_t old_root
    )
{
    node_t old_root_node;
    raw_bdd_t new_low, new_high, new_root;
    old_root_node = raw_bdd_to_node (env->src_mgr, old_root);
    if (old_root_node.var == UINT_MAX)
        return old_root_node.low;
    else {
        env->stats.raw_bdd_num_copied += 1;
        if (old_root == raw_bdd_true || old_root == raw_bdd_false) {
            new_low = old_root_node.low;
            new_high = old_root_node.high;
        }
        else {
            new_low = copy_bdd_rec (env, old_root_node.low);
            new_high = copy_bdd_rec (env, old_root_node.high);
        }
        assert (new_low != new_high);
        new_root = make_node (env->dst_mgr,
                              old_root_node.var, new_low, new_high);
        old_root_node.var = UINT_MAX;
        old_root_node.low = new_root;
        node_vec_set (&env->src_mgr->nodes_by_idx, old_root, old_root_node);
        assert (old_root != raw_bdd_false || old_root == new_root);
        assert (old_root != raw_bdd_true || old_root == new_root);
        return new_root;
    }
}

static void
copy_live_free_dead_usr_nodes (void *env, bdd_t *key, usr_bdd_entry_t *val)
{
    bdd_gc_env_t *gc_env = (bdd_gc_env_t *)env;
    if (val->ref_cnt > 0) {
        usr_bdd_entry_t new_entry;
        gc_env->stats.usr_bdd_num_copied += 1;
        new_entry.ref_cnt = val->ref_cnt;
        new_entry.raw_bdd = copy_bdd_rec (gc_env, val->raw_bdd);
        usr_bdd_ht_insert (gc_env->dst_mgr->usr_bdd_map, key, new_entry);
        bdd_rtu_ht_insert (gc_env->dst_mgr->raw_bdd_map,
                           new_entry.raw_bdd,
                           key);
    }
    else {
        gc_env->stats.usr_bdd_num_collected += 1;
        checked_free (key);
    }
}

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
mk_bdd_gc_stats_t ()
{
    bdd_gc_stats_t res;
    res.usr_bdd_num_copied = 0;
    res.usr_bdd_num_collected = 0;
    res.raw_bdd_num_copied = 0;
    res.raw_bdd_num_collected = 0;
    return res;
}

void
bdd_mgr_perform_gc (bdd_mgr_t *mgr)
{
    bdd_gc_env_t env;
    bdd_mgr_t dst_mgr;
    unsigned old_num_nodes;

    old_num_nodes = bdd_mgr_get_num_nodes (mgr);
    bdd_mgr_initialize_with_hint (&dst_mgr,
                                  bdd_mgr_get_num_vars (mgr),
                                  bdd_mgr_get_num_allocated (mgr));
    dst_mgr.new_usr_id = mgr->new_usr_id;
    dst_mgr.ite_cache_stats = mgr->ite_cache_stats;
    env.src_mgr = mgr;
    env.dst_mgr = &dst_mgr;
    env.stats = mk_bdd_gc_stats_t ();

    {
        /* Special case to make sure the terminal nodes are copied
         * appropriately.  Without this, previously, sometimes the
         * terminal nodes would be flipped during collection, which
         * silently gave incorrect results.  This is a hack, and the
         * GC code should be rewritten. */
        bdd_t *false_bdd, *true_bdd;
        usr_bdd_entry_t *false_bdd_entry, *true_bdd_entry;
        false_bdd = bdd_false (mgr);
        true_bdd = bdd_true (mgr);
        false_bdd_entry = usr_bdd_ht_lookup (mgr->usr_bdd_map, false_bdd);
        true_bdd_entry = usr_bdd_ht_lookup (mgr->usr_bdd_map, true_bdd);
        copy_live_free_dead_usr_nodes ((void *)&env, false_bdd, false_bdd_entry);
        copy_live_free_dead_usr_nodes ((void *)&env, true_bdd, true_bdd_entry);
    }

    usr_bdd_ht_map_entries (mgr->usr_bdd_map,
                            (void *)&env,
                            copy_live_free_dead_usr_nodes);
    bdd_mgr_deinitialize_partial (mgr);
    *mgr = dst_mgr;
    env.stats.raw_bdd_num_collected = old_num_nodes - env.stats.raw_bdd_num_copied;
    bdd_gc_stats_fprint (stderr, env.stats);
}
