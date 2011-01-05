#include "bdd_impl.h"
#include "bdd_ht.h"

typedef struct
{
    bdd_mgr_t *src_mgr;         /* source */
    bdd_mgr_t *dst_mgr;         /* destination */
    bdd_ht_t *copied_map;       /* keeps track of which nodes have
                                 * been copied */
    bdd_gc_stats_t stats;       /* collection statistics */
} bdd_gc_env_t;

/* Copies the BDD subgraph with the given root from the source to the
 * destination manager, returning the new root. */
static raw_bdd_t
copy_bdd_rec (
    bdd_gc_env_t *env,
    raw_bdd_t old_root
    )
{
    raw_bdd_t *existing_bdd;

    existing_bdd = bdd_ht_lookup (env->copied_map, old_root);
    if (existing_bdd != NULL)
        return *existing_bdd;
    else {
        raw_bdd_t new_low, new_high, new_root;
        node_t old_root_node;
        env->stats.raw_bdd_num_copied += 1;
        old_root_node = raw_bdd_to_node (env->src_mgr, old_root);
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
        bdd_ht_insert (env->copied_map, old_root, new_root);
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
    env.src_mgr = mgr;
    env.dst_mgr = &dst_mgr;
    env.copied_map = bdd_ht_create_with_hint (old_num_nodes);
    env.stats = mk_bdd_gc_stats_t ();
    usr_bdd_ht_map_entries (mgr->usr_bdd_map,
                            (void *)&env,
                            copy_live_free_dead_usr_nodes);
    bdd_mgr_deinitialize_partial (mgr);
    *mgr = dst_mgr;
    bdd_ht_destroy (env.copied_map);
    env.stats.raw_bdd_num_collected = old_num_nodes - env.stats.raw_bdd_num_copied;
    bdd_gc_stats_fprint (stderr, env.stats);
}
