/* BDD garbage collection code. */

#include "bdd_impl.h"
#include "bdd_ht.h"

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

/* Copies the BDD subgraph with the given root from the old BDD heap
 * space into the new BDD heap space, returning the new index.  The
 * entry for 'old_root' in the old heap is modified during this copy
 * to indicate that copying has already been done: if the 'var' field
 * of the node denoted by 'old_root' equals 'UINT_MAX', then the 'low'
 * field contains the value of the new index. */
static raw_bdd_t
copy_bdd_rec (
    bdd_gc_env_t *env,
    raw_bdd_t old_root
    )
{
    node_t old_root_node;
    raw_bdd_t new_low, new_high, new_root;
    old_root_node = node_vec_get (&env->mgr->old_nodes_by_idx, (unsigned) old_root);
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
        new_root = make_node (env->mgr, old_root_node.var, new_low, new_high);
        old_root_node.var = UINT_MAX;
        old_root_node.low = new_root;
        node_vec_set (&env->mgr->old_nodes_by_idx, old_root, old_root_node);
        assert (old_root != raw_bdd_false || old_root == new_root);
        assert (old_root != raw_bdd_true || old_root == new_root);
        return new_root;
    }
}

static void
gc_usr_bdd (void *env, bdd_t *key, usr_bdd_entry_t *val)
{
    bdd_gc_env_t *gc_env = (bdd_gc_env_t *)env;
    if (val->ref_cnt > 0) {
        usr_bdd_entry_t new_entry;
        gc_env->stats.usr_bdd_num_copied += 1;
        new_entry.ref_cnt = val->ref_cnt;
        new_entry.raw_bdd = copy_bdd_rec (gc_env, val->raw_bdd);
        usr_bdd_ht_insert (gc_env->mgr->usr_bdd_map, key, new_entry);
        bdd_rtu_ht_insert (gc_env->mgr->raw_bdd_map, new_entry.raw_bdd, key);
    }
    else {
        gc_env->stats.usr_bdd_num_collected += 1;
        checked_free (key);
    }
}

/* FIXME: this is a poorly named function */
static void
swap_heaps (bdd_mgr_t *mgr)
{
    node_vec_t tmp_vec;

    tmp_vec = mgr->old_nodes_by_idx;
    mgr->old_nodes_by_idx = mgr->nodes_by_idx;
    mgr->nodes_by_idx = tmp_vec;
    node_vec_clear (&mgr->nodes_by_idx);
    node_ht_clear (&mgr->idxs_by_node);

    assert (bdd_mgr_get_num_nodes (mgr) == 0);
    assert (node_vec_get_num_elems (&mgr->nodes_by_idx) == 0);
    assert (node_ht_get_num_entries (&mgr->idxs_by_node) == 0);
}

void
bdd_mgr_perform_gc (bdd_mgr_t *mgr)
{
    usr_bdd_ht_t *old_usr_bdd_map;
    unsigned old_num_nodes;
    bdd_gc_env_t env;
    bdd_t *false_bdd, *true_bdd; /* FIXME: hack! */
    usr_bdd_entry_t *false_bdd_entry, *true_bdd_entry; /* FIXME: hack! */

    /* FIXME: hack! */
    false_bdd = bdd_false (mgr);
    assert (false_bdd != NULL);
    true_bdd = bdd_true (mgr);
    assert (true_bdd != NULL);
    false_bdd_entry = usr_bdd_ht_lookup (mgr->usr_bdd_map, false_bdd);
    assert (false_bdd_entry != NULL);
    true_bdd_entry = usr_bdd_ht_lookup (mgr->usr_bdd_map, true_bdd);
    assert (true_bdd_entry != NULL);
    /* end hack */

    old_num_nodes = bdd_mgr_get_num_nodes (mgr);

    env.mgr = mgr;
    env.stats = make_bdd_gc_stats_t ();

    swap_heaps (mgr);
    /* FIXME: patch up the cache instead of clearing it */
    bdd_ite_cache_clear (&mgr->ite_cache);

    /* FIXME: use a hash table that supports deletion to avoid copying */
    old_usr_bdd_map = mgr->usr_bdd_map;
    mgr->usr_bdd_map = usr_bdd_ht_create ();
    bdd_rtu_ht_destroy (mgr->raw_bdd_map);
    mgr->raw_bdd_map = bdd_rtu_ht_create ();
    mgr->num_unreferenced_bdds = 0;

    /* FIXME: hack! */
    /* Without this, sometimes the terminal nodes are flipped during
     * collection, which silently gives incorrect results. */
    gc_usr_bdd ((void *)&env, false_bdd, false_bdd_entry);
    gc_usr_bdd ((void *)&env, true_bdd, true_bdd_entry);
    /* end hack */

    usr_bdd_ht_map_entries (old_usr_bdd_map, (void *)&env, gc_usr_bdd);
    usr_bdd_ht_destroy (old_usr_bdd_map);

    env.stats.raw_bdd_num_collected = old_num_nodes - env.stats.raw_bdd_num_copied;
    bdd_gc_stats_fprint (stderr, env.stats);
}
