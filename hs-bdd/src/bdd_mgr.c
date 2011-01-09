#include "bdd_impl.h"

bdd_t *
bdd_false (bdd_mgr_t *mgr)
{
    return raw_to_usr (mgr, raw_bdd_false);
}

bdd_t *
bdd_true (bdd_mgr_t *mgr)
{
    return raw_to_usr (mgr, raw_bdd_true);
}

bdd_mgr_t *
bdd_mgr_create (unsigned num_vars)
{
    return bdd_mgr_create_with_hint (num_vars, 1024);
}

static void
add_false_node (bdd_mgr_t *mgr)
{
    raw_bdd_t usr_false;
    usr_false = make_node (mgr, mgr->num_vars, raw_bdd_true, raw_bdd_false);
    bdd_inc_ref (mgr, intern_raw_bdd (mgr, usr_false));
}

static void
add_true_node (bdd_mgr_t *mgr)
{
    raw_bdd_t usr_true;
    usr_true = make_node (mgr, mgr->num_vars, raw_bdd_false, raw_bdd_true);
    bdd_inc_ref (mgr, intern_raw_bdd (mgr, usr_true));
}

static bdd_cache_stats_t
make_cache_stats ()
{
    bdd_cache_stats_t res;
    res.num_lookups = 0;
    res.num_hits = 0;
    res.num_inserts = 0;
    res.num_replacements = 0;
    return res;
}

bdd_mgr_t *
bdd_mgr_create_with_hint (unsigned num_vars, unsigned capacity_hint)
{
    bdd_mgr_t *mgr = (bdd_mgr_t *) checked_malloc (sizeof(bdd_mgr_t));

    mgr->num_vars = num_vars;

    node_vec_create_with_capacity (&mgr->nodes_by_idx, capacity_hint);
    node_ht_create_with_hint (&mgr->idxs_by_node, capacity_hint);

    mgr->usr_bdd_map = usr_bdd_ht_create ();
    mgr->raw_bdd_map = bdd_rtu_ht_create ();

    mgr->new_usr_id = 0;

    mgr->num_unreferenced_bdds = 0;
    node_vec_create_with_capacity (&mgr->old_nodes_by_idx, capacity_hint);

    /* FIXME: use a more reasonable cache size */
    bdd_ite_cache_create_with_hint (&mgr->ite_cache, 1024 * 32);
    mgr->ite_cache_stats = make_cache_stats ();

    add_false_node (mgr);
    add_true_node (mgr);

    return mgr;
}

/* Mapping function for a 'usr_bdd_ht_t' that frees the bdd_t structs. */
static void
free_usr_bdds (void *env, raw_bdd_t raw, bdd_t **usr)
{
    (void) env;
    (void) raw;
    checked_free (*usr);
}

void
bdd_mgr_destroy (bdd_mgr_t *mgr)
{
    if (mgr == NULL) return;
    bdd_rtu_ht_map_entries (mgr->raw_bdd_map, NULL, free_usr_bdds);
    bdd_ite_cache_destroy (&mgr->ite_cache);

    bdd_rtu_ht_destroy (mgr->raw_bdd_map);
    usr_bdd_ht_destroy (mgr->usr_bdd_map);
    
    node_ht_destroy (&mgr->idxs_by_node);
    node_vec_destroy (&mgr->nodes_by_idx);
    checked_free (mgr);
}

unsigned
bdd_mgr_get_num_vars (bdd_mgr_t *mgr)
{
    return mgr->num_vars;
}

unsigned
bdd_mgr_get_num_nodes (bdd_mgr_t *mgr)
{
    return node_vec_get_num_elems(&mgr->nodes_by_idx);
}

unsigned
bdd_mgr_get_num_allocated (bdd_mgr_t *mgr)
{
    return node_vec_get_capacity (&mgr->nodes_by_idx);
}

bdd_cache_stats_t
bdd_mgr_get_cache_stats (bdd_mgr_t *mgr)
{
    return mgr->ite_cache_stats;
}

void
bdd_cache_stats_fprint (FILE *handle, bdd_cache_stats_t stats)
{
    float hit_p, repl_p;
    if (stats.num_lookups == 0)
        hit_p = 0.0f;
    else
        hit_p = (float) stats.num_hits / (float) stats.num_lookups * 100.0f;

    if (stats.num_inserts == 0)
        repl_p = 0.0f;
    else
        repl_p = (float) stats.num_replacements / (float) stats.num_inserts
            * 100.0f;

    fprintf (handle, "%u/%u hits (%.0f%%), %u/%u replacements (%.0f%%)",
             stats.num_hits, stats.num_lookups, hit_p,
             stats.num_replacements, stats.num_inserts, repl_p);
}
