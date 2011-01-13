#include "bdd_mgr.h"

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
    mgr->nodes[0].var = mgr->num_vars;
    mgr->nodes[0].low = 1;
    mgr->nodes[0].high = 0;
    mgr->num_nodes += 1;
}

static void
add_true_node (bdd_mgr_t *mgr)
{
    mgr->nodes[1].var = mgr->num_vars;
    mgr->nodes[1].low = 0;
    mgr->nodes[1].high = 1;
    mgr->num_nodes += 1;
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

static unsigned
up_to_next_power_of_two (unsigned n)
{
    unsigned i;
    for (i = 1; i <= n; i *= 2) {}
    return i;
}

bdd_mgr_t *
bdd_mgr_create_with_hint (unsigned num_vars, unsigned capacity_hint)
{
    unsigned i;
    bdd_mgr_t *mgr = (bdd_mgr_t *) checked_malloc (sizeof(bdd_mgr_t));

    mgr->num_vars = num_vars;

    mgr->capacity = up_to_next_power_of_two (capacity_hint);
    mgr->num_nodes = 0;
    mgr->nodes = (node_t *) checked_malloc (mgr->capacity * sizeof(node_t));
    for (i = 0; i < mgr->capacity; i += 1)
        mgr->nodes[i].var = UINT_MAX;

    mgr->hash_histo = (unsigned *) checked_calloc (mgr->capacity, sizeof(unsigned));

    mgr->usr_bdd_map = usr_bdd_ht_create ();
    mgr->raw_bdd_map = bdd_rtu_ht_create ();

    mgr->new_usr_id = 0;

    mgr->num_unreferenced_bdds = 0;
    /* FIXME: use a more reasonable initial gc hint value */
    mgr->next_gc_at_node_count = mgr->capacity * 0.5;

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

static void
fprint_hash_histo (FILE *handle, bdd_mgr_t *mgr)
{
    unsigned i;
    unsigned total;

    total = 0;
    for (i = 0; i < mgr->capacity; i += 1)
        total += mgr->hash_histo[i];

    fprintf (handle, "histogram of linear probe counts:\n");
    for (i = 0; i < mgr->capacity; i += 1)
        if (mgr->hash_histo[i] > 0)
            fprintf (handle, "  %u: %u (%.1f%%)\n", i, mgr->hash_histo[i],
                     (float)mgr->hash_histo[i] / (float)total * 100.0f);
}

void
bdd_mgr_destroy (bdd_mgr_t *mgr)
{
    if (mgr == NULL) return;

    bdd_rtu_ht_map_entries (mgr->raw_bdd_map, NULL, free_usr_bdds);
    bdd_ite_cache_destroy (&mgr->ite_cache);

    bdd_rtu_ht_destroy (mgr->raw_bdd_map);
    usr_bdd_ht_destroy (mgr->usr_bdd_map);

    fprint_hash_histo (stderr, mgr);
    checked_free (mgr->hash_histo);
    checked_free (mgr->nodes);
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
    return mgr->num_nodes;
}

unsigned
bdd_mgr_get_num_allocated (bdd_mgr_t *mgr)
{
    return mgr->capacity;
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
