#include "bdd_mgr.h"

static node_t *
create_node_array (unsigned capacity)
{
    unsigned i;
    node_t *nodes;
    nodes = (node_t *) checked_malloc (capacity * sizeof(node_t));
    for (i = 0; i < capacity; i += 1)
        nodes[i].var = INT_MAX;
    return nodes;
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
    for (i = 1; i < n; i *= 2) {}
    return i;
}

/* Copies the BDD subgraph with the given root from the old node array
 * into the manager, returning the new index.  This must not be called
 * on a terminal or empty node.
 * 
 * The node corresponding to 'old_root' in the old node array is
 * modified during this copy operation to memoize copying: if the
 * 'low' field of the node denoted by 'old_root' equals 'UINT_MAX',
 * the the 'high' field contains the value of the new index. */
static raw_bdd_t
copy_bdd_rec (bdd_mgr_t *mgr, node_t *old_nodes, raw_bdd_t old_root)
{
    node_t old_root_node;
    assert (!raw_bdd_is_terminal (old_root));
    old_root_node = old_nodes[old_root];
    assert (!node_is_empty (old_root_node));
    if (old_root_node.low == UINT_MAX)
        return old_root_node.high;
    else {
        raw_bdd_t new_low, new_high, new_root;
        new_low = raw_bdd_is_terminal (old_root_node.low)
            ? old_root_node.low
            : copy_bdd_rec (mgr, old_nodes, old_root_node.low);
        new_high = raw_bdd_is_terminal (old_root_node.high)
            ? old_root_node.high
            : copy_bdd_rec (mgr, old_nodes, old_root_node.high);
        assert (new_low != new_high);
        /* FIXME: don't use '_bdd_make_node' here */
        /* It could (but definitely shouldn't) result in recursive
         * resizing, and does more work than necessary. */
        new_root = _bdd_make_node (mgr, old_root_node.var, new_low, new_high);
        old_nodes[old_root].low = UINT_MAX;
        old_nodes[old_root].high = new_root;
        return new_root;
    }
}

static void
patch_bdd_indirection (
    bdd_mgr_t *mgr,
    bdd_rtu_ht_t *old_raw_bdd_map,
    usr_bdd_ht_t *old_usr_bdd_map,
    raw_bdd_t old_i,
    raw_bdd_t new_i
    )
{
    bdd_t **usr_bdd;
    assert (!node_is_empty (raw_bdd_to_node (mgr, new_i)));
    assert (!node_is_deleted (raw_bdd_to_node (mgr, new_i)));
    usr_bdd = bdd_rtu_ht_lookup (old_raw_bdd_map, old_i);
    if (usr_bdd != NULL) {
        usr_bdd_entry_t new_entry;
        new_entry = *usr_bdd_ht_lookup (old_usr_bdd_map, *usr_bdd);
        new_entry.raw_bdd = new_i;
        usr_bdd_ht_insert (mgr->usr_bdd_map, *usr_bdd, new_entry);
        bdd_rtu_ht_insert (mgr->raw_bdd_map, new_i, *usr_bdd);
    }
}

void
bdd_mgr_resize (bdd_mgr_t *mgr, unsigned new_capacity_hint)
{
    const unsigned old_capacity = mgr->capacity;
    bdd_rtu_ht_t *old_raw_bdd_map;
    usr_bdd_ht_t *old_usr_bdd_map;
#ifndef NDEBUG
    unsigned *old_hash_histo;
#endif
    node_t *old_nodes;
    unsigned i;

    mgr->capacity = up_to_next_power_of_two (new_capacity_hint);
    old_raw_bdd_map = mgr->raw_bdd_map;
    old_usr_bdd_map = mgr->usr_bdd_map;
    old_nodes = mgr->nodes;
    assert (0.75 * mgr->capacity >= mgr->num_nodes);

    mgr->nodes = create_node_array (mgr->capacity);
    mgr->num_nodes = 0;
    mgr->num_deleted_nodes = 0;
    mgr->usr_bdd_map = usr_bdd_ht_create ();
    mgr->raw_bdd_map = bdd_rtu_ht_create ();

#ifndef NDEBUG
    old_hash_histo = mgr->hash_histo;
    mgr->hash_histo = (unsigned *)
        checked_calloc (mgr->capacity, sizeof(unsigned));
    memcpy (mgr->hash_histo, old_hash_histo, old_capacity * sizeof(unsigned));
    checked_free (old_hash_histo);
#endif

    add_false_node (mgr);
    add_true_node (mgr);
    patch_bdd_indirection (mgr,
                           old_raw_bdd_map,
                           old_usr_bdd_map,
                           raw_bdd_false,
                           raw_bdd_false);
    patch_bdd_indirection (mgr,
                           old_raw_bdd_map,
                           old_usr_bdd_map,
                           raw_bdd_true,
                           raw_bdd_true);

    for (i = 2; i < old_capacity; i += 1)
        if (!node_is_empty(old_nodes[i]) && !node_is_deleted(old_nodes[i])) {
            raw_bdd_t new_i = copy_bdd_rec (mgr, old_nodes, i);
            patch_bdd_indirection (mgr,
                                   old_raw_bdd_map,
                                   old_usr_bdd_map,
                                   i,
                                   new_i);
        }
    assert (usr_bdd_ht_get_num_entries (old_usr_bdd_map) ==
            usr_bdd_ht_get_num_entries (mgr->usr_bdd_map));
    assert (bdd_rtu_ht_get_num_entries (old_raw_bdd_map) ==
            bdd_rtu_ht_get_num_entries (mgr->raw_bdd_map));

    usr_bdd_ht_destroy (old_usr_bdd_map);
    bdd_rtu_ht_destroy (old_raw_bdd_map);
    checked_free (old_nodes);
    bdd_ite_cache_clear (&mgr->ite_cache);
}

/* Interns the raw BDD index, mapping it to a new user-level BDD
 * index with a reference count of 0. */
static bdd_t *
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

bdd_t *
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

raw_bdd_t
usr_to_raw (bdd_mgr_t *mgr, bdd_t *usr)
{
    usr_bdd_entry_t *res = usr_bdd_ht_lookup (mgr->usr_bdd_map, usr);
    assert (res != NULL);
    assert (raw_bdd_is_valid_and_live (mgr, res->raw_bdd));
    return res->raw_bdd;
}

static void
_bdd_raise_out_of_nodes (bdd_mgr_t *mgr)
{
    longjmp (mgr->out_of_nodes_cxt, 1);
}

static inline unsigned
node_hash (unsigned var, raw_bdd_t low, raw_bdd_t high)
{
    return hash_unsigned_pair (var, hash_unsigned_pair(low, high));
}

raw_bdd_t
_bdd_make_node (
    bdd_mgr_t *mgr,
    int var,
    raw_bdd_t low,
    raw_bdd_t high
    )
{
    if (low == high)
        return low;
    else {
        /* try to find existing node */
        unsigned loop_count;
        int free_idx;
        unsigned idx;
        free_idx = -1;
        idx = node_hash (var, low, high) & (mgr->capacity - 1);
        loop_count = 0;
        while (!node_is_empty (mgr->nodes[idx])) {
            node_t n = mgr->nodes[idx];
            loop_count += 1;
            if (free_idx == -1 && node_is_deleted (mgr->nodes[idx]))
                free_idx = idx;
            if (n.var == var && n.low == low && n.high == high) {
#ifndef NDEBUG
                mgr->hash_histo[loop_count] += 1;
#endif
                return idx;
            }
            idx = (idx + 1) & (mgr->capacity - 1);
        }
#ifndef NDEBUG
        mgr->hash_histo[loop_count] += 1;
#endif

        /* try to get more space */
        if (mgr->num_nodes >= 0.75 * mgr->capacity)
            _bdd_raise_out_of_nodes (mgr);
        else if (free_idx != -1) {
            /* reuse a deleted node */
            mgr->num_deleted_nodes -= 1;
            idx = free_idx;
        }

        assert (mgr->num_nodes < 0.75 * mgr->capacity);
        mgr->nodes[idx].var = var;
        mgr->nodes[idx].low = low;
        mgr->nodes[idx].high = high;
        mgr->num_nodes += 1;
        return idx;
    }
}

bdd_mgr_t *
bdd_mgr_create (unsigned num_vars)
{
    return bdd_mgr_create_with_hint (num_vars, 1024);
}

bdd_mgr_t *
bdd_mgr_create_with_hint (unsigned num_vars, unsigned capacity_hint)
{
    bdd_mgr_t *mgr = (bdd_mgr_t *) checked_malloc (sizeof(bdd_mgr_t));

    mgr->num_vars = num_vars;

    mgr->capacity = up_to_next_power_of_two (capacity_hint + 32);
    mgr->num_nodes = 0;
    mgr->num_deleted_nodes = 0;
    mgr->nodes = create_node_array (mgr->capacity);

#ifndef NDEBUG
    mgr->hash_histo = (unsigned *)
        checked_calloc (mgr->capacity, sizeof(unsigned));
#endif

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

#ifndef NDEBUG
static void
fprint_hash_histo (FILE *handle, bdd_mgr_t *mgr)
{
    unsigned i, j;
    unsigned total;

    fprintf (handle, "histogram of linear probe counts:\n");

    total = 0;
    for (i = 0; i < mgr->capacity; i += 1)
        total += mgr->hash_histo[i];

    i = 0;
    while (i < mgr->capacity) {
        int num_printed;
        unsigned low_i;
        unsigned num_in_bucket;
        float frac_in_bucket;
        low_i = i;
        num_in_bucket = 0;
        frac_in_bucket = 0.0f;
        while (frac_in_bucket < 0.075f && i < mgr->capacity) {
            frac_in_bucket += (float)mgr->hash_histo[i] / (float)total;
            num_in_bucket += mgr->hash_histo[i];
            i += 1;
        }

        if (low_i == i)
            num_printed = fprintf (handle, "  %u:", i);
        else
            num_printed = fprintf (handle, "  %u--%u:", low_i, i);

        for (j = 0; j < (unsigned)(22 - num_printed); j += 1)
            fputc (' ', handle);

        fprintf (handle, "%10u (%4.1f%%)  ",
                 num_in_bucket, frac_in_bucket * 100.0f);

        for (j = 0; j < (unsigned)(frac_in_bucket * 100.0f); j += 3)
            fputc ('*', handle);
        fputc ('\n', handle);
    }
}
#endif

void
bdd_mgr_destroy (bdd_mgr_t *mgr)
{
    if (mgr == NULL) return;

    bdd_rtu_ht_map_entries (mgr->raw_bdd_map, NULL, free_usr_bdds);
    bdd_ite_cache_destroy (&mgr->ite_cache);

    bdd_rtu_ht_destroy (mgr->raw_bdd_map);
    usr_bdd_ht_destroy (mgr->usr_bdd_map);

#ifndef NDEBUG
    fprint_hash_histo (stderr, mgr);
    checked_free (mgr->hash_histo);
#endif
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

bdd_cache_stats_t
bdd_mgr_get_cache_stats (bdd_mgr_t *mgr)
{
    return mgr->ite_cache_stats;
}
