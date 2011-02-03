#include "bdd_mgr.h"

static node_t *
create_node_array (unsigned capacity)
{
    unsigned i;
    node_t *nodes;
    nodes = (node_t *) checked_malloc (capacity * sizeof(node_t));
    for (i = 0; i < capacity; i += 1) {
        set_node_empty(&nodes[i]);
        nodes[i].hash_next = 0;
        nodes[i].level_next = 0;
    }
    return nodes;
}

static void
add_false_node (bdd_mgr_t *mgr)
{
    mgr->nodes[0].lvl = mgr->num_vars;
    mgr->nodes[0].low = 1;
    mgr->nodes[0].high = 0;
    mgr->nodes[0].hash_next = 0;
    mgr->nodes[0].level_next = 0;
    mgr->num_nodes += 1;
}

static void
add_true_node (bdd_mgr_t *mgr)
{
    mgr->nodes[1].lvl = mgr->num_vars;
    mgr->nodes[1].low = 0;
    mgr->nodes[1].high = 1;
    mgr->nodes[1].hash_next = 0;
    mgr->nodes[1].level_next = 0;
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

/* Converts the given size hint into the smallest valid size greater
 * than or equal to the hint.  A valid size is a power of two greater
 * than or equal to 128. */
static unsigned
size_hint_to_size (unsigned hint)
{
    unsigned i;
    for (i = 1; i < hint; i *= 2) {}
    return 128u > i ? 128u : i;
}

void
_bdd_mgr_resize (bdd_mgr_t *mgr, unsigned new_capacity_hint)
{
    assert (0);
}

static inline unsigned
node_hash (unsigned lvl, bdd_t low, bdd_t high)
{
    return hash_unsigned_pair (lvl, hash_unsigned_pair(low, high));
}

/* Walks down the nodes array from the start index until an empty
 * location is found. */
static bdd_t
linear_probe_to_empty_node (bdd_mgr_t *mgr, unsigned start)
{
    unsigned i = start;
    while (!node_is_empty (mgr->nodes[i]))
        i = (i + 1) & (mgr->capacity - 1);
    return i;
}

/* Returns the index for a node with the given components on the hash
 * chain rooted at 'start', or else returns 0 if there is no such
 * node. */
static bdd_t
find_node_on_hash_chain (
    bdd_mgr_t *mgr,
    unsigned lvl,
    bdd_t low,
    bdd_t high,
    unsigned start
    )
{
    unsigned idx = start;
    while (idx != 0) {
        node_t n = mgr->nodes[idx];
        if (n.lvl == lvl && n.low == low && n.high == high)
            break;
        else
            idx = n.hash_next;
    }
    return idx;
}

static void
append_hash_chains (bdd_mgr_t *mgr, unsigned x, unsigned y)
{
    if (x == y)
        return;

    while (mgr->nodes[x].hash_next != 0)
        x = mgr->nodes[x].hash_next;
    mgr->nodes[x].hash_next = y;
}

bdd_t
_bdd_make_node (
    bdd_mgr_t *mgr,
    unsigned lvl,
    bdd_t low,
    bdd_t high
    )
{
    if (low == high)
        return low;
    else {
        /* try to find existing node */
        unsigned hash_val, node_idx;
        hash_val = node_hash (lvl, low, high) & (mgr->capacity - 1);
        node_idx = find_node_on_hash_chain (mgr, lvl, low, high, hash_val);
        if (node_idx != 0) {
            node_t n = mgr->nodes[node_idx];
            assert (!node_is_empty (n));
            assert (n.lvl == lvl && n.low == low && n.high == high);
            return node_idx;
        }

        if (mgr->num_nodes == mgr->capacity - 1)
            /* ensure there are enough nodes */
            longjmp (mgr->out_of_nodes_cxt, 1);

        /* create a new node */
        node_idx = linear_probe_to_empty_node (mgr, hash_val);
        fprintf (stderr, "!!! putting (%u, %u, %u) at %u\n",
                 lvl, low, high, node_idx);
        mgr->nodes[node_idx].lvl = lvl;
        mgr->nodes[node_idx].low = low;
        mgr->nodes[node_idx].high = high;
        append_hash_chains (mgr, hash_val, node_idx);
        mgr->num_nodes += 1;
        return node_idx;
    }
}

static void
create_lvl_var_mapping (bdd_mgr_t *mgr)
{
    unsigned i;
    mgr->lvl_to_var = (unsigned *)
        checked_malloc (mgr->num_vars * sizeof(unsigned));
    mgr->var_to_lvl = (unsigned *)
        checked_malloc (mgr->num_vars * sizeof(unsigned));
    for (i = 0; i < mgr->num_vars; i += 1) {
        mgr->lvl_to_var[i] = i;
        mgr->var_to_lvl[i] = i;
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

    assert (num_vars > 0);
    mgr->num_vars = num_vars;

    mgr->capacity = size_hint_to_size (capacity_hint);
    mgr->num_nodes = 0;
    mgr->nodes = create_node_array (mgr->capacity);

    create_lvl_var_mapping (mgr);

    /* FIXME: use a more reasonable cache size */
    bdd_ite_cache_create_with_hint (&mgr->ite_cache, 1024 * 32);
    mgr->ite_cache_stats = make_cache_stats ();

    add_false_node (mgr);
    add_true_node (mgr);

    _bdd_mgr_check_invariants (mgr);

    return mgr;
}

void
bdd_mgr_destroy (bdd_mgr_t *mgr)
{
    if (mgr == NULL) return;

    _bdd_mgr_check_invariants (mgr);

    bdd_ite_cache_destroy (&mgr->ite_cache);

    checked_free (mgr->nodes);
    checked_free (mgr->lvl_to_var);
    checked_free (mgr->var_to_lvl);
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
