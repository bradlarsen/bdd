#include "bdd_mgr.h"

/* Doubles the size of the storage allocated for nodes. */
void
_bdd_mgr_double_capacity (bdd_mgr_t *mgr);

static void
_initialize_nodes (node_t *nodes, unsigned start, unsigned stop)
{
    unsigned i;
    for (i = start; i < stop; i += 1)
        nodes[i].ref_cnt = 0;
}

/* Adds a node with the given fields at the given index of the nodes array. */
static void
add_terminal_node (
    bdd_mgr_t *mgr,
    unsigned idx,
    unsigned lvl,
    bdd_t low,
    bdd_t high
    )
{
    mgr->nodes[idx].ref_cnt = _BDD_MAX_REF_CNT;
    mgr->nodes[idx].lvl = lvl;
    mgr->nodes[idx].low = low;
    mgr->nodes[idx].high = high;
    mgr->num_nodes += 1;
    _node_ht_insert (mgr, idx);
    mgr->last_used_alloc_idx = idx;
}

static bdd_cache_stats_t
make_cache_stats ()
{
    bdd_cache_stats_t res;
    res.num_lookups = 0;
    res.num_hits = 0;
    res.num_inserts = 0;
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

static inline boolean
node_on_hash_chain (
    bdd_mgr_t *mgr,
    unsigned lvl,
    bdd_t low,
    bdd_t high,
    unsigned bucket_idx,
    unsigned *node_idx
    )
{
    unsigned idx = mgr->nodes_hash[bucket_idx];
    while (idx != 0) {
        node_t n = mgr->nodes[idx - 1];
        if (n.lvl == lvl && n.low == low && n.high == high) {
            *node_idx = idx - 1;
            return 1;
        }
        idx = mgr->nodes[idx - 1].hash_next;
    }
    return 0;
}

void
_node_ht_bucket_dump (bdd_mgr_t *mgr, char *prefix, unsigned bucket_idx)
{
    unsigned idx = mgr->nodes_hash[bucket_idx];
    fprintf (stderr, "    %s hash chain at bucket %u:  ",
             prefix, bucket_idx);
    while (idx != 0) {
        fprintf (stderr, "%u -> ", idx - 1);
        idx = mgr->nodes[idx - 1].hash_next;
    }
    fprintf (stderr, "NULL\n");
}

void
_node_ht_bucket_delete (bdd_mgr_t *mgr, unsigned node_idx, unsigned bucket_idx)
{
    unsigned prev = 0;  /* 0 is the sentinel value for the table */
    unsigned cur = mgr->nodes_hash[bucket_idx];
    assert (mgr->num_nodes > 0);
    assert (cur != 0);
    while (cur - 1 != node_idx) {
        prev = cur;
        cur = mgr->nodes[cur - 1].hash_next;
        assert (cur != 0);
    }
    if (prev != 0)
        mgr->nodes[prev - 1].hash_next = mgr->nodes[cur - 1].hash_next;
    else
        mgr->nodes_hash[bucket_idx] = mgr->nodes[cur - 1].hash_next;
}

static void
_lvl_list_insert (bdd_mgr_t *mgr, bdd_t node_idx)
{
    node_t *n = &mgr->nodes[node_idx];
    n->lvl_prev = 0;
    n->lvl_next = mgr->lvl_chain_roots[n->lvl];
    if (mgr->lvl_chain_roots[n->lvl] != 0)
        mgr->nodes[mgr->lvl_chain_roots[n->lvl] - 1].lvl_prev = node_idx + 1;
    mgr->lvl_chain_roots[n->lvl] = node_idx + 1;
    mgr->nodes_at_level[n->lvl] += 1;
}

static void
_lvl_list_delete (bdd_mgr_t *mgr, bdd_t node_idx)
{
    node_t *n = &mgr->nodes[node_idx];
    if (n->lvl_prev != 0)
        mgr->nodes[n->lvl_prev - 1].lvl_next = n->lvl_next;
    else
        mgr->lvl_chain_roots[n->lvl] = n->lvl_next;
    if (n->lvl_next != 0)
        mgr->nodes[n->lvl_next - 1].lvl_prev = n->lvl_prev;
    assert (mgr->nodes_at_level[n->lvl] > 0);
    mgr->nodes_at_level[n->lvl] -= 1;
}

void
_bdd_dec_ref_rec (bdd_mgr_t *mgr, bdd_t b)
{
    assert (b < mgr->capacity);
    assert (mgr->nodes[b].ref_cnt > 0);
    assert (b > 1 || mgr->nodes[b].ref_cnt > 1);
    assert (mgr->nodes[mgr->nodes[b].low].ref_cnt > 0);
    assert (mgr->nodes[mgr->nodes[b].high].ref_cnt > 0);
    _bdd_dec_ref (mgr, b);
    if (mgr->nodes[b].ref_cnt == 0) {
        /* fprintf (stderr, "!!! deleting bdd at index %u (%u %u %u)\n", */
        /*          b, mgr->nodes[b].lvl, mgr->nodes[b].low, mgr->nodes[b].high); */
        assert (b > 1);
        assert (mgr->num_nodes > 0);
        _node_ht_delete (mgr, b);
        _lvl_list_delete (mgr, b);
        mgr->num_nodes -= 1;
        _bdd_dec_ref_rec (mgr, mgr->nodes[b].low);
        _bdd_dec_ref_rec (mgr, mgr->nodes[b].high);
    }
}

static unsigned
_bdd_mgr_get_free_index (bdd_mgr_t *mgr)
{
    unsigned mask = mgr->capacity - 1;
    unsigned i = (mgr->last_used_alloc_idx + 1) & mask;
    assert (mgr->num_nodes < mgr->capacity - 1);
    while (!node_is_empty (mgr->nodes[i]))
        i = (i + 1) & mask;
    mgr->last_used_alloc_idx = i;
    return i;
}

bdd_t
_bdd_make_node (
    bdd_mgr_t *mgr,
    unsigned lvl,
    bdd_t low,
    bdd_t high
    )
{
    assert (lvl < mgr->num_vars);
    assert (low < mgr->capacity);
    assert (high < mgr->capacity);
    assert (mgr->nodes[low].lvl > lvl);
    assert (mgr->nodes[high].lvl > lvl);
    assert (mgr->nodes[low].ref_cnt > 0);
    assert (mgr->nodes[high].ref_cnt > 0);

    if (low == high)
        return low;
    else {
        unsigned node_idx;
        unsigned bucket_idx = _node_hash (mgr, lvl, low, high);
        if (node_on_hash_chain (mgr, lvl, low, high, bucket_idx, &node_idx))
            return node_idx;    /* existing node found */

        if (mgr->num_nodes == mgr->capacity - 1) {
            _bdd_mgr_double_capacity (mgr);
            /* need to recompute bucket since size is changed */
            bucket_idx = _node_hash (mgr, lvl, low, high);
        }

        /* create a new node */
        node_idx = _bdd_mgr_get_free_index (mgr);
        assert (node_idx != low);
        assert (node_idx != high);
        mgr->num_nodes += 1;
        mgr->nodes[node_idx].lvl = lvl;
        _bdd_inc_ref (mgr, low);
        mgr->nodes[node_idx].low = low;
        _bdd_inc_ref (mgr, high);
        mgr->nodes[node_idx].high = high;
        _node_ht_bucket_insert (mgr, node_idx, bucket_idx);
        _lvl_list_insert (mgr, node_idx);
        return node_idx;
    }
}

static void
_bdd_mgr_rehash (bdd_mgr_t *mgr)
{
    unsigned i;
    memset (mgr->nodes_hash, 0,
            _bdd_mgr_num_hash_buckets (mgr) * sizeof (unsigned));
    for (i = 0; i < mgr->capacity; i += 1)
        if (node_is_live (mgr->nodes[i]))
            _node_ht_insert (mgr, i);
}

void
_bdd_mgr_double_capacity (bdd_mgr_t *mgr)
{
    unsigned old_capacity = mgr->capacity;
    fprintf (stderr, "!!! begin double\n");
    mgr->capacity *= 2;
    mgr->nodes = (node_t *)
        checked_realloc (mgr->nodes, mgr->capacity * sizeof (node_t));
    _initialize_nodes (mgr->nodes, old_capacity, mgr->capacity);
    mgr->nodes_hash = (unsigned *)
        checked_realloc (mgr->nodes_hash,
                         _bdd_mgr_num_hash_buckets (mgr) * sizeof (unsigned));
    _bdd_mgr_rehash (mgr);
    fprintf (stderr, "!!! end double\n");
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

static void
create_nodes_hash_table (bdd_mgr_t *mgr)
{
    mgr->nodes_hash = (unsigned *)
        checked_calloc (_bdd_mgr_num_hash_buckets (mgr), sizeof(unsigned));
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
    /* fprintf (stderr, "!!! creating a manager at %p with %u vars and %u capacity hint\n",
             (void *) mgr, num_vars, capacity_hint);
    */
    assert (num_vars > 0);
    assert (num_vars <= _BDD_MAX_NUM_VARS);
    mgr->num_vars = num_vars;

    mgr->capacity = size_hint_to_size (capacity_hint);
    mgr->num_nodes = 0;
    mgr->nodes = (node_t *) checked_malloc (mgr->capacity * sizeof(node_t));
    _initialize_nodes (mgr->nodes, 0, mgr->capacity);

    create_nodes_hash_table (mgr);
    create_lvl_var_mapping (mgr);

    mgr->lvl_chain_roots = (unsigned *)
        checked_calloc (num_vars, sizeof(unsigned));
    mgr->nodes_at_level = (unsigned *)
        checked_calloc (num_vars, sizeof(unsigned));

    /* FIXME: use a more reasonable cache size */
    bdd_ite_cache_create_with_hint (&mgr->ite_cache, 1024 * 32);
    mgr->ite_cache_stats = make_cache_stats ();

    mgr->last_used_alloc_idx = 0;

    add_terminal_node (mgr, 0, num_vars, 1, 0); /* false terminal */
    add_terminal_node (mgr, 1, num_vars, 0, 1); /* true terminal */

    _bdd_mgr_check_invariants (mgr);
    return mgr;
}

void
bdd_mgr_destroy (bdd_mgr_t *mgr)
{
/*
    fprintf (stderr, "!!! destroying manager at %p\n", (void *) mgr);
*/

    if (mgr == NULL) return;

    _bdd_mgr_check_invariants (mgr);

    bdd_ite_cache_destroy (&mgr->ite_cache);
    checked_free (mgr->nodes_hash);
    checked_free (mgr->nodes);
    checked_free (mgr->nodes_at_level);
    checked_free (mgr->lvl_chain_roots);
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
bdd_mgr_get_num_nodes_at_level (bdd_mgr_t *mgr, unsigned lvl)
{
    assert (lvl < mgr->num_vars);
    return mgr->nodes_at_level[lvl];
}

unsigned
bdd_mgr_get_num_allocated (bdd_mgr_t *mgr)
{
    return mgr->capacity;
}

void
bdd_cache_stats_fprint (FILE *handle, bdd_cache_stats_t stats)
{
    float hit_p;
    if (stats.num_lookups == 0)
        hit_p = 0.0f;
    else
        hit_p = (float) stats.num_hits / (float) stats.num_lookups * 100.0f;

    fprintf (handle, "%u/%u hits (%.1f%%)",
             stats.num_hits, stats.num_lookups, hit_p);
}

bdd_cache_stats_t
bdd_mgr_get_cache_stats (bdd_mgr_t *mgr)
{
    return mgr->ite_cache_stats;
}
