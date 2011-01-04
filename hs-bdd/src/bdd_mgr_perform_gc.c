#include "bdd_impl.h"
#include "bdd_ht.h"

/* mapping function for 'num_live_usr_nodes' */
static void
count_live_usr_nodes (void *env, bdd_t *key, usr_bdd_entry_t *val)
{
    (void) key;
    if (val->ref_cnt > 0)
        *((unsigned *)env) += 1;
}

/* count the number of live user nodes */
static unsigned
num_live_usr_nodes (bdd_mgr_t *mgr)
{
    unsigned num = 0;
    usr_bdd_ht_map_entries (mgr->usr_bdd_map, (void *)&num,
                            count_live_usr_nodes);
    return num;
}

/* count the number of user nodes */
static unsigned
num_usr_nodes (bdd_mgr_t *mgr)
{
    return usr_bdd_ht_get_num_entries (mgr->usr_bdd_map);
}

/*** LIVE NODE ACCUMULATION ***/
typedef struct
{
    bdd_ht_t *visited;
    bdd_mgr_t *mgr;
} visit_formula_pair_t;

static void
visit_formula_rec (bdd_mgr_t *mgr, bdd_ht_t *visited, raw_bdd_t b)
{
    raw_bdd_t *entry;
    entry = bdd_ht_lookup (visited, b);
    if (entry == NULL) {
        node_t b_node = raw_bdd_to_node (mgr, b);
        bdd_ht_insert (visited, b, b);
        visit_formula_rec (mgr, visited, b_node.low);
        visit_formula_rec (mgr, visited, b_node.high);
    }
}

static void
visit_formula (void *env, bdd_t *key, usr_bdd_entry_t *val)
{
    visit_formula_pair_t *pair = (visit_formula_pair_t *)env;
    (void) key;
    if (val->ref_cnt > 0)
        visit_formula_rec (pair->mgr, pair->visited, val->raw_bdd);
}

/* Count the number of live nodes. */
static unsigned
num_live_nodes (bdd_mgr_t *mgr)
{
    unsigned num_live;
    visit_formula_pair_t env;
    env.visited = bdd_ht_create ();
    env.mgr = mgr;
    usr_bdd_ht_map_entries (mgr->usr_bdd_map, (void *)&env, visit_formula);
    num_live = bdd_ht_get_num_entries (env.visited);
    bdd_ht_destroy (env.visited);

    return num_live;
}

/*** BDD COPYING CODE ***/
typedef struct
{
    bdd_mgr_t *src_mgr;         /* source */
    bdd_mgr_t *dst_mgr;         /* destination */
    bdd_ht_t *copied_map;       /* keeps track of which nodes have
                                 * been copied */
} bdd_gc_env_t;

/* Copies the BDD subgraph with the given root from the source to the
 * destination manager, returning the new root. */
static raw_bdd_t
copy_bdd_rec (
    bdd_mgr_t *src_mgr,
    bdd_mgr_t *dst_mgr,
    bdd_ht_t *copied_map,
    raw_bdd_t old_root
    )
{
    raw_bdd_t *existing_bdd;
    existing_bdd = bdd_ht_lookup (copied_map, old_root);
    if (existing_bdd != NULL)
        return *existing_bdd;
    else {
        raw_bdd_t new_low, new_high, new_root;
        node_t old_root_node;
        old_root_node = raw_bdd_to_node (src_mgr, old_root);
        if (old_root == raw_bdd_true || old_root == raw_bdd_false) {
            new_low = old_root_node.low;
            new_high = old_root_node.high;
        }
        else {
            new_low = copy_bdd_rec (src_mgr, dst_mgr, copied_map, old_root_node.low);
            new_high = copy_bdd_rec (src_mgr, dst_mgr, copied_map, old_root_node.high);
        }
        assert (new_low != new_high);
        new_root = make_node (dst_mgr, old_root_node.var, new_low, new_high);
        bdd_ht_insert (copied_map, old_root, new_root);
        return new_root;
    }
}

static void
copy_live_usr_nodes (void *env, bdd_t *key, usr_bdd_entry_t *val)
{
    bdd_gc_env_t *gc_env = (bdd_gc_env_t *)env;
    if (val->ref_cnt > 0) {
        usr_bdd_entry_t new_entry;
        new_entry.ref_cnt = val->ref_cnt;
        new_entry.raw_bdd = copy_bdd_rec (gc_env->src_mgr,
                                          gc_env->dst_mgr,
                                          gc_env->copied_map,
                                          val->raw_bdd);
        usr_bdd_ht_insert (gc_env->dst_mgr->usr_bdd_map, key, new_entry);
        bdd_rtu_ht_insert (gc_env->dst_mgr->raw_bdd_map,
                           new_entry.raw_bdd,
                           key);
    }
    else {
        checked_free (key);
    }
}

static void
fprint_gc_info (FILE *handle, bdd_mgr_t *mgr)
{
    double n_usr_nodes, n_nodes, nl_usr_nodes, nl_nodes;

    n_usr_nodes = num_usr_nodes (mgr);
    n_nodes = bdd_mgr_get_num_nodes (mgr);
    nl_usr_nodes = num_live_usr_nodes (mgr);
    nl_nodes = num_live_nodes (mgr);

    fprintf (handle, "    %.0f/%.0f live user nodes (%.2f%%)\n",
             nl_usr_nodes, n_usr_nodes, nl_usr_nodes / n_usr_nodes * 100);
    fprintf (handle, "    %.0f/%.0f live nodes (%.2f%%)\n",
             nl_nodes, n_nodes, nl_nodes / n_nodes * 100);
    fprintf (handle, "    %u allocated nodes\n",
             bdd_mgr_get_num_allocated (mgr));
}

void
bdd_mgr_perform_gc (bdd_mgr_t *mgr)
{
    bdd_gc_env_t env;
    bdd_mgr_t dst_mgr;

    fprintf (stderr, "********************************\n");
    fprintf (stderr, "bdd_mgr_perform_gc begin:\n");
    fprint_gc_info (stderr, mgr);

    /* FIXME: memory leaks lurk here, as true nodes and false nodes
     * never get freed. */
    bdd_mgr_initialize_with_hint (&dst_mgr,
                                  bdd_mgr_get_num_vars (mgr),
                                  bdd_mgr_get_num_allocated (mgr));
    dst_mgr.new_usr_id = mgr->new_usr_id;
    env.src_mgr = mgr;
    env.dst_mgr = &dst_mgr;
    env.copied_map = bdd_ht_create_with_hint (bdd_mgr_get_num_nodes (mgr));
    usr_bdd_ht_map_entries (mgr->usr_bdd_map,
                            (void *)&env,
                            copy_live_usr_nodes);
    bdd_mgr_deinitialize_partial (mgr);
    *mgr = dst_mgr;
    bdd_ht_destroy (env.copied_map);

    fprintf (stderr, "bdd_mgr_perform_gc end:\n");
    fprint_gc_info (stderr, mgr);
    fprintf (stderr, "********************************\n");
}
