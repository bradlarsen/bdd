#include "bdd_impl.h"
#include "bdd_ht.h"

static void
count_live_formulae (void *env, bdd_t *key, usr_bdd_entry_t val)
{
    (void) key;
    if (val.ref_cnt > 0)
        *((unsigned *)env) += 1;
}

static unsigned
num_live_formulae (bdd_mgr_t *mgr)
{
    unsigned num = 0;
    usr_bdd_ht_map_entries (mgr->usr_bdd_map, (void *)&num,
                            count_live_formulae);
    return num;
}

static unsigned
num_formulae (bdd_mgr_t *mgr)
{
    return usr_bdd_ht_get_num_entries (mgr->usr_bdd_map);
}

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
visit_formula (void *env, bdd_t *key, usr_bdd_entry_t val)
{
    visit_formula_pair_t *pair = env;
    (void) key;
    if (val.ref_cnt > 0)
        visit_formula_rec (pair->mgr, pair->visited, val.raw_bdd);
}

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

void
bdd_mgr_perform_gc (bdd_mgr_t *mgr)
{
    double n_formulae, n_nodes, nl_formulae, nl_nodes;

    n_formulae = num_formulae (mgr);
    n_nodes = bdd_mgr_get_num_nodes (mgr);
    nl_formulae = num_live_formulae (mgr);
    nl_nodes = num_live_nodes (mgr);

    fprintf (stderr, "*** bdd_mgr_perform_gc begin ***\n");
    fprintf (stderr, "    %.0f/%.0f live user formulae (%.2f%%)\n",
             nl_formulae, n_formulae, nl_formulae / n_formulae * 100);
    fprintf (stderr, "    %.0f/%.0f live nodes (%.2f%%)\n",
             nl_nodes, n_nodes, nl_nodes / n_nodes * 100);
    fprintf (stderr, "*** bdd_mgr_perform_gc end ***\n");
}
