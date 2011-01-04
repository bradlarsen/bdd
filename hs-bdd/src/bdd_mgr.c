#include "bdd_impl.h"
#include <stdio.h>

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

/* Gets the node representing T for the given manager. */
static node_t
get_true_node (bdd_mgr_t *mgr)
{
    node_t t;
    t.var = mgr->num_vars;
    t.low = raw_bdd_false;
    t.high = raw_bdd_true;
    return t;
}

/* Gets the node representing F for the given manager. */
static node_t
get_false_node (bdd_mgr_t *mgr)
{
    node_t f;
    f.var = mgr->num_vars;
    f.low = raw_bdd_true;
    f.high = raw_bdd_false;
    return f;
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
    node_vec_create_with_capacity (&mgr->nodes_by_idx, capacity_hint);
    node_ht_create_with_hint (&mgr->idxs_by_node, capacity_hint);

    mgr->usr_bdd_map = usr_bdd_ht_create ();
    mgr->new_usr_id = 0;
    mgr->raw_bdd_map = bdd_rtu_ht_create ();

    /* FIXME: use a more reasonable cache size */
    bdd_ite_cache_create_with_hint (&mgr->ite_cache, 1024 * 32);

    intern_raw_bdd (mgr, make_node (mgr, get_false_node(mgr)));
    intern_raw_bdd (mgr, make_node (mgr, get_true_node(mgr)));

    return mgr;
}

/* Frees all the allocated bdd_t structs handed out by this manager. */
static void
free_usr_bdds (raw_bdd_t raw, bdd_t *usr)
{
    checked_free (usr);
}

void
bdd_mgr_destroy (bdd_mgr_t *mgr)
{
    if (mgr == NULL) return;

    fprintf (stderr, "!!! there are %u BDD nodes\n",
             bdd_mgr_get_num_nodes (mgr));
    fprintf (stderr, "!!! there are %u raw BDD entries\n",
             bdd_rtu_ht_get_num_entries (mgr->raw_bdd_map));
    fprintf (stderr, "!!! there are %u user BDD entries\n",
             usr_bdd_ht_get_num_entries (mgr->usr_bdd_map));

    bdd_ite_cache_destroy (&mgr->ite_cache);

    bdd_rtu_ht_map_entries (mgr->raw_bdd_map, free_usr_bdds);
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
