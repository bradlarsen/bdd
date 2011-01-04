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

    /* add false node */
    intern_raw_bdd (
        mgr,
        make_node (mgr, mgr->num_vars, raw_bdd_true, raw_bdd_false)
        );

    /* add true node */
    intern_raw_bdd (
        mgr,
        make_node (mgr, mgr->num_vars, raw_bdd_false, raw_bdd_true)
        );

    return mgr;
}

/* Frees all the allocated bdd_t structs handed out by this manager. */
static void
free_usr_bdds (void *env, raw_bdd_t raw, bdd_t *usr)
{
    (void) env;
    (void) raw;
    checked_free (usr);
}

void
bdd_mgr_destroy (bdd_mgr_t *mgr)
{
    if (mgr == NULL) return;

    assert (bdd_rtu_ht_get_num_entries (mgr->raw_bdd_map) ==
            usr_bdd_ht_get_num_entries (mgr->usr_bdd_map));
    assert (bdd_rtu_ht_get_num_entries (mgr->raw_bdd_map) <=
            bdd_mgr_get_num_nodes (mgr));

    bdd_ite_cache_destroy (&mgr->ite_cache);

    bdd_rtu_ht_map_entries (mgr->raw_bdd_map, NULL, free_usr_bdds);
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
