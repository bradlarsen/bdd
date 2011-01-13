/* BDD garbage collection code. */

#include "bdd_mgr.h"

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

/* FIXME: enable garbage collection to occur before increasing vector size */
void
bdd_mgr_perform_gc (bdd_mgr_t *mgr)
{
    /* FIXME: reimplement */
}
