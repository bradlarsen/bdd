/* The definition of a separate-chaining hash table from node_t
 * to bdd_t. */

#include "node_ht.h"
#include <math.h>

/***********************************************************************/
/* HASH TABLE CREATION AND DESTRUCTION                                 */
/***********************************************************************/
static unsigned
up_to_next_power_of_two (unsigned n)
{
    unsigned i;
    for (i = 1; i <= n; i *= 2) {}
    return i;
}

static void
create_bucket_pool (node_ht_bucket_pool_t *pool, unsigned num_buckets)
{
    node_ht_bucket_pool_create (pool,
                           (unsigned)ceilf (NODE_HT_MAX_LOAD * num_buckets));
}

/* Creates and returns a new hash table with a default number of buckets. */
void
node_ht_create (node_ht_t *tab)
{
    node_ht_create_with_hint (tab, 32);
}

/* Creates and returns a new hash table with a suggested number of buckets. */
void
node_ht_create_with_hint (node_ht_t *tab, unsigned num_buckets_hint)
{
    const unsigned num_buckets = up_to_next_power_of_two (num_buckets_hint);

    tab->num_entries = 0;
    tab->num_buckets = num_buckets;
    tab->buckets = (node_ht_bucket_t **)
        checked_calloc (num_buckets, sizeof(node_ht_bucket_t *));
    create_bucket_pool (&tab->pool, num_buckets);
}

/* Frees the memory used by the given hash table.  It is an error
 * to call this procedure more than once on a hash table. */
void
node_ht_destroy (node_ht_t *tab)
{
    node_ht_bucket_pool_destroy (&tab->pool);
    checked_free (tab->buckets);
}

/* Double the number of buckets in the hash table. */
void
node_ht_double_num_buckets (node_ht_t *tab)
{
    node_ht_bucket_pool_t old_pool;
    node_ht_bucket_t **old_buckets;
    const unsigned old_num_buckets = tab->num_buckets;

    unsigned i;
    node_ht_bucket_t *p;

    old_pool = tab->pool;
    old_buckets = tab->buckets;

    tab->num_buckets *= 2;
    tab->buckets = (node_ht_bucket_t **)
        checked_malloc (tab->num_buckets * sizeof(node_ht_bucket_t *));
    for (i = 0; i < tab->num_buckets; i += 1)
        tab->buckets[i] = NULL;
    tab->num_entries = 0;
    create_bucket_pool (&tab->pool, tab->num_buckets);

    for (i = 0; i < old_num_buckets; i += 1) {
        for (p = old_buckets[i]; p != NULL; p = p->next) {
            node_ht_insert (tab, p->key, p->value);
        }
    }
    checked_free (old_buckets);
    node_ht_bucket_pool_destroy (&old_pool);
}
