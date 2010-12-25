/* The definition of a separate-chaining hash table from node_t
 * to bdd_t. */

#include "node_ht.h"

/***********************************************************************/
/* HASH TABLE STRUCT INVARIANTS CHECKING                               */
/***********************************************************************/
#define node_ht_check_invariants(tab)                   \
    do {                                                \
        assert (tab != NULL);                           \
        assert (tab->buckets != NULL);                  \
        assert (node_ht_proper_hash_values(tab));       \
        assert (node_ht_no_duplicate_keys(tab));        \
    } while (0)

#ifndef NDEBUG
/* Returns true if and only if every entry in a bucket hashes to that
 * bucket. */
static bool
node_ht_proper_hash_values (node_ht_t *tab)
{
    unsigned i;
    ht_bucket_t *p;
    for (i = 0; i < tab->num_buckets; i += 1)
        for (p = tab->buckets[i]; p != NULL; p = p->next)
            if (node_ht_get_hash_index(tab, p->key) != i)
                return false;
    return true;
}

/* Returns true if and only if there are no two distinct entries with
 * the same key. */
static bool
node_ht_no_duplicate_keys (node_ht_t *tab)
{
    unsigned num_buckets;
    ht_bucket_t **buckets;

    unsigned i, j;
    ht_bucket_t *p, *q;

    num_buckets = tab->num_buckets;
    buckets = tab->buckets;

    for (i = 0; i < num_buckets; i += 1)
        for (p = buckets[i]; p != NULL; p = p->next)
            for (j = i; j < num_buckets; j += 1)
                for (q = buckets[j]; q != NULL; q = q->next)
                    if (p != q && node_equal(p->key, q->key))
                        return false;
    return true;
}
#endif /* NDEBUG */

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
    unsigned i;
    const unsigned num_buckets = up_to_next_power_of_two (num_buckets_hint);

    tab->num_entries = 0;

    tab->num_buckets = num_buckets;
    tab->buckets =
        (ht_bucket_t **) malloc (num_buckets * sizeof(ht_bucket_t *));
    for (i = 0; i < num_buckets; i += 1)
        tab->buckets[i] = NULL;
    node_ht_check_invariants (tab);
}

/* Frees the memory used by the given hash table.  It is an error
 * to call this procedure more than once on a hash table. */
void
node_ht_destroy (node_ht_t *tab)
{
    unsigned i;

    node_ht_check_invariants (tab);
    for (i = 0; i < tab->num_buckets; i += 1)
        ht_bucket_free (tab->buckets[i]);
    free (tab->buckets);
}

/* Double the number of buckets in the hash table. */
void
double_hash_table_num_buckets (node_ht_t *tab)
{
    ht_bucket_t **old_buckets;
    const unsigned old_num_buckets = tab->num_buckets;

    unsigned i;
    ht_bucket_t *p;

    old_buckets = tab->buckets;

    tab->num_buckets *= 2;
    tab->buckets =
        (ht_bucket_t **) malloc (tab->num_buckets * sizeof(ht_bucket_t *));
    for (i = 0; i < tab->num_buckets; i += 1)
        tab->buckets[i] = NULL;
    tab->num_entries = 0;

    for (i = 0; i < old_num_buckets; i += 1) {
        for (p = old_buckets[i]; p != NULL; p = p->next) {
            node_ht_insert (tab, p->key, p->value);
        }
        ht_bucket_free (old_buckets[i]);
    }
    free (old_buckets);

    node_ht_check_invariants (tab);
}
