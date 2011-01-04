/* THIS FILE WAS GENERATED FROM A TEMPLATE FILE.  MAKE MODIFICATIONS
TO THE TEMPLATE INSTEAD. */

/* The definition of a separate-chaining hash table from raw_bdd_t
 * to bdd_t *. */

#include "bdd_rtu_ht.h"

#include <assert.h>
#include <stdlib.h>

/***********************************************************************/
/* HASH TABLE BUCKETS                                                  */
/***********************************************************************/
typedef struct bdd_rtu_ht_bucket
{
    raw_bdd_t key;
    bdd_t * value;
    struct bdd_rtu_ht_bucket *next;
} bdd_rtu_ht_bucket_t;

/* Allocates and initializes a bucket with the given parameters. */
static bdd_rtu_ht_bucket_t *
bdd_rtu_ht_bucket_create (raw_bdd_t key,
                            bdd_t * value,
                            bdd_rtu_ht_bucket_t *next)
{
    bdd_rtu_ht_bucket_t * bucket = (bdd_rtu_ht_bucket_t *)
        checked_malloc (sizeof(bdd_rtu_ht_bucket_t));
    bucket->key = key;
    bucket->value = value;
    bucket->next = next;
    return bucket;
}

/* Frees the memory used by the list rooted by the bucket. */
static void
bdd_rtu_ht_bucket_free (bdd_rtu_ht_bucket_t *bucket)
{
    bdd_rtu_ht_bucket_t *p = bucket;
    while (p != NULL) {
        bdd_rtu_ht_bucket_t *prev = p;
        p = p->next;
        checked_free (prev);
    }
}

/* Performs a linear search down a bucket list for the given key.  A
 * pointer to the bucket with the matching key is returned if one
 * exists, and NULL returned otherwise. */
static bdd_rtu_ht_bucket_t *
bdd_rtu_ht_bucket_search (bdd_rtu_ht_bucket_t *bucket, raw_bdd_t key)
{
    bdd_rtu_ht_bucket_t *p = bucket;
    while (p != NULL) {
        if (raw_bdd_equal(p->key, key))
            break;
        p = p->next;
    }
    return p;
}

/***********************************************************************/
/* HASH TABLE STRUCT                                                   */
/***********************************************************************/
struct bdd_rtu_ht_t
{
    /* the number of entries in the table */
    unsigned num_entries;
    /* the number of elems in the buckets array */
    unsigned num_buckets;
    /* the array of buckets */
    bdd_rtu_ht_bucket_t **buckets;
};

/* bdd_rtu_ht_t invariants:
 *     - buckets != NULL
 *     - for each i in [0, num_entries), for each key/value entry in
 *       buckets[i], raw_bdd_hash(key) == i.
 *     - For all key1/value1, key2/value2 pairs in the hash table, if
 *       key1 == key2, then value1 == value2
 */

/***********************************************************************/
/* HASHING FUNCTIONS                                                   */
/***********************************************************************/
/* Hashes the given key and computes the corresponding index for
 * it. */
static unsigned
bdd_rtu_ht_get_hash_index (bdd_rtu_ht_t *tab, raw_bdd_t key)
{
    return raw_bdd_hash (key) % tab->num_buckets;
}

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
bdd_rtu_ht_t *
bdd_rtu_ht_create ()
{
    return bdd_rtu_ht_create_with_hint (32);
}

/* Creates and returns a new hash table with a suggested number of buckets. */
bdd_rtu_ht_t *
bdd_rtu_ht_create_with_hint (unsigned num_buckets_hint)
{
    unsigned i;
    const unsigned num_buckets = up_to_next_power_of_two (num_buckets_hint);

    bdd_rtu_ht_t *tab = (bdd_rtu_ht_t *)
        checked_malloc (sizeof(bdd_rtu_ht_t));
    tab->num_entries = 0;

    tab->num_buckets = num_buckets;
    tab->buckets = (bdd_rtu_ht_bucket_t **)
        checked_malloc (num_buckets * sizeof(bdd_rtu_ht_bucket_t *));
    for (i = 0; i < num_buckets; i += 1)
        tab->buckets[i] = NULL;
    return tab;
}

/* Frees the memory used by the given hash table.  It is an error
 * to call this procedure more than once on a hash table. */
void
bdd_rtu_ht_destroy (bdd_rtu_ht_t *tab)
{
    unsigned i;

    assert (tab != NULL);
    for (i = 0; i < tab->num_buckets; i += 1)
        bdd_rtu_ht_bucket_free (tab->buckets[i]);
    checked_free (tab->buckets);
    checked_free (tab);
}

/***********************************************************************/
/* HASH TABLE ACCESSORS                                                */
/***********************************************************************/
/* Gets the number of entries in the hash table. */
unsigned
bdd_rtu_ht_get_num_entries (bdd_rtu_ht_t *tab)
{
    assert (tab != NULL);
    return tab->num_entries;
}

/* Gets the number of buckets in the hash table. */
unsigned
bdd_rtu_ht_get_num_buckets (bdd_rtu_ht_t *tab)
{
    assert (tab != NULL);
    return tab->num_buckets;
}

/* Return the hash table load, defined as the number of entries
 * divided by the number of buckets. */
float
bdd_rtu_ht_get_load (bdd_rtu_ht_t *tab)
{
    assert (tab != NULL);
    return
        (float) bdd_rtu_ht_get_num_entries (tab) /
        (float) bdd_rtu_ht_get_num_buckets (tab);
}

/* Double the number of buckets in the hash table. */
static void
double_hash_table_num_buckets (bdd_rtu_ht_t *tab)
{
    bdd_rtu_ht_bucket_t **old_buckets;
    const unsigned old_num_buckets = tab->num_buckets;

    unsigned i;
    bdd_rtu_ht_bucket_t *p;

    old_buckets = tab->buckets;

    tab->num_buckets *= 2;
    tab->buckets = (bdd_rtu_ht_bucket_t **)
        checked_malloc (tab->num_buckets * sizeof(bdd_rtu_ht_bucket_t *));
    for (i = 0; i < tab->num_buckets; i += 1)
        tab->buckets[i] = NULL;
    tab->num_entries = 0;

    for (i = 0; i < old_num_buckets; i += 1) {
        for (p = old_buckets[i]; p != NULL; p = p->next) {
            bdd_rtu_ht_insert (tab, p->key, p->value);
        }
        bdd_rtu_ht_bucket_free (old_buckets[i]);
    }
    checked_free (old_buckets);
}

/* Inserts a binding for the given key and value into the hash table.
 * If there is already an entry with the given key, its value is
 * replaced. */
void
bdd_rtu_ht_insert (bdd_rtu_ht_t *tab,
                  raw_bdd_t key,
                  bdd_t * val)
{
    unsigned b_idx;
    bdd_rtu_ht_bucket_t *b;

    assert (tab != NULL);
    if (bdd_rtu_ht_get_load(tab) > 0.70f)
        double_hash_table_num_buckets (tab);
    b_idx = bdd_rtu_ht_get_hash_index (tab, key);
    b = bdd_rtu_ht_bucket_search (tab->buckets[b_idx], key);
    if (b == NULL) {
        tab->buckets[b_idx] = bdd_rtu_ht_bucket_create (key, val, tab->buckets[b_idx]);
        tab->num_entries += 1;
    }
    else
        b->value = val;
}

/* Retrieves a pointer to the value bound to the specified key.  If no
 * such entry exists, NULL is returned.  When any modifying hash table
 * operations are performed upon the table, it is an error to
 * dereference the pointer returned by this function. */
bdd_t * *
bdd_rtu_ht_lookup (bdd_rtu_ht_t *tab, raw_bdd_t key)
{
    unsigned b_idx;
    bdd_rtu_ht_bucket_t *b;

    assert (tab != NULL);
    b_idx = bdd_rtu_ht_get_hash_index (tab, key);
    b = bdd_rtu_ht_bucket_search (tab->buckets[b_idx], key);
    return b != NULL ? &b->value : NULL;
}

/***********************************************************************/
/* HASH TABLE MAPPING                                                  */
/***********************************************************************/
void
bdd_rtu_ht_map_entries (
    bdd_rtu_ht_t *tab,
    void *env,
    bdd_rtu_ht_map_fun fun
    )
{
    unsigned idx;
    for (idx = 0; idx < bdd_rtu_ht_get_num_buckets (tab); idx += 1) {
        bdd_rtu_ht_bucket_t *p = tab->buckets[idx];
        while (p != NULL) {
            fun (env, p->key, p->value);
            p = p->next;
        }
    }
}
