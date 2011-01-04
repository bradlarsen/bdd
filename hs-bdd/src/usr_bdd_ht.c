/* THIS FILE WAS GENERATED FROM A TEMPLATE FILE.  MAKE MODIFICATIONS
TO THE TEMPLATE INSTEAD. */

/* The definition of a separate-chaining hash table from bdd_t *
 * to usr_bdd_entry_t. */

#include "usr_bdd_ht.h"

#include <assert.h>
#include <stdlib.h>

/***********************************************************************/
/* HASH TABLE BUCKETS                                                  */
/***********************************************************************/
typedef struct usr_bdd_ht_bucket
{
    bdd_t * key;
    usr_bdd_entry_t value;
    struct usr_bdd_ht_bucket *next;
} usr_bdd_ht_bucket_t;

/* Allocates and initializes a bucket with the given parameters. */
static usr_bdd_ht_bucket_t *
usr_bdd_ht_bucket_create (bdd_t * key,
                            usr_bdd_entry_t value,
                            usr_bdd_ht_bucket_t *next)
{
    usr_bdd_ht_bucket_t * bucket = (usr_bdd_ht_bucket_t *)
        checked_malloc (sizeof(usr_bdd_ht_bucket_t));
    bucket->key = key;
    bucket->value = value;
    bucket->next = next;
    return bucket;
}

/* Frees the memory used by the list rooted by the bucket. */
static void
usr_bdd_ht_bucket_free (usr_bdd_ht_bucket_t *bucket)
{
    usr_bdd_ht_bucket_t *p = bucket;
    while (p != NULL) {
        usr_bdd_ht_bucket_t *prev = p;
        p = p->next;
        checked_free (prev);
    }
}

/* Performs a linear search down a bucket list for the given key.  A
 * pointer to the bucket with the matching key is returned if one
 * exists, and NULL returned otherwise. */
static usr_bdd_ht_bucket_t *
usr_bdd_ht_bucket_search (usr_bdd_ht_bucket_t *bucket, bdd_t * key)
{
    usr_bdd_ht_bucket_t *p = bucket;
    while (p != NULL) {
        if (bdd_ptr_equal(p->key, key))
            break;
        p = p->next;
    }
    return p;
}

/***********************************************************************/
/* HASH TABLE STRUCT                                                   */
/***********************************************************************/
struct usr_bdd_ht_t
{
    /* the number of entries in the table */
    unsigned num_entries;
    /* the number of elems in the buckets array */
    unsigned num_buckets;
    /* the array of buckets */
    usr_bdd_ht_bucket_t **buckets;
};

/* usr_bdd_ht_t invariants:
 *     - buckets != NULL
 *     - for each i in [0, num_entries), for each key/value entry in
 *       buckets[i], bdd_ptr_hash(key) == i.
 *     - For all key1/value1, key2/value2 pairs in the hash table, if
 *       key1 == key2, then value1 == value2
 */

/***********************************************************************/
/* HASHING FUNCTIONS                                                   */
/***********************************************************************/
/* Hashes the given key and computes the corresponding index for
 * it. */
static unsigned
usr_bdd_ht_get_hash_index (usr_bdd_ht_t *tab, bdd_t * key)
{
    return bdd_ptr_hash (key) % tab->num_buckets;
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
usr_bdd_ht_t *
usr_bdd_ht_create ()
{
    return usr_bdd_ht_create_with_hint (32);
}

/* Creates and returns a new hash table with a suggested number of buckets. */
usr_bdd_ht_t *
usr_bdd_ht_create_with_hint (unsigned num_buckets_hint)
{
    unsigned i;
    const unsigned num_buckets = up_to_next_power_of_two (num_buckets_hint);

    usr_bdd_ht_t *tab = (usr_bdd_ht_t *)
        checked_malloc (sizeof(usr_bdd_ht_t));
    tab->num_entries = 0;

    tab->num_buckets = num_buckets;
    tab->buckets = (usr_bdd_ht_bucket_t **)
        checked_malloc (num_buckets * sizeof(usr_bdd_ht_bucket_t *));
    for (i = 0; i < num_buckets; i += 1)
        tab->buckets[i] = NULL;
    return tab;
}

/* Frees the memory used by the given hash table.  It is an error
 * to call this procedure more than once on a hash table. */
void
usr_bdd_ht_destroy (usr_bdd_ht_t *tab)
{
    unsigned i;

    assert (tab != NULL);
    for (i = 0; i < tab->num_buckets; i += 1)
        usr_bdd_ht_bucket_free (tab->buckets[i]);
    checked_free (tab->buckets);
    checked_free (tab);
}

/***********************************************************************/
/* HASH TABLE ACCESSORS                                                */
/***********************************************************************/
/* Gets the number of entries in the hash table. */
unsigned
usr_bdd_ht_get_num_entries (usr_bdd_ht_t *tab)
{
    assert (tab != NULL);
    return tab->num_entries;
}

/* Gets the number of buckets in the hash table. */
unsigned
usr_bdd_ht_get_num_buckets (usr_bdd_ht_t *tab)
{
    assert (tab != NULL);
    return tab->num_buckets;
}

/* Return the hash table load, defined as the number of entries
 * divided by the number of buckets. */
float
usr_bdd_ht_get_load (usr_bdd_ht_t *tab)
{
    assert (tab != NULL);
    return
        (float) usr_bdd_ht_get_num_entries (tab) /
        (float) usr_bdd_ht_get_num_buckets (tab);
}

/* Double the number of buckets in the hash table. */
static void
double_hash_table_num_buckets (usr_bdd_ht_t *tab)
{
    usr_bdd_ht_bucket_t **old_buckets;
    const unsigned old_num_buckets = tab->num_buckets;

    unsigned i;
    usr_bdd_ht_bucket_t *p;

    old_buckets = tab->buckets;

    tab->num_buckets *= 2;
    tab->buckets = (usr_bdd_ht_bucket_t **)
        checked_malloc (tab->num_buckets * sizeof(usr_bdd_ht_bucket_t *));
    for (i = 0; i < tab->num_buckets; i += 1)
        tab->buckets[i] = NULL;
    tab->num_entries = 0;

    for (i = 0; i < old_num_buckets; i += 1) {
        for (p = old_buckets[i]; p != NULL; p = p->next) {
            usr_bdd_ht_insert (tab, p->key, p->value);
        }
        usr_bdd_ht_bucket_free (old_buckets[i]);
    }
    checked_free (old_buckets);
}

/* Inserts a binding for the given key and value into the hash table.
 * If there is already an entry with the given key, its value is
 * replaced. */
void
usr_bdd_ht_insert (usr_bdd_ht_t *tab,
                  bdd_t * key,
                  usr_bdd_entry_t val)
{
    unsigned b_idx;
    usr_bdd_ht_bucket_t *b;

    assert (tab != NULL);
    if (usr_bdd_ht_get_load(tab) > 0.70f)
        double_hash_table_num_buckets (tab);
    b_idx = usr_bdd_ht_get_hash_index (tab, key);
    b = usr_bdd_ht_bucket_search (tab->buckets[b_idx], key);
    if (b == NULL) {
        tab->buckets[b_idx] = usr_bdd_ht_bucket_create (key, val, tab->buckets[b_idx]);
        tab->num_entries += 1;
    }
    else
        b->value = val;
}

/* Retrieves a pointer to the value bound to the specified key.  If no
 * such entry exists, NULL is returned.  When any modifying hash table
 * operations are performed upon the table, it is an error to
 * dereference the pointer returned by this function. */
usr_bdd_entry_t *
usr_bdd_ht_lookup (usr_bdd_ht_t *tab, bdd_t * key)
{
    unsigned b_idx;
    usr_bdd_ht_bucket_t *b;

    assert (tab != NULL);
    b_idx = usr_bdd_ht_get_hash_index (tab, key);
    b = usr_bdd_ht_bucket_search (tab->buckets[b_idx], key);
    return b != NULL ? &b->value : NULL;
}

/***********************************************************************/
/* HASH TABLE MAPPING                                                  */
/***********************************************************************/
void
usr_bdd_ht_map_entries (
    usr_bdd_ht_t *tab,
    void *env,
    usr_bdd_ht_map_fun fun
    )
{
    unsigned idx;
    for (idx = 0; idx < usr_bdd_ht_get_num_buckets (tab); idx += 1) {
        usr_bdd_ht_bucket_t *p = tab->buckets[idx];
        while (p != NULL) {
            fun (env, p->key, &p->value);
            p = p->next;
        }
    }
}
