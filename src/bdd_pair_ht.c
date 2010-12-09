/* THIS FILE WAS GENERATED FROM A TEMPLATE FILE.  MAKE MODIFICATIONS
TO THE TEMPLATE INSTEAD. */

/* The definition of a separate-chaining hash table from bdd_pair_t
 * to bdd_t. */

#include "bdd_pair_ht.h"

#include <assert.h>
#include <stdlib.h>

/***********************************************************************/
/* HASH TABLE BUCKETS                                                  */
/***********************************************************************/
typedef struct ht_bucket
{
    bdd_pair_t key;
    bdd_t value;
    struct ht_bucket *next;
} ht_bucket_t;

/* Allocates and initializes a bucket with the given parameters. */
static ht_bucket_t *
ht_bucket_create (bdd_pair_t key,
                  bdd_t value,
                  ht_bucket_t *next)
{
    ht_bucket_t * bucket = (ht_bucket_t *) malloc (sizeof(ht_bucket_t));
    bucket->key = key;
    bucket->value = value;
    bucket->next = next;
    return bucket;
}

/* Frees the memory used by the list rooted by the bucket. */
static void
ht_bucket_free (ht_bucket_t *bucket)
{
    ht_bucket_t *p = bucket;
    while (p != NULL) {
        ht_bucket_t *prev = p;
        p = p->next;
        free (prev);
    }
}

/* Performs a linear search down a bucket list for the given key.  A
 * pointer to the bucket with the matching key is returned if one
 * exists, and NULL returned otherwise. */
static ht_bucket_t *
ht_bucket_search (ht_bucket_t *bucket, bdd_pair_t key)
{
    ht_bucket_t *p = bucket;
    while (p != NULL) {
        if (bdd_pair_equal(p->key, key))
            break;
        p = p->next;
    }
    return p;
}

/***********************************************************************/
/* HASH TABLE STRUCT                                                   */
/***********************************************************************/
struct bdd_pair_ht_t
{
    /* the number of entries in the table */
    unsigned num_entries;
    /* the number of elems in the buckets array */
    unsigned num_buckets;
    /* the array of buckets */
    ht_bucket_t **buckets;
};

/***********************************************************************/
/* HASHING FUNCTIONS                                                   */
/***********************************************************************/
/* Hashes the given key and computes the corresponding index for
 * it. */
static unsigned
bdd_pair_ht_get_hash_index (bdd_pair_ht_t *tab, bdd_pair_t key)
{
    return bdd_pair_hash (key) % tab->num_buckets;
}

/***********************************************************************/
/* HASH TABLE STRUCT INVARIANTS CHECKING                               */
/***********************************************************************/
#define bdd_pair_ht_check_invariants(tab)                 \
    do {                                                \
        assert (tab != NULL);                           \
        assert (tab->buckets != NULL);                  \
        assert (bdd_pair_ht_proper_hash_values(tab));     \
        assert (bdd_pair_ht_no_duplicate_keys(tab));      \
    } while (0)

#ifndef NDEBUG
/* Returns true if and only if every entry in a bucket hashes to that
 * bucket. */
static bool
bdd_pair_ht_proper_hash_values (bdd_pair_ht_t *tab)
{
    unsigned i;
    ht_bucket_t *p;
    for (i = 0; i < tab->num_buckets; i += 1)
        for (p = tab->buckets[i]; p != NULL; p = p->next)
            if (bdd_pair_ht_get_hash_index(tab, p->key) != i)
                return false;
    return true;
}

/* Returns true if and only if there are no two distinct entries with
 * the same key. */
static bool
bdd_pair_ht_no_duplicate_keys (bdd_pair_ht_t *tab)
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
                    if (p != q && bdd_pair_equal(p->key, q->key))
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
    for (i = 1; i <= n; i *= 2);
    return i;
}

/* Creates and returns a new hash table with a default number of buckets. */
bdd_pair_ht_t *
bdd_pair_ht_create ()
{
    return bdd_pair_ht_create_with_hint (32);
}

/* Creates and returns a new hash table with a suggested number of buckets. */
bdd_pair_ht_t *
bdd_pair_ht_create_with_hint (unsigned num_buckets_hint)
{
    unsigned i;
    const unsigned num_buckets = up_to_next_power_of_two (num_buckets_hint);

    bdd_pair_ht_t *tab =
        (bdd_pair_ht_t *) malloc (sizeof(bdd_pair_ht_t));
    tab->num_entries = 0;

    tab->num_buckets = num_buckets;
    tab->buckets =
        (ht_bucket_t **) malloc (num_buckets * sizeof(ht_bucket_t *));
    for (i = 0; i < num_buckets; i += 1)
        tab->buckets[i] = NULL;
    bdd_pair_ht_check_invariants (tab);
    return tab;
}

/* Frees the memory used by the given hash table.  It is an error
 * to call this procedure more than once on a hash table. */
void
bdd_pair_ht_destroy (bdd_pair_ht_t *tab)
{
    unsigned i;

    bdd_pair_ht_check_invariants (tab);
    for (i = 0; i < tab->num_buckets; i += 1)
        ht_bucket_free (tab->buckets[i]);
    free (tab->buckets);
    free (tab);
}

/***********************************************************************/
/* HASH TABLE ACCESSORS                                                */
/***********************************************************************/
/* Gets the number of entries in the hash table. */
unsigned
bdd_pair_ht_get_num_entries (bdd_pair_ht_t *tab)
{
    bdd_pair_ht_check_invariants (tab);
    return tab->num_entries;
}

/* Gets the number of buckets in the hash table. */
unsigned
bdd_pair_ht_get_num_buckets (bdd_pair_ht_t *tab)
{
    bdd_pair_ht_check_invariants (tab);
    return tab->num_buckets;
}

/* Return the hash table load, defined as the number of entries
 * divided by the number of buckets. */
float
bdd_pair_ht_get_load (bdd_pair_ht_t *tab)
{
    bdd_pair_ht_check_invariants (tab);
    return
        (float) bdd_pair_ht_get_num_entries (tab) /
        (float) bdd_pair_ht_get_num_buckets (tab);
}

/* Double the number of buckets in the hash table. */
static void
double_hash_table_num_buckets (bdd_pair_ht_t *tab)
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
            bdd_pair_ht_insert (tab, p->key, p->value);
        }
        ht_bucket_free (old_buckets[i]);
    }
    free (old_buckets);

    bdd_pair_ht_check_invariants (tab);
}

/* Inserts a binding for the given key and value into the hash table.
 * If there is already an entry with the given key, its value is
 * replaced. */
void
bdd_pair_ht_insert (bdd_pair_ht_t *tab,
                  bdd_pair_t key,
                  bdd_t val)
{
    unsigned b_idx;
    ht_bucket_t *b;

    bdd_pair_ht_check_invariants (tab);
    if (bdd_pair_ht_get_load(tab) > 0.70f)
        double_hash_table_num_buckets (tab);
    b_idx = bdd_pair_ht_get_hash_index (tab, key);
    b = ht_bucket_search (tab->buckets[b_idx], key);
    if (b == NULL) {
        tab->buckets[b_idx] = ht_bucket_create (key, val, tab->buckets[b_idx]);
        tab->num_entries += 1;
    }
    else
        b->value = val;
}

/* Retrieves a pointer to the value bound to the specified key.  If no
 * such entry exists, NULL is returned.  When any modifying hash table
 * operations are performed upon the table, it is an error to
 * dereference the pointer returned by this function. */
bdd_t *
bdd_pair_ht_lookup (bdd_pair_ht_t *tab, bdd_pair_t key)
{
    unsigned b_idx;
    ht_bucket_t *b;

    bdd_pair_ht_check_invariants (tab);
    b_idx = bdd_pair_ht_get_hash_index (tab, key);
    b = ht_bucket_search (tab->buckets[b_idx], key);
    return b != NULL ? &b->value : NULL;
}
