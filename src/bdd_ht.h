/* THIS FILE WAS GENERATED FROM A TEMPLATE FILE.  MAKE MODIFICATIONS
TO THE TEMPLATE INSTEAD. */

/* This module provides a hash table from bdd_t to bdd_t
 * implemented using separate chaining. */

#ifndef BDD_HT_INCLUDED
#define BDD_HT_INCLUDED


#include "bdd.h"
#include <stdbool.h>

static inline bool
bdd_equal (bdd_t b1, bdd_t b2)
{
    return b1 == b2;
}

static inline unsigned
bdd_hash (bdd_t b)
{
    return b;
}


/* A hash table implemented using separate chaining. */
typedef struct bdd_ht_t bdd_ht_t;

/* Creates and returns a new hash table with a default number of buckets. */
extern bdd_ht_t *
bdd_ht_create ();

/* Creates and returns a new hash table with a suggested number of buckets. */
extern bdd_ht_t *
bdd_ht_create_with_hint (unsigned num_buckets_hint);

/* Frees the memory used by the given hash table.  It is an error
 * to call this procedure more than once on a hash table. */
extern void
bdd_ht_destroy (bdd_ht_t *tab);

/* Gets the number of entries in the hash table. */
extern unsigned
bdd_ht_get_num_entries (bdd_ht_t *tab);

/* Gets the number of buckets in the hash table. */
extern unsigned
bdd_ht_get_num_buckets (bdd_ht_t *tab);

/* Return the hash table load, defined as the number of entries
 * divided by the number of buckets. */
extern float
bdd_ht_get_load (bdd_ht_t *tab);

/* Inserts a binding for the given key and value into the hash table.
 * If there is already an entry with the given key, its value is
 * replaced. */
extern void
bdd_ht_insert (
    bdd_ht_t *tab,
    bdd_t key,
    bdd_t val
    );

/* Retrieves a pointer to the value bound to the specified key.  If no
 * such entry exists, NULL is returned.  When any modifying hash table
 * operations are performed upon the table, it is an error to
 * dereference the pointer returned by this function. */
extern bdd_t *
bdd_ht_lookup (
    bdd_ht_t *tab,
    bdd_t key
    );

#endif /* BDD_HT_INCLUDED */