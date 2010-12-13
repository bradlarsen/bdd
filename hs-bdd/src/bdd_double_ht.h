/* THIS FILE WAS GENERATED FROM A TEMPLATE FILE.  MAKE MODIFICATIONS
TO THE TEMPLATE INSTEAD. */

/* This module provides a hash table from bdd_t to double
 * implemented using separate chaining. */

#ifndef BDD_DOUBLE_HT_INCLUDED
#define BDD_DOUBLE_HT_INCLUDED

#include "bdd.h"

/* A hash table implemented using separate chaining. */
typedef struct bdd_double_ht_t bdd_double_ht_t;

/* Creates and returns a new hash table with a default number of buckets. */
extern bdd_double_ht_t *
bdd_double_ht_create ();

/* Creates and returns a new hash table with a suggested number of buckets. */
extern bdd_double_ht_t *
bdd_double_ht_create_with_hint (unsigned num_buckets_hint);

/* Frees the memory used by the given hash table.  It is an error
 * to call this procedure more than once on a hash table. */
extern void
bdd_double_ht_destroy (bdd_double_ht_t *tab);

/* Gets the number of entries in the hash table. */
extern unsigned
bdd_double_ht_get_num_entries (bdd_double_ht_t *tab);

/* Gets the number of buckets in the hash table. */
extern unsigned
bdd_double_ht_get_num_buckets (bdd_double_ht_t *tab);

/* Return the hash table load, defined as the number of entries
 * divided by the number of buckets. */
extern float
bdd_double_ht_get_load (bdd_double_ht_t *tab);

/* Inserts a binding for the given key and value into the hash table.
 * If there is already an entry with the given key, its value is
 * replaced. */
extern void
bdd_double_ht_insert (
    bdd_double_ht_t *tab,
    bdd_t key,
    double val
    );

/* Retrieves a pointer to the value bound to the specified key.  If no
 * such entry exists, NULL is returned.  When any modifying hash table
 * operations are performed upon the table, it is an error to
 * dereference the pointer returned by this function. */
extern double *
bdd_double_ht_lookup (
    bdd_double_ht_t *tab,
    bdd_t key
    );

#endif /* BDD_DOUBLE_HT_INCLUDED */
