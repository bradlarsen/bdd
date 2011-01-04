/* THIS FILE WAS GENERATED FROM A TEMPLATE FILE.  MAKE MODIFICATIONS
TO THE TEMPLATE INSTEAD. */

/* This module provides a hash table from raw_bdd_t to bdd_t *
 * implemented using separate chaining. */

#ifndef BDD_RTU_INCLUDED
#define BDD_RTU_INCLUDED

#include "bddlib.h"
#include "raw_bdd.h"
#include "memory.h"

/* A hash table implemented using separate chaining. */
typedef struct bdd_rtu_ht_t bdd_rtu_ht_t;

/* Creates and returns a new hash table with a default number of buckets. */
extern bdd_rtu_ht_t *
bdd_rtu_ht_create ();

/* Creates and returns a new hash table with a suggested number of buckets. */
extern bdd_rtu_ht_t *
bdd_rtu_ht_create_with_hint (unsigned num_buckets_hint);

/* Frees the memory used by the given hash table.  It is an error
 * to call this procedure more than once on a hash table. */
extern void
bdd_rtu_ht_destroy (bdd_rtu_ht_t *tab);

/* Gets the number of entries in the hash table. */
extern unsigned
bdd_rtu_ht_get_num_entries (bdd_rtu_ht_t *tab);

/* Gets the number of buckets in the hash table. */
extern unsigned
bdd_rtu_ht_get_num_buckets (bdd_rtu_ht_t *tab);

/* Return the hash table load, defined as the number of entries
 * divided by the number of buckets. */
extern float
bdd_rtu_ht_get_load (bdd_rtu_ht_t *tab);

/* Inserts a binding for the given key and value into the hash table.
 * If there is already an entry with the given key, its value is
 * replaced. */
extern void
bdd_rtu_ht_insert (
    bdd_rtu_ht_t *tab,
    raw_bdd_t key,
    bdd_t * val
    );

/* Retrieves a pointer to the value bound to the specified key.  If no
 * such entry exists, NULL is returned.  When any modifying hash table
 * operations are performed upon the table, it is an error to
 * dereference the pointer returned by this function. */
extern bdd_t * *
bdd_rtu_ht_lookup (
    bdd_rtu_ht_t *tab,
    raw_bdd_t key
    );

typedef void (* bdd_rtu_ht_map_fun) (raw_bdd_t key, bdd_t * val);

/* Applies a function to each key/value entry. */
extern void
bdd_rtu_ht_map_entries (bdd_rtu_ht_t *tab, bdd_rtu_ht_map_fun fun);

#endif /* BDD_RTU_INCLUDED */
