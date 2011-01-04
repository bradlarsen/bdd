/* THIS FILE WAS GENERATED FROM A TEMPLATE FILE.  MAKE MODIFICATIONS
TO THE TEMPLATE INSTEAD. */

/* This module provides a hash table from bdd_t * to usr_bdd_entry_t
 * implemented using separate chaining. */

#ifndef USR_BDD_HT_INCLUDED
#define USR_BDD_HT_INCLUDED

#include "bddlib.h"
#include "memory.h"
#include "bdd.h"
#include "usr_bdd_entry.h"

/* A hash table implemented using separate chaining. */
typedef struct usr_bdd_ht_t usr_bdd_ht_t;

/* Creates and returns a new hash table with a default number of buckets. */
extern usr_bdd_ht_t *
usr_bdd_ht_create ();

/* Creates and returns a new hash table with a suggested number of buckets. */
extern usr_bdd_ht_t *
usr_bdd_ht_create_with_hint (unsigned num_buckets_hint);

/* Frees the memory used by the given hash table.  It is an error
 * to call this procedure more than once on a hash table. */
extern void
usr_bdd_ht_destroy (usr_bdd_ht_t *tab);

/* Gets the number of entries in the hash table. */
extern unsigned
usr_bdd_ht_get_num_entries (usr_bdd_ht_t *tab);

/* Gets the number of buckets in the hash table. */
extern unsigned
usr_bdd_ht_get_num_buckets (usr_bdd_ht_t *tab);

/* Return the hash table load, defined as the number of entries
 * divided by the number of buckets. */
extern float
usr_bdd_ht_get_load (usr_bdd_ht_t *tab);

/* Inserts a binding for the given key and value into the hash table.
 * If there is already an entry with the given key, its value is
 * replaced. */
extern void
usr_bdd_ht_insert (
    usr_bdd_ht_t *tab,
    bdd_t * key,
    usr_bdd_entry_t val
    );

/* Retrieves a pointer to the value bound to the specified key.  If no
 * such entry exists, NULL is returned.  When any modifying hash table
 * operations are performed upon the table, it is an error to
 * dereference the pointer returned by this function. */
extern usr_bdd_entry_t *
usr_bdd_ht_lookup (
    usr_bdd_ht_t *tab,
    bdd_t * key
    );

typedef void (* usr_bdd_ht_map_fun) (bdd_t * key, usr_bdd_entry_t val);

/* Applies a function to each key/value entry. */
extern void
usr_bdd_ht_map_entries (usr_bdd_ht_t *tab, usr_bdd_ht_map_fun fun);

#endif /* USR_BDD_HT_INCLUDED */
