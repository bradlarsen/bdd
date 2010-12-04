#ifndef BDD_PAIR_HASH_TABLE_INCLUDED
#define BDD_PAIR_HASH_TABLE_INCLUDED

#include "bdd_pair.h"

#include <stdbool.h>

/* A hash table from bdd_pairs to indexes, implemented using separate
 * chaining. */
typedef struct bdd_pair_hash_table bdd_pair_hash_table_t;

/* Creates and returns a new hash table with a default number of buckets. */
extern bdd_pair_hash_table_t *
bdd_pair_hash_table_create ();

/* Creates and returns a new hash table with a suggested number of buckets. */
extern bdd_pair_hash_table_t *
bdd_pair_hash_table_create_with_hint (unsigned num_buckets_hint);

/* Frees the memory used by the given hash table.  It is an error
 * to call this procedure more than once on a hash table. */
extern void
bdd_pair_hash_table_destroy (bdd_pair_hash_table_t *tab);

/* Gets the number of entries in the hash table. */
extern unsigned
bdd_pair_hash_table_get_num_entries (bdd_pair_hash_table_t *tab);

/* Gets the number of buckets in the hash table. */
extern unsigned
bdd_pair_hash_table_get_num_buckets (bdd_pair_hash_table_t *tab);

/* Return the hash table load, defined as the number of entries
 * divided by the number of buckets. */
extern float
bdd_pair_hash_table_get_load (bdd_pair_hash_table_t *tab);

/* Indicates if there is an entry for the given key in the hash table. */
extern bool
bdd_pair_hash_table_has_key (bdd_pair_hash_table_t *tab, bdd_pair_t key);

/* Inserts a binding for the given key and value into the hash table.
 * If there is already an entry with the given key, its value is
 * replaced. */
extern void
bdd_pair_hash_table_insert (bdd_pair_hash_table_t *tab,
                            bdd_pair_t key,
                            unsigned val);

/* Retrieves a pointer to the value bound to the specified key.  If no
 * such entry exists, NULL is returned.  When any modifying hash table
 * operations are performed upon the table, it is an error to
 * dereference the pointer returned by this function. */
extern unsigned *
bdd_pair_hash_table_lookup (bdd_pair_hash_table_t *tab, bdd_pair_t key);

#endif /* BDD_PAIR_HASH_TABLE_INCLUDED */
