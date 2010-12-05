/* The definition of the node hash table types.  This hash table is
 * implemented using separate chaining. */

#ifndef NODE_HASH_TABLE_DEF_INCLUDED
#define NODE_HASH_TABLE_DEF_INCLUDED

#include "node_hash_table.h"
#include "hash_pair.h"
#include <assert.h>
#include <stdlib.h>


typedef struct node_bucket
{
    node_t key;
    unsigned value;
    struct node_bucket *next;
} node_bucket_t;

/* Allocates and initializes a bucket with the given parameters. */
extern node_bucket_t *
node_bucket_create (node_t key, unsigned value, node_bucket_t *next);

/* Frees the memory used by the list rooted by the bucket. */
extern void
node_bucket_free (node_bucket_t *bucket);

/* Performs a linear search down a bucket list for the given key.  A
 * pointer to the bucket with the matching key is returned if one
 * exists, and NULL returned otherwise. */
extern node_bucket_t *
node_bucket_search (node_bucket_t *bucket, node_t key);


struct node_hash_table
{
    unsigned num_entries;    /* the number of entries in the table */
    unsigned num_buckets;    /* the number of elems in the buckets array */
    node_bucket_t **buckets;   /* the array of buckets */
};

#define node_hash_table_check_invariants(tab)                   \
    do {                                                        \
        assert (tab != NULL);                                   \
        assert (tab->buckets != NULL);                          \
        assert (node_hash_table_proper_hash_values(tab));       \
        assert (node_hash_table_no_duplicate_keys(tab));        \
    } while (0)

/* Returns true if and only if every entry in a bucket hashes to that
 * bucket. */
extern bool
node_hash_table_proper_hash_values (node_hash_table_t *tab);

/* Returns true if and only if there are no two distinct entries with
 * the same key. */
extern bool
node_hash_table_no_duplicate_keys (node_hash_table_t *tab);


/* HASHING FUNCTIONS */

/* The node hash function h from Andersen's ``An introduction to
 * binary decision diagrams''. */
inline unsigned
node_hash_table_hash_node (node_t node)
{
    return hash_pair(node.var, hash_pair(node.low, node.high)) % 15485863;
}

/* Function h' from Andersen's ``An introduction to binary decision
 * diagrams''.  Gets the bucket index for the given key. */
inline unsigned
node_hash_table_get_hash_index (node_hash_table_t *tab, node_t key)
{
    return node_hash_table_hash_node (key) % tab->num_buckets;
}

#endif /* NODE_HASH_TABLE_DEF_INCLUDED */
