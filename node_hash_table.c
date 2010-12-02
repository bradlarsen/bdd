#include "node_hash_table.h"

#include <assert.h>
#include <stdlib.h>

/* A bucket in the hash table, i.e., a singly-linked list of (node_t,
 * unsigned) values. */
typedef struct hash_bucket hash_bucket_t;

struct hash_bucket
{
    node_t key;
    unsigned value;
    hash_bucket_t *next;
};

/* Performs a linear search through the linked list, returning the
list node with matching key if one exists, and returning NULL
otherwise. */
static hash_bucket_t *
hash_bucket_find_node (hash_bucket_t *bucket, node_t key)
{
    hash_bucket_t *p = bucket;
    while (p != NULL) {
        if (node_equal(p->key, key)) return p;
        else p = p->next;
    }
    return p;
}

static unsigned *
hash_bucket_find_value (hash_bucket_t *bucket, node_t key)
{
    hash_bucket_t *node = hash_bucket_find_node (bucket, key);
    if (node == NULL)
        return NULL;
    else
        return &node->value;
}

static hash_bucket_t *
hash_bucket_push_front (hash_bucket_t *bucket, node_t key, unsigned value)
{
    hash_bucket_t *new_bucket = (hash_bucket_t *) malloc (sizeof(hash_bucket_t));
    new_bucket->key = key;
    new_bucket->value = value;
    new_bucket->next = bucket;
    return new_bucket;
}

static void
hash_bucket_free (hash_bucket_t *bucket)
{
    hash_bucket_t *p = bucket;
    while (p != NULL) {
        hash_bucket_t *prev = p;
        p = p->next;
        free(prev);
    }        
}

static unsigned
hash_bucket_length (hash_bucket_t *bucket)
{
    unsigned count = 0;
    hash_bucket_t *p = bucket;
    while (p != NULL) {
        count += 1;
        p = p->next;
    }
    return count;
}




struct node_hash_table
{
    unsigned num_entries;        /* the number of entries in the table */
    unsigned num_buckets;        /* number of buckets */
    hash_bucket_t **buckets;     /* bucket array */
};

static inline bool
is_power_of_two (unsigned n)
{
    unsigned i;
    for (i = 1; i < n; i *= 2);
    return i == n;
}

static inline unsigned
grow_to_power_of_two (unsigned n)
{
    unsigned i;
    for (i = 1; i < n; i *= 2);
    return i;
}

static unsigned
count_num_entries (node_hash_table_t *tab)
{
    unsigned count = 0;
    for (unsigned i = 0; i < tab->num_buckets; i += 1)
        count += hash_bucket_length (tab->buckets[i]);
    return count;
}

static void
node_hash_table_check_invariants (node_hash_table_t *tab)
{
    assert (tab != NULL);
    assert (tab->buckets != NULL);
    assert (is_power_of_two (tab->num_buckets));
    assert (tab->num_entries == count_num_entries (tab));
}

static inline unsigned
hash_pair (unsigned i, unsigned j)
{
    return ((i + j) * (i + j + 1) / 2) + i;
}

static unsigned
compute_hash_idx (node_hash_table_t *tab, node_t node)
{
    node_hash_table_check_invariants (tab);
    const unsigned m = 86028121;    /* the 5,000,000th prime */
    const unsigned hash =
        hash_pair(node.var, hash_pair(node.low, node.high)) % m;
    return hash % tab->num_buckets;
}

node_hash_table_t *
node_hash_table_create ()
{
    const unsigned default_num_buckets = 32;
    return node_hash_table_create_with_hint (default_num_buckets);
}

node_hash_table_t *
node_hash_table_create_with_hint (unsigned num_buckets_hint)
{
    node_hash_table_t *tab =
        (node_hash_table_t *) malloc (sizeof(node_hash_table_t));
    const unsigned num_buckets = grow_to_power_of_two (num_buckets_hint);
    tab->num_buckets = num_buckets;
    tab->num_entries = 0;
    tab->buckets =
        (hash_bucket_t **) malloc (num_buckets * sizeof(hash_bucket_t *));
    for (unsigned i = 0; i < num_buckets; i += 1)
        tab->buckets[i] = NULL;
    node_hash_table_check_invariants (tab);
    return tab;
}

void
node_hash_table_destroy (node_hash_table_t *tab)
{
    if (tab == NULL) return;
    node_hash_table_check_invariants (tab);
    for (unsigned i = 0; i < tab->num_buckets; i += 1)
        hash_bucket_free(tab->buckets[i]);
    free(tab->buckets);
    free(tab);
}

unsigned
node_hash_table_get_num_entries (node_hash_table_t *tab)
{
    node_hash_table_check_invariants (tab);
    return tab->num_entries;
}

unsigned
node_hash_table_get_num_buckets (node_hash_table_t *tab)
{
    node_hash_table_check_invariants (tab);
    return tab->num_buckets;
}

static inline unsigned *
find_value (node_hash_table_t *tab, node_t key)
{
    node_hash_table_check_invariants (tab);
    unsigned hash_idx = compute_hash_idx (tab, key);
    return hash_bucket_find_value (tab->buckets[hash_idx], key);
}

bool
node_hash_table_has_key (node_hash_table_t *tab, node_t key)
{
    return find_value (tab, key) != NULL;
}

static inline bool
node_hash_table_should_grow (node_hash_table_t *tab)
{
    float load = (float) tab->num_entries / (float) tab->num_buckets;
    return load > 0.70;
}

static void
double_hash_table_size (node_hash_table_t *tab)
{
    const unsigned old_num_buckets = tab->num_buckets;
    hash_bucket_t **old_buckets = tab->buckets;

    const unsigned new_num_buckets = tab->num_buckets * 2;
    tab->num_buckets = new_num_buckets;
    tab->buckets =
        (hash_bucket_t **) malloc (new_num_buckets * sizeof(hash_bucket_t *));
    for (unsigned i = 0; i < tab->num_buckets; i += 1)
        tab->buckets[i] = NULL;

    for (unsigned i = 0; i < old_num_buckets; i += 1) {
        hash_bucket_t *p = old_buckets[i];
        while (p != NULL) {
            node_hash_table_insert (tab, p->key, p->value);
            p = p->next;
        }
        hash_bucket_free (old_buckets[i]);
    }
    free (old_buckets);

    node_hash_table_check_invariants (tab);
}

void
node_hash_table_insert (node_hash_table_t *tab,
                        node_t key,
                        unsigned val)
{
    node_hash_table_check_invariants (tab);
    if (node_hash_table_should_grow (tab))
        double_hash_table_size (tab);
    unsigned hash_idx = compute_hash_idx (tab, key);
    unsigned *val_ptr = hash_bucket_find_value (tab->buckets[hash_idx], key);
    if (val_ptr == NULL) {
        tab->buckets[hash_idx] =
            hash_bucket_push_front(tab->buckets[hash_idx], key, val);
        tab->num_entries += 1;
    }
    else {
        *val_ptr = val;
    }
    node_hash_table_check_invariants (tab);
}

unsigned *
node_hash_table_lookup (node_hash_table_t *tab, node_t key)
{
    return find_value (tab, key);
}
