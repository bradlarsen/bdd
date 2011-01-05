#include "node_ht.h"
#include "memory.h"

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

/* Allocates and initializes a new array of node_ht_entry_t. */
static node_ht_entry_t *
make_new_store (unsigned num_entries)
{
    node_ht_entry_t *store;
    unsigned i;
    store = (node_ht_entry_t *)
        checked_malloc (num_entries * sizeof(node_ht_entry_t));
    for (i = 0; i < num_entries; i += 1) {
        store[i].key.low = 0;
        store[i].key.high = 0;
    }
    return store;
}

void
node_ht_create (node_ht_t *tab)
{
    node_ht_create_with_hint (tab, 32);
}

void
node_ht_create_with_hint (node_ht_t *tab, unsigned capacity_hint)
{
    const unsigned capacity = up_to_next_power_of_two (capacity_hint);

    tab->num_entries = 0;
    tab->max_num_entries = capacity;
    tab->store = make_new_store (capacity);
}

/* Frees the memory used by the given hash table.  It is an error
 * to call this procedure more than once on a hash table. */
void
node_ht_destroy (node_ht_t *tab)
{
    checked_free (tab->store);
}

/* Double the capacity of the hash table. */
void
node_ht_double_size (node_ht_t *tab)
{
    node_ht_entry_t *old_store;
    const unsigned old_capacity = tab->max_num_entries;
    unsigned i;

    old_store = tab->store;

    tab->max_num_entries *= 2;
    tab->store = make_new_store (tab->max_num_entries);

    for (i = 0; i < old_capacity; i += 1) {
        node_ht_entry_t *old_entry = &old_store[i];
        if (!node_ht_entry_is_unoccupied(old_entry)) {
            node_ht_entry_t *entry = node_ht_lookup (tab, old_entry->key);
            if (node_ht_entry_is_unoccupied(entry))
                *entry = *old_entry;
            else {
                assert (node_equal(entry->key, old_entry->key));
                entry->val = old_entry->val;
            }
        }
    }

    checked_free (old_store);
}
