#include "bdd_pair_cache.h"

#include <assert.h>
#include <stdlib.h>

static unsigned
up_to_next_power_of_two (unsigned n)
{
    unsigned i;
    for (i = 1; i <= n; i *= 2) {}
    return i;
}

bdd_pair_cache_t
bdd_pair_cache_create_with_hint (unsigned num_entries_hint)
{
    unsigned i;
    const unsigned num_entries = up_to_next_power_of_two (num_entries_hint);

    bdd_pair_cache_t tab;
    tab.num_entries = num_entries;
    tab.entries =
        (cache_entry_t *) malloc (sizeof(cache_entry_t) * num_entries);

    for (i = 0; i < num_entries; i += 1) {
        const cache_entry_t sentinel = { {0, 0}, bdd_false };
        tab.entries[i] = sentinel;
    }
    return tab;
}

/* Frees the memory used by the given hash table.  It is an error
 * to call this procedure more than once on a hash table. */
void
bdd_pair_cache_destroy (bdd_pair_cache_t tab)
{
    free (tab.entries);
}
