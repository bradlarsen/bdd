#include "bdd_ite_cache.h"
#include "memory.h"

#include <assert.h>
#include <stdlib.h>
#include <limits.h>

static unsigned
up_to_next_power_of_two (unsigned n)
{
    unsigned i;
    for (i = 1; i <= n; i *= 2) {}
    return i;
}

void
bdd_ite_cache_create_with_hint (bdd_ite_cache_t *tab, unsigned num_entries_hint)
{
    unsigned i;
    const unsigned num_entries = up_to_next_power_of_two (num_entries_hint);
    const bdd_ite_cache_entry_t sentinel =
        { INT_MAX, INT_MAX, INT_MAX, bdd_false };

    tab->num_entries = num_entries;
    tab->entries = (bdd_ite_cache_entry_t *)
        checked_malloc (sizeof(bdd_ite_cache_entry_t) * num_entries);

    for (i = 0; i < num_entries; i += 1) {
        /* FIXME: this sentinel is bogus */
        tab->entries[i] = sentinel;
    }
}

/* Frees the memory used by the given hash table.  It is an error
 * to call this procedure more than once on a hash table. */
void
bdd_ite_cache_destroy (bdd_ite_cache_t *tab)
{
    checked_free (tab->entries);
}
