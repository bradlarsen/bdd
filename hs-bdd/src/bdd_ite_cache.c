#include "bdd_ite_cache.h"
#include "memory.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

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
    unsigned num_entries = up_to_next_power_of_two (num_entries_hint);

    tab->num_entries = num_entries;
    tab->entries = (bdd_ite_cache_entry_t *)
        checked_calloc (sizeof(bdd_ite_cache_entry_t), num_entries);
}

void
bdd_ite_cache_destroy (bdd_ite_cache_t *tab)
{
    checked_free (tab->entries);
}

void
bdd_ite_cache_clear (bdd_ite_cache_t *tab)
{
    memset (tab->entries, 0, tab->num_entries * sizeof (bdd_ite_cache_entry_t));
}
