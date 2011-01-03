from instantiate import make_hash_table

make_hash_table(
    include_guard_name='BDD_HT_INCLUDED',
    local_includes='#include "bdd.h"\n#include "memory.h"',
    prefix='bdd_ht',
    hash_table_type='bdd_ht_t',
    key_type='bdd_t',
    key_equal_func='bdd_equal',
    key_hash_func='bdd_hash',
    value_type='bdd_t',
    header_name='bdd_ht.h',
    code_name='bdd_ht.c',
    malloc_func='checked_malloc',
    free_func='checked_free',
    )

make_hash_table(
    include_guard_name='BDD_DOUBLE_HT_INCLUDED',
    local_includes='#include "bdd.h"\n#include "memory.h"',
    prefix='bdd_double_ht',
    hash_table_type='bdd_double_ht_t',
    key_type='bdd_t',
    key_equal_func='bdd_equal',
    key_hash_func='bdd_hash',
    value_type='double',
    header_name='bdd_double_ht.h',
    code_name='bdd_double_ht.c',
    malloc_func='checked_malloc',
    free_func='checked_free',
    )
