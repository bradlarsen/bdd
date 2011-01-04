from instantiate import make_hash_table

make_hash_table(
    include_guard_name='BDD_RTU_INCLUDED',
    local_includes='#include "bddlib.h"\n#include "raw_bdd.h"\n#include "memory.h"',
    prefix='bdd_rtu_ht',
    hash_table_type='bdd_rtu_ht_t',
    key_type='raw_bdd_t',
    key_equal_func='raw_bdd_equal',
    key_hash_func='raw_bdd_hash',
    value_type='bdd_t *',
    header_name='bdd_rtu_ht.h',
    code_name='bdd_rtu_ht.c',
    malloc_func='checked_malloc',
    free_func='checked_free',
    )

make_hash_table(
    include_guard_name='BDD_HT_INCLUDED',
    local_includes='#include "bddlib.h"\n#include "raw_bdd.h"\n#include "memory.h"',
    prefix='bdd_ht',
    hash_table_type='bdd_ht_t',
    key_type='raw_bdd_t',
    key_equal_func='raw_bdd_equal',
    key_hash_func='raw_bdd_hash',
    value_type='raw_bdd_t',
    header_name='bdd_ht.h',
    code_name='bdd_ht.c',
    malloc_func='checked_malloc',
    free_func='checked_free',
    )

make_hash_table(
    include_guard_name='BDD_DOUBLE_HT_INCLUDED',
    local_includes='#include "bddlib.h"\n#include "raw_bdd.h"\n#include "memory.h"',
    prefix='bdd_double_ht',
    hash_table_type='bdd_double_ht_t',
    key_type='raw_bdd_t',
    key_equal_func='raw_bdd_equal',
    key_hash_func='raw_bdd_hash',
    value_type='double',
    header_name='bdd_double_ht.h',
    code_name='bdd_double_ht.c',
    malloc_func='checked_malloc',
    free_func='checked_free',
    )

make_hash_table(
    include_guard_name='USR_BDD_HT_INCLUDED',
    local_includes='#include "bddlib.h"\n#include "memory.h"\n#include "bdd.h"\n#include "usr_bdd_entry.h"',
    prefix='usr_bdd_ht',
    hash_table_type='usr_bdd_ht_t',
    key_type='bdd_t *',
    key_equal_func='bdd_ptr_equal',
    key_hash_func='bdd_ptr_hash',
    value_type='usr_bdd_entry_t',
    header_name='usr_bdd_ht.h',
    code_name='usr_bdd_ht.c',
    malloc_func='checked_malloc',
    free_func='checked_free',
    )
