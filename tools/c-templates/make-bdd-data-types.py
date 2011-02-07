from instantiate import make_hash_table

make_hash_table(
    include_guard_name='NODE_HT_INCLUDED',
    local_includes='#include "bdd_mgr.h"\n#include "memory.h"\n',
    prefix='node_ht',
    hash_table_type='node_ht_t',
    key_type='node_t',
    key_equal_func='node_equal',
    key_hash_func='node_hash',
    value_type='unsigned',
    header_name='node_ht.h',
    code_name='node_ht.c',
    malloc_func='checked_malloc',
    free_func='checked_free',
    )
