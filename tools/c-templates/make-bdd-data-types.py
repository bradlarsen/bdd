from instantiate import make_vector, make_hash_table

make_vector(
    include_guard_name='NODE_VECTOR_INCLUDED',
    local_includes='#include "node.h"',
    prefix='node_vector',
    vector_type='node_vector_t',
    element_type='node_t',
    header_name='node_vector.h',
    code_name='node_vector.c'
    )

make_hash_table(
    include_guard_name='NODE_HASH_TABLE_INCLUDED',
    local_includes='#include "node.h"\n#include "bdd.h"',
    prefix='node_hash_table',
    hash_table_type='node_hash_table_t',
    key_type='node_t',
    key_equal_func='node_equal',
    key_hash_func='node_hash',
    value_type='bdd',
    header_name='node_hash_table.h',
    code_name='node_hash_table.c'
    )
