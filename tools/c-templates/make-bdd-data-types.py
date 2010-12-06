from instantiate import make_vector, make_hash_table

make_vector(
    include_guard_name='NODE_VEC_INCLUDED',
    local_includes='#include "node.h"',
    prefix='node_vec',
    vector_type='node_vec_t',
    element_type='node_t',
    header_name='node_vec.h',
    code_name='node_vec.c'
    )

make_hash_table(
    include_guard_name='NODE_HT_INCLUDED',
    local_includes='#include "node.h"',
    prefix='node_ht',
    hash_table_type='node_ht_t',
    key_type='node_t',
    key_equal_func='node_equal',
    key_hash_func='node_hash',
    value_type='bdd_t',
    header_name='node_ht.h',
    code_name='node_ht.c'
    )

make_hash_table(
    include_guard_name='BDD_PAIR_HT_INCLUDED',
    local_includes='#include "bdd_pair.h"',
    prefix='bdd_pair_ht',
    hash_table_type='bdd_pair_ht_t',
    key_type='bdd_pair_t',
    key_equal_func='bdd_pair_equal',
    key_hash_func='bdd_pair_hash',
    value_type='bdd_t',
    header_name='bdd_pair_ht.h',
    code_name='bdd_pair_ht.c'
    )
