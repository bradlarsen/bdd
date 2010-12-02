#include "node_hash_table_impl.h"

extern unsigned
hash_pair (unsigned i, unsigned j);

extern unsigned
hash_node (node_t node);

extern unsigned
get_hash_index (node_hash_table_t *tab, node_t key);
