#ifndef BDD_TRIPLE_INCLUDED
#define BDD_TRIPLE_INCLUDED

#include <stdint.h>

// A reduced, ordered BDD represents a Boolean function as a rooted,
// directed, acyclic graph.  (Unless stated otherwise, when we say
// `BDD' we mean `reduced, ordered BDD'.)  For a Boolean function f,
// the corresponding BDD has the following properties:
//
//   * There exists a total order `<' on the variables of f.
//   * Each non-terminal node has a label corresponding to a variable.
//   * There is one root node.
//   * There are at most two terminal nodes, denoted T and F.
//   * Each non-terminal node has two children: the low child and the
//     high child.
//   * For each node n and m, if n is a predecessor of m, then the
//     label of n is less than the label of m.
//   * There exists no node with a single node as both its low and
//     high child.
//   * Structural equality of nodes n and m implies that n and m are
//     the same node.

struct
bdd_triple
{
    // The node's label.  0 is reserved for the F terminal, 1 for the
    // T terminal.
    uint32_t variable;

    // The index of the low child.
    uint32_t low_idx;

    // The index of the high child.
    uint32_t high_idx;
};

#endif  /* BDD_TRIPLE_VECTOR_INCLUDED */
