#ifndef USR_BDD_ENTRY_INCLUDED
#define USR_BDD_ENTRY_INCLUDED

#include "raw_bdd.h"

typedef struct
{
    raw_bdd_t raw_bdd;
    unsigned ref_cnt;
} usr_bdd_entry_t;

#endif /* USR_BDD_ENTRY_INCLUDED */
