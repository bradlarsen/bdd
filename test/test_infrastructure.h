/**********************************************************************/
/* TEST INFRASTRUCTURE                                                */
/**********************************************************************/

#ifndef TEST_INFRASTRUCTURE_INCLUDED
#define TEST_INFRASTRUCTURE_INCLUDED

#include <stdio.h>
#include <stdlib.h>

/* FIXME: these don't belong here */
static char *test_name = NULL;
static unsigned num_tests = 0;

#define ASSERT(cond)                                            \
    do {                                                        \
        if (!(cond)) {                                          \
            fprintf (stderr, "FAIL\n"                           \
                             "At %s:%u, the condition\n\n"      \
                             "\t%s\n\n"                         \
                             "failed.\n",                       \
                     __FILE__, __LINE__, #cond);                \
            exit(EXIT_FAILURE);                                 \
        }                                                       \
    } while (0)

#define TEST_NAME(str)                          \
    do {                                        \
        if (test_name != NULL)                  \
            fprintf (stderr, "PASS\n");         \
        fprintf (stderr, "%s: ", str);          \
        test_name = str;                        \
        num_tests += 1;                         \
    } while (0)

#endif /* TEST_INFRASTRUCTURE_INCLUDED */
