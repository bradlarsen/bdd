#include "memory.h"
#include <stdio.h>

void *
checked_calloc (size_t nmemb, size_t size)
{
    void *res = calloc (nmemb, size);
    if (res == NULL) {
        fprintf (stderr, "checked_calloc: allocation failed\n");
        exit (EXIT_FAILURE);
    }
    return res;
}

void *
checked_malloc (size_t size)
{
    void *res = malloc (size);
    if (res == NULL) {
        fprintf (stderr, "checked_malloc: allocation failed\n");
        exit (EXIT_FAILURE);
    }
    return res;
}

void
checked_free (void *ptr)
{
    free (ptr);
}

void *
checked_realloc (void *ptr, size_t size)
{
    void *res = realloc (ptr, size);
    if (res == NULL) {
        fprintf (stderr, "checked_realloc: allocation failed\n");
        exit (EXIT_FAILURE);
    }
    return res;
}
