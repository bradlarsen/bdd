/* Wrappers around memory management routines to do error checking: if
 * the wrapped functions fail, the program aborts. */

#ifndef MEMORY_INCLUDED
#define MEMORY_INCLUDED

#include <stdlib.h>

void *
checked_calloc (size_t nmemb, size_t size);

void *
checked_malloc (size_t size);

void
checked_free (void *ptr);

void *
checked_realloc (void *ptr, size_t size);

#endif /* MEMORY_INCLUDED */
