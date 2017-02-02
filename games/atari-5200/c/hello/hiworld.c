#include <stdio.h>
#include <stdlib.h>

extern const char text[];       /* In text.s */

/**
http://www.cc65.org/snapshot-doc/intro-6.html#ss6.2
**/

int main (void)
{
    printf ("%s\n", text);
    while(1);
    return EXIT_SUCCESS;
}