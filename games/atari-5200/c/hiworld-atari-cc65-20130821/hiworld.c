/* hiworld.c */

#include <atari.h>
#include <stdio.h>
#include <unistd.h>

#define COLOR1 *(unsigned char *) 709
#define RTCLOK_lo *(unsigned char *) 20

int main(void) {
  int i;

  sleep(1);

  COLOR1 = 0;
  for (i = 0; i < 10; i++)
    printf("C - Hello [ATARI 2600]\n");

  for (i = 0; i < 32; i++) {
    COLOR1 = i << 1;
    RTCLOK_lo = 0; do {} while (RTCLOK_lo < 10);
    printf(".");
  }

  return(0);
}
