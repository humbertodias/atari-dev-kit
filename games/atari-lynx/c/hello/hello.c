#include <lynx.h>
#include <tgi.h>
#include <6502.h> 

/*
http://www.cc65.org/snapshot-doc/lynx-2.html
*/

void main(void) {
  tgi_install(tgi_static_stddrv);
  tgi_init();
  CLI();
  while (tgi_busy())
    ;
  tgi_clear();
  tgi_setcolor(COLOR_GREEN);
  tgi_outtextxy(0, 0, "Hello World");
  tgi_updatedisplay();
  while (1)
    ;
}