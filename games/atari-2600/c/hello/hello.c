#include <stdio.h>
#include <stdlib.h>

extern const char text[]; /* in text.s */

/*
cc65 -O -t c64 hello.c
ca65 hello.s
ca65 -t c64 text.s
ld65 -t c64 -o hello.a26 hello.o text.o c64.lib
*/

/*
Fast way
cl65 hello.c text.s -o hello.a26
*/

int main(void){
	printf("%s\n", text);
	while(1);
	return EXIT_SUCCESS;
}