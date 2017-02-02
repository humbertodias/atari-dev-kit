# CC65_HOME=/usr/local/lib/cc65
FRANNY=/usr/bin/franny

all:	hiworld.atr

clean:
	-rm -f hiworld.atr
	-rm -f hiworld.xex
	-rm -f *.o

hiworld.xex:	hiworld.c
	cl65 -t atari hiworld.c text.s
	mv hiworld hiworld.xex

hiworld.atr:	hiworld.atr.in hiworld.xex
	cp hiworld.atr.in hiworld.atr
	${FRANNY} -A -i hiworld.xex -o HIWORLD.AR0 hiworld.atr

run-atr:	hiworld.atr
	atari800 -nobasic hiworld.atr

run-exe:	hiworld.xex
	atari800 -nobasic -run hiworld.xex
