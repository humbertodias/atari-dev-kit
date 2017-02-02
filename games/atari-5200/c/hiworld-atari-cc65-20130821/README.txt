README.txt for hiworld, "Hello world" in C, for Atari 8-bit
Bill Kendrick <bill@newbreedsoftware.com>
August 2013

= About =
A hello world for Atari 8-bits, written in C, cross-compiled with cc65, and
assembled into a bootable disk image with Franny.

= Files =
 * hiworld.c
   Source code

 * Makefile
   Makefile, with targets:
    - all: hiworld.atr
    - clean
    - hiworld.xex: Executable
    - hiworld.atr: Bootable disk image, with executable auto-loaded
    - run-exe: Launches the executable in Atari800 emulator (via -run option)
    - run-atr: Boots Atari800 emulator into the disk image

 * hiworld.atr.in
   A mostly-blank disk image, created first by Franny (see below) with:
     $ franny -C -d s -f a hiatari.atr.in
     (Create, density: single, format: Atari DOS)
   (Alternatively, use Atari800 emulator: [F1] -> Disk Management ->
   Make Blank ATR Disk)

   Then booted Atari800 emulator (see below) with a MyDOS 4.53/4 boot disk
   and the new .atr in D2: and issued:
     [O] Change config,
     Drive number: 2,
     Remove drive? N
     Is drive configurable? N
   to ensure drive D2: was seen as Single Density.

   Then issued:
     [I] Initialize disk
     Disk to format: 2
     ...Type [Y] to Format Drive 2: Y  (for single-density)

   Then:
     [H] Write DOS Files
     Drive to write DOS files to? 2

   Finally, booted Atari800 with the new, now-bootable, .atr in D2: and
   issued:
     [O] Change config
     Drive number: 1
     Remove drive? N
     Is drive configurable? N
   to ensure dirve D1: was seen as Single Density.

   And re-wrote DOS files:
     [H] Write DOS Files
     Drive to write DOS files to? 1

= Requirements =
 * cc65 C cross compiler for 6502-based targets:
   http://oliverschmidt.github.io/cc65/
   (Old site, prior to March 2013, was http://www.cc65.org/ )
   (Probably available in your distro)

= Optional Requirements =
 * Franny Atari disk image manipulator
   http://atari8.sourceforge.net/franny.html
   Part of "atari8" project at SourceForge: http://atari8.sourceforge.net/
   Files here: http://sourceforge.net/projects/atari8/files/

 * Atari800 Atari emulator
   http://atari800.sourceforge.net/
   (Probably available in your distro)

= Other Sources =
 * MyDOS, which can be found at various places, e.g.
   http://www.mathyvannisselroy.nl/mydos.htm
   http://www.umich.edu/~archive/atari/8bit/Dos/Mydos/ (and mirrors)

 * "Mapping the Atari" Memory Map
   http://www.atariarchives.org/mapping/memorymap.php

 * Atari specific information for cc65
   http://www.cc65.org/doc/atari.html

