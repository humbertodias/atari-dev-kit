#!/bin/sh
echo "Compiling $1"
dasm $1 -o$1.a26 -f3