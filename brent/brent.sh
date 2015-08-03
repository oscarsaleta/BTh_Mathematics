#!/bin/bash
#
cp brent.h /$HOME/include
#
gcc -c brent.c
if [ $? -ne 0 ]; then
  echo "Errors compiling brent.c."
  exit
fi
#
mv brent.o ~/libc/$ARCH/brent.o
#
echo "Library installed as ~/libc/$ARCH/brent.o"
