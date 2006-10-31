#!/bin/sh

export APSROOT=`pwd`
export LD_LIBRARY_PATH=$APSROOT/lib
ulimit -s unlimited

make -f Makefile.linux clobber 
make -f Makefile.linux install 

