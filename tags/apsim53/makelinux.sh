#!/bin/sh

export APSROOT=`pwd`

#export APSROOT=/psfs/enclass2/DevelopmentLinux/APSIM/
#export BOOST_INCLUDEDIR-I/usr/local/include/boost-1_33_1
#export XML2_INCLUDEDIR=-I/usr/local/libxml2-2.6.23/include/libxml2
#export XML2_LIBDIR=-L/usr/local/libxml2-2.6.23/lib

export LD_LIBRARY_PATH=$APSROOT/lib
ulimit -s unlimited

make -f Makefile.linux clobber 
make -f Makefile.linux install 
