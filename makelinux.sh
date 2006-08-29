#!/bin/sh

echo ---------------------------------------------------
export APSROOT=`pwd`
export LD_LIBRARY_PATH=$APSROOT/lib
ulimit -s unlimited

rm -rf $APSROOT/lib
mkdir $APSROOT/lib

echo ---------------------------------------------------
cd $APSROOT/Shared/general
make -f Makefile.linux clobber
make -f Makefile.linux install

echo ---------------------------------------------------
cd $APSROOT/Shared/ApsimShared
make -f Makefile.linux clobber
make -f Makefile.linux install

echo ---------------------------------------------------
cd $APSROOT/APSBuild/source
make -f Makefile.linux clobber
make -f Makefile.linux install

echo ---------------------------------------------------
cd $APSROOT/Shared/Protocol
make -f Makefile.linux clobber
make -f Makefile.linux install

echo ---------------------------------------------------
cd $APSROOT/Shared/ComponentInterface
make -f Makefile.linux clobber
make -f Makefile.linux install

echo ---------------------------------------------------
cd $APSROOT/apsim/infra/source
make -f Makefile.linux clobber
make -f Makefile.linux

echo ---------------------------------------------------
cd $APSROOT/apsim/infra/engine
make -f Makefile.linux clobber
make -f Makefile.linux install

echo ---------------------------------------------------
cd $APSROOT/apsim/ProtocolManager/Source
make -f Makefile.linux clobber
make -f Makefile.linux

echo ---------------------------------------------------
cd $APSROOT/apsim/clock/source
make -f Makefile.linux clobber
make -f Makefile.linux

echo ---------------------------------------------------
cd $APSROOT/apsim/input/source
make -f Makefile.linux clobber
make -f Makefile.linux

echo ---------------------------------------------------
cd $APSROOT/apsim/manager/source
make -f Makefile.linux clobber
make -f Makefile.linux

echo ---------------------------------------------------
cd $APSROOT/apsim/report/source
make -f Makefile.linux clobber
make -f Makefile.linux

echo ---------------------------------------------------
cd $APSROOT/apsim/soilwat2/source
make -f Makefile.linux clobber
make -f Makefile.linux

echo ---------------------------------------------------
cd $APSROOT/apsim/soiln2/source
make -f Makefile.linux clobber
make -f Makefile.linux

echo ---------------------------------------------------
cd $APSROOT/apsim/operatns/source
make -f Makefile.linux clobber
make -f Makefile.linux

echo ---------------------------------------------------
cd $APSROOT/apsim/SurfaceOM/source
make -f Makefile.linux clobber
make -f Makefile.linux

echo ---------------------------------------------------
cd $APSROOT/apsim/fertiliz/source
make -f Makefile.linux clobber
make -f Makefile.linux

echo ---------------------------------------------------
cd $APSROOT/apsim/irrigate/source
make -f Makefile.linux clobber
make -f Makefile.linux

echo ---------------------------------------------------
cd $APSROOT/apsim/croptemp/source
make -f Makefile.linux clobber
make -f Makefile.linux

echo ---------------------------------------------------
cd $APSROOT/apsim/CropMod/source
make -f Makefile.linux clobber
make -f Makefile.linux

echo ---------------------------------------------------
cd $APSROOT/apsim/maize/source
make -f Makefile.linux clobber
make -f Makefile.linux

echo ---------------------------------------------------
cd $APSROOT/apsim/tracker/source
make -f Makefile.linux clobber
make -f Makefile.linux
