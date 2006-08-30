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
if [ ! -d $APSROOT/apsim/ProtocolManager/lib ]; then mkdir $APSROOT/apsim/ProtocolManager/lib; fi
make -f Makefile.linux clobber
make -f Makefile.linux

echo ---------------------------------------------------
cd $APSROOT/apsim/clock/source
if [ ! -d $APSROOT/apsim/clock/lib ]; then mkdir $APSROOT/apsim/clock/lib; fi
make -f Makefile.linux clobber
make -f Makefile.linux

echo ---------------------------------------------------
cd $APSROOT/apsim/input/source
if [ ! -d $APSROOT/apsim/input/lib ]; then mkdir $APSROOT/apsim/input/lib; fi
make -f Makefile.linux clobber
make -f Makefile.linux

echo ---------------------------------------------------
cd $APSROOT/apsim/manager/source
if [ ! -d $APSROOT/apsim/manager/lib ]; then mkdir $APSROOT/apsim/manager/lib; fi
make -f Makefile.linux clobber
make -f Makefile.linux

echo ---------------------------------------------------
cd $APSROOT/apsim/report/source
if [ ! -d $APSROOT/apsim/report/lib ]; then mkdir $APSROOT/apsim/report/lib; fi
make -f Makefile.linux clobber
make -f Makefile.linux

echo ---------------------------------------------------
cd $APSROOT/apsim/soilwat2/source
if [ ! -d $APSROOT/apsim/soilwat2/lib ]; then mkdir $APSROOT/apsim/soilwat2/lib; fi
make -f Makefile.linux clobber
make -f Makefile.linux

echo ---------------------------------------------------
cd $APSROOT/apsim/soiln2/source
if [ ! -d $APSROOT/apsim/soiln2/lib ]; then mkdir $APSROOT/apsim/soiln2/lib; fi
make -f Makefile.linux clobber
make -f Makefile.linux

echo ---------------------------------------------------
cd $APSROOT/apsim/operatns/source
if [ ! -d $APSROOT/apsim/operatns/lib ]; then mkdir $APSROOT/apsim/operatns/lib; fi
make -f Makefile.linux clobber
make -f Makefile.linux

echo ---------------------------------------------------
cd $APSROOT/apsim/SurfaceOM/source
if [ ! -d $APSROOT/apsim/SurfaceOM/lib ]; then mkdir $APSROOT/apsim/SurfaceOM/lib; fi
make -f Makefile.linux clobber
make -f Makefile.linux

echo ---------------------------------------------------
cd $APSROOT/apsim/fertiliz/source
if [ ! -d $APSROOT/apsim/fertiliz/lib ]; then mkdir $APSROOT/apsim/fertiliz/lib; fi
make -f Makefile.linux clobber
make -f Makefile.linux

echo ---------------------------------------------------
cd $APSROOT/apsim/irrigate/source
if [ ! -d $APSROOT/apsim/irrigate/lib ]; then mkdir $APSROOT/apsim/irrigate/lib; fi
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
if [ ! -d $APSROOT/apsim/maize/lib ]; then mkdir $APSROOT/apsim/maize/lib; fi
make -f Makefile.linux clobber
make -f Makefile.linux

echo ---------------------------------------------------
cd $APSROOT/apsim/Tracker/Source
if [ ! -d $APSROOT/apsim/Tracker/lib ]; then mkdir $APSROOT/apsim/Tracker/lib; fi
make -f Makefile.linux clobber
make -f Makefile.linux
