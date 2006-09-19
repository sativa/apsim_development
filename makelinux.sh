#!/bin/sh

echo ---------------------------------------------------
export APSROOT=`pwd`
export LD_LIBRARY_PATH=$APSROOT/lib
ulimit -s unlimited

# Set these to compile with debug/whatever flags
#export CPPDEBUGFLAGS=-g
#export LDDEBUGFLAGS=-lg

# Optimised flags
export CPPDEBUGFLAGS=-O3
export LDDEBUGFLAGS=

# Profiling (gmon.out) flags
#export CPPDEBUGFLAGS=-pg
#export LDDEBUGFLAGS=-pg

# Clean up
rm -rf $APSROOT/lib
mkdir $APSROOT/lib

for dir in \
  $APSROOT/Shared/general \
  $APSROOT/Shared/ApsimShared \
  $APSROOT/APSBuild/source \
  $APSROOT/Shared/Protocol \
  $APSROOT/Shared/ComponentInterface \
  $APSROOT/apsim/infra/source \
  $APSROOT/apsim/infra/engine \
  $APSROOT/APSRun/ConToSim/Source \
 ;\
 do  \
  echo building $dir ;\
  cd $dir ;\
  make -f Makefile.linux clobber ;\
  make -f Makefile.linux install ;\
 done

for dir in \
  $APSROOT/apsim/ProtocolManager/Source \
  $APSROOT/apsim/accum/source \
  $APSROOT/apsim/canopy/source \
  $APSROOT/apsim/clock/source \
  $APSROOT/apsim/croptemp/source \
  $APSROOT/apsim/CropMod/source \
  $APSROOT/apsim/fertiliz/source \
  $APSROOT/apsim/input/source \
  $APSROOT/apsim/irrigate/source \
  $APSROOT/apsim/maize/source \
  $APSROOT/apsim/manager/source \
  $APSROOT/apsim/operatns/source \
  $APSROOT/apsim/ozcot/source \
  $APSROOT/apsim/Plant/source \
  $APSROOT/apsim/report/source \
  $APSROOT/apsim/soilwat2/source \
  $APSROOT/apsim/soiln2/source \
  $APSROOT/apsim/sorghum/source \
  $APSROOT/apsim/SummaryFile/Source \
  $APSROOT/apsim/SurfaceOM/source \
  $APSROOT/apsim/Tracker/Source \
  $APSROOT/apsim/tcllink/source \
 ;\
 do  \
  echo building $dir ;\
  cd $dir ;\
  if [ ! -d `dirname $dir`/lib ]; then mkdir `dirname $dir`/lib; fi ;\
  make -f Makefile.linux clobber ;\
  make -f Makefile.linux install ;\
 done
