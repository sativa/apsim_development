@echo off

set HERE=%CD%
set APSROOT=%CD%\..\..\..\..\

rem -------------------------------------------------------------------
rem Run APSIM on all simulations.
rem -------------------------------------------------------------------
%APSROOT%\bin\apsrun /auto YieldProphet.apsim

rem -------------------------------------------------------------------
rem Merge all outputs files into a single file.
rem -------------------------------------------------------------------
cd %APSROOT%\tools\MergeOutputFiles\bin\Debug
MergeOutputFiles %HERE%\*Yearly.out %HERE%\All.txt

rem -------------------------------------------------------------------
rem Clean up all unwanted files.
rem -------------------------------------------------------------------
cd %HERE%
del /Q *.sum
del /Q *.sim
del /Q *.out

rem -------------------------------------------------------------------
rem Rename All.txt to All.out. Couldn't do this earlier as it would
rem have been deleted.
rem -------------------------------------------------------------------
ren All.txt All.out

set HERE=
set APSROOT=
