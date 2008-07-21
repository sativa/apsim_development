@echo off

set HERE=%CD%
set APSROOT=%CD%\..\..\..\..\

rem -------------------------------------------------------------------
rem Merge all outputs files into a single file.
rem -------------------------------------------------------------------
cd %APSROOT%\tools\MergeOutputFiles\bin\Release
MergeOutputFiles %HERE%

rem -------------------------------------------------------------------
rem Clean up all unwanted files.
rem -------------------------------------------------------------------
cd %HERE%
ren all.out all.txt
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
