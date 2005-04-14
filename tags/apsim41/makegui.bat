@echo off
set APSROOT=c:\development
set APSMAKE=c:\PROGRA~1\Borland\CBUILD~1\Bin\make.exe -s -f makefile.bc6

set SHARED=%APSROOT%\shared
rem --------- Make SEGMake first
cd %APSROOT%\Tools\segmake
echo ------Compiling  %APSROOT%\Tools\segmake > %APSROOT%\gui.out
cmd /c %APSMAKE%  >> %APSROOT%\gui.out

rem --------- Go build all gui stuff.
cd %APSROOT%
%APSROOT%\tools\segmake\segmake %APSROOT%\all.bpg

rem --------- Build old APSFront
echo ------Compiling  %APSROOT%\APSFront\source >> %APSROOT%\gui.out
cd %APSROOT%
path > p.bat
path=c:\bc5\bin
cd %APSROOT%\apsfront\source
make -s -f apsfront.mak  >> %APSROOT%\gui.out

rem --------- Build old HOWWET
echo ------Compiling  %APSROOT%\Howwet\source >> %APSROOT%\gui.out
cd %APSROOT%\howwet\source
make -s -f howwet.mak >> %APSROOT%\gui.out

rem --------- Restore path.
cd %APSROOT%
call p.bat
del p.bat
