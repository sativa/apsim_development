@echo off
set APSROOT=c:\mydata\development
set APSMAKE=c:\PROGRA~1\Borland\CBUILD~1\Bin\make.exe -s -f makefile.bc6

set SHARED=%APSROOT%\shared
rem --------- Make SEGMake first
cd %APSROOT%\Tools\segmake
echo ------Compiling  %APSROOT%\Tools\segmake > %APSROOT%\gui.out
cmd /c %APSMAKE%  >> %APSROOT%\gui.out

rem --------- Go build all gui stuff.
cd %APSROOT%
%APSROOT%\tools\segmake\segmake %APSROOT%\all.bpg