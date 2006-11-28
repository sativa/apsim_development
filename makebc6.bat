@echo off
REM Set up the make system for borland CBuilder6/Lahey F95 compiler suite
set APSROOT=%CD%
set APSMAKE=c:\PROGRA~1\Borland\CBUILD~1\Bin\make.exe -s -f makefile.bc6

copy /y %APSROOT%\APSBuild\ApsimComponentInterface.make.bc6 %APSROOT%\APSBuild\ApsimComponentInterface.make
copy /y %APSROOT%\APSBuild\fortran.make.bc6 %APSROOT%\APSBuild\fortran.make
copy /y %APSROOT%\APSBuild\platform.make.bc6 %APSROOT%\APSBuild\platform.make

REM -------Build the general and shared dlls first
cd %APSROOT%\Shared\general
echo ------Compiling  %APSROOT%\Shared\general > %APSROOT%\build.out
cmd /c %APSMAKE%  >> %APSROOT%\build.out

cd %APSROOT%\Tools\def2imp
echo ------Compiling  %APSROOT%\Tools\def2imp >> %APSROOT%\build.out
cmd /c %APSMAKE%  >> %APSROOT%\build.out

cd %APSROOT%\Shared\ApsimShared
echo ------Compiling  %APSROOT%\Shared\ApsimShared >> %APSROOT%\build.out
cmd /c %APSMAKE%  >> %APSROOT%\build.out

REM Now tools to make datatypes (needed in infrastructure build)
cd %APSROOT%\APSBuild\source
echo ------Compiling  %APSROOT%\APSBuild\source >> %APSROOT%\build.out
cmd /c %APSMAKE%  >> %APSROOT%\build.out

REM -------Compile ProcessDataTypesInterface - needed to create ComponentInterface2/datatypes.h
cd %APSROOT%\Shared\ComponentInterface2\ProcessDataTypesInterface
echo ------Compiling ProcessDataTypesInterface >> %APSROOT%\build.out
"C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" ProcessDataTypesInterface.sln /build release >> %APSROOT%\build.out

REM -------Compile RunMacro - needed to create ComponentInterface2/datatypes.h
cd %APSROOT%\tools\runmacro
echo ------Compiling RunMacro >> %APSROOT%\build.out
"C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" RunMacro.sln /build release >> %APSROOT%\build.out

REM -------Go create ComponentInterface2/datatypes.h
cd %APSROOT%\Shared\ComponentInterface2
call DataTypes.bat

REM -------Protocol
cd %APSROOT%\Shared\Protocol
echo ------Compiling  %APSROOT%\Shared\Protocol >> %APSROOT%\build.out
cmd /c %APSMAKE%  >> %APSROOT%\build.out

REM  ---- Has side effect of building datatypes.[cpp,h,f90] etc here
cd %APSROOT%\Shared\ComponentInterface
echo ------Compiling  %APSROOT%\Shared\ComponentInterface >> %APSROOT%\build.out
cmd /c %APSMAKE%  >> %APSROOT%\build.out
REM \tools\segmake ComponentInterface.bpg
REM %APSROOT%\Tools\def2imp\def2imp.exe ApsimFortranWrapper.def ApsimFortranWrapper.imp

REM  ---- ComponentInterface2
cd %APSROOT%\Shared\ComponentInterface2
echo ------Compiling  %APSROOT%\Shared\ComponentInterface2 >> %APSROOT%\build.out
cmd /c %APSMAKE%  >> %APSROOT%\build.out

cd %APSROOT%\apsim\ProtocolManager\Source
echo ------Compiling  %APSROOT%\apsim\ProtocolManager\Source >> %APSROOT%\build.out
cmd /c %APSMAKE%  >> %APSROOT%\build.out

REM -------Build apsrun
cd %APSROOT%\apsrun\source
echo ------Compiling  %APSROOT%\apsrun\source >> %APSROOT%\build.out
cmd /c %APSMAKE%  >> %APSROOT%\build.out

REM -------Build apsrun contosim
cd %APSROOT%\apsrun\ConToSim
echo ------Compiling  %APSROOT%\apsrun\contosim >> %APSROOT%\build.out
cmd /c c:\PROGRA~1\Borland\CBUILD~1\Bin\make.exe -s -f makefile.bc6  >> %APSROOT%\build.out

REM -------Build TestAPSRun
cd %APSROOT%\apsrun\source\test
echo ------Compiling  %APSROOT%\apsrun\source\test\TestAPSRun >> %APSROOT%\build.out
cmd /c c:\PROGRA~1\Borland\CBUILD~1\Bin\make.exe -s -f TestAPSRun.mak  >> %APSROOT%\build.out

REM -------Everything else..
cd %APSROOT%\apsim
echo ------Compiling  %APSROOT%\apsim >> %APSROOT%\build.out
call makefile.bat >> %APSROOT%\build.out

cd %APSROOT%

REM -------All Done.
cd ..
