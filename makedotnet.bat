@echo off
set APSROOT=%CD%

rem --------- Compile apsimui
cd %APSROOT%\apsimui\source
echo ------Compiling apsimui > %APSROOT%\dotnet.out
"C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" apsimui.sln /build release >> %APSROOT%\dotnet.out
cd %APSROOT%

rem --------- Compile apsoil
cd %APSROOT%\apsoil\source
echo ------Compiling apsoil >> %APSROOT%\dotnet.out
"C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" apsoil.sln /build release >> %APSROOT%\dotnet.out
cd %APSROOT%

rem --------- Compile ApsimReportData
cd %APSROOT%\apsimreport\ApsimReportData
echo ------Compiling ApsimReportData >> %APSROOT%\dotnet.out
"C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" ApsimReportData.sln /build release >> %APSROOT%\dotnet.out
cd %APSROOT%

rem --------- Compile Tools\CallManagedDLL
cd %APSROOT%\Tools\CallManagedDLL
echo ------Compiling Tools\CallManagedDLL >> %APSROOT%\dotnet.out
"C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" CallManagedDLL.sln /build release >> %APSROOT%\dotnet.out
cd %APSROOT%

rem --------- Compile GetComponentDescription
cd %APSROOT%\tools\GetComponentDescription
echo ------Compiling  GetComponentDescription >> %APSROOT%\dotnet.out
"C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" GetComponentDescription.sln /build release >> %APSROOT%\build.out
cd %APSROOT%

rem --------- Compile DotNetComponentInterface
cd %APSROOT%\Shared\DotNetComponentInterface
echo ------Compiling  %APSROOT%\DotNetComponentInterface >> %APSROOT%\dotnet.out
call datatypes.bat >> %APSROOT%\dotnet.out
"C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" DotNetComponentInterface.sln /build release >> %APSROOT%\dotnet.out

rem --------- Compile SLURP Module
cd %APSROOT%\apsim\slurp\source
echo ------Compiling Slurp >> %APSROOT%\dotnet.out
"C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" Slurp.sln /build release >> %APSROOT%\dotnet.out
cd %APSROOT%

rem --------- Compile ApsimToSim Module
cd %APSROOT%\apsrun\ApsimToSim
echo ------Compiling ApsimToSim >> %APSROOT%\dotnet.out
"C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" ApsimToSim.sln /build release >> %APSROOT%\dotnet.out
cd %APSROOT%

rem --------- Compile Howwetv2
cd %APSROOT%\howwetv2\source
echo ------Compiling Howwetv2 >> %APSROOT%\dotnet.out
"C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" Howwetv2.sln /build release >> %APSROOT%\dotnet.out
cd %APSROOT%