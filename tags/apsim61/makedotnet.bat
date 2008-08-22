@echo off
set APSROOT=%CD%

rem --------- Compile apsimui
cd %APSROOT%
echo ------Compiling DotNet source > %APSROOT%\dotnet.out
"C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" AllDotNet.sln /build release >> %APSROOT%\dotnet.out

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

rem --------- Create the ComponentDescription.xml
call %APSROOT%\ApsimUI\CreateComponentDescription.bat

rem --------- Compile YieldProphet\ProcessYPDirectory
cd %APSROOT%\apsim\yieldprophet\ProcessYPDirectory
echo ------Compiling YieldProphet\ProcessYPDirectory >> %APSROOT%\dotnet.out
"C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" ProcessYPDirectory.sln /build release >> %APSROOT%\dotnet.out
cd %APSROOT%

rem --------- Compile Tools
cd %APSROOT%\tools
echo ------Compiling Tools >> %APSROOT%\dotnet.out
"C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" Tools.sln /build release >> %APSROOT%\dotnet.out
cd %APSROOT%
