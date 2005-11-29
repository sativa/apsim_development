@echo off
set APSROOT=c:\development

rem --------- Compile apsimui
cd %APSROOT%\apsimui\source
echo ------Compiling apsimui > %APSROOT%\dotnet.out
"C:\Program Files\Microsoft Visual Studio .NET 2003\Common7\IDE\devenv" apsimui.sln /rebuild debug >> %APSROOT%\dotnet.out
cd %APSROOT%

rem --------- Compile apsoil
cd %APSROOT%\apsoil\source
echo ------Compiling apsoil >> %APSROOT%\dotnet.out
"C:\Program Files\Microsoft Visual Studio .NET 2003\Common7\IDE\devenv" apsoil.sln /rebuild debug >> %APSROOT%\dotnet.out
cd %APSROOT%

rem --------- Compile GetComponentDescription
cd %APSROOT%\tools\GetComponentDescription
echo ------Compiling  GetComponentDescription >> %APSROOT%\dotnet.out
"C:\Program Files\Microsoft Visual Studio .NET 2003\Common7\IDE\devenv" GetComponentDescription.csproj /rebuild debug >> %APSROOT%\build.out
cd %APSROOT%

rem --------- Compile DotNetComponentInterface
cd %APSROOT%\Shared\DotNetComponentInterface
call datatypes.bat
echo ------Compiling  %APSROOT%\DotNetComponentInterface >> %APSROOT%\dotnet.out
"C:\Program Files\Microsoft Visual Studio .NET 2003\Common7\IDE\devenv" DotNetComponentInterface.vcproj /rebuild debug >> %APSROOT%\dotnet.out

rem --------- Compile SLURP Module
cd %APSROOT%\apsim\slurp\source
echo ------Compiling Slurp >> %APSROOT%\dotnet.out
"C:\Program Files\Microsoft Visual Studio .NET 2003\Common7\IDE\devenv" Slurp.vbproj /rebuild debug >> %APSROOT%\dotnet.out
cd %APSROOT%

rem --------- Compile ApsimToSim Module
cd %APSROOT%\apsrun\ApsimToSim
echo ------Compiling ApsimToSim >> %APSROOT%\dotnet.out
"C:\Program Files\Microsoft Visual Studio .NET 2003\Common7\IDE\devenv" ApsimToSim.csproj /rebuild debug >> %APSROOT%\dotnet.out
cd %APSROOT%