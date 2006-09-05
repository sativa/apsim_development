@echo off
set APSROOT=%CD%

rem --------- Compile VBGeneral
echo. > %APSROOT%\dotnet.out
cd %APSROOT%\shared\vbgeneral
"C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" vbgeneral.vbproj /rebuild release >> %APSROOT%\dotnet.out

rem --------- Compile VBMet
echo. >> %APSROOT%\dotnet.out
cd %APSROOT%\shared\vbmet
"C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" vbmet.vbproj /rebuild release >> %APSROOT%\dotnet.out

rem --------- Compile CSGeneral
echo. >> %APSROOT%\dotnet.out
cd %APSROOT%\shared\c#general
"C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" csgeneral.csproj /rebuild release >> %APSROOT%\dotnet.out

rem --------- Compile ChangeTool
echo. >> %APSROOT%\dotnet.out
cd %APSROOT%\shared\ChangeTool
"C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" ChangeTool.csproj /rebuild release >> %APSROOT%\dotnet.out

rem --------- Compile ChangeTool
echo. >> %APSROOT%\dotnet.out
cd %APSROOT%\shared\ChangeTool
"C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" ChangeTool.csproj /rebuild release >> %APSROOT%\dotnet.out

rem --------- Compile Excel
echo. >> %APSROOT%\dotnet.out
cd %APSROOT%\shared\Excel
"C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" ExcelUtility.csproj /rebuild release >> %APSROOT%\dotnet.out

rem --------- Compile Soil
echo. >> %APSROOT%\dotnet.out
cd %APSROOT%\shared\Soil
"C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" Soil.csproj /rebuild release >> %APSROOT%\dotnet.out

rem --------- Compile DotNetComponentInterface
echo. >> %APSROOT%\dotnet.out
cd %APSROOT%\shared\DotNetComponentInterface
call datatypes.bat >> %APSROOT%\dotnet.out
"C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" DotNetComponentInterface.vcproj /rebuild debug >> %APSROOT%\dotnet.out

rem --------- Compile ApsimUI
echo. >> %APSROOT%\dotnet.out
cd %APSROOT%\ApsimUI\source
ren ApsimUI.sln ApsimUI.sl~
"C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" ApsimUI.vbproj /rebuild release >> %APSROOT%\dotnet.out
ren ApsimUI.sl~ ApsimUI.sln

rem --------- Compile ApsimUI
echo. >> %APSROOT%\dotnet.out
cd %APSROOT%\Apsoil\source
ren Apsoil.sln Apsoil.sl~
"C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" Apsoil.csproj /rebuild release >> %APSROOT%\dotnet.out
ren Apsoil.sl~ Apsoil.sln

rem --------- Compile GetComponentDescription
echo. >> %APSROOT%\dotnet.out
cd %APSROOT%\Tools\GetComponentDescription
"C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" GetComponentDescription.csproj /rebuild release >> %APSROOT%\dotnet.out

rem --------- Compile Slurp
echo. >> %APSROOT%\dotnet.out
cd %APSROOT%\Apsim\Slurp\Source
"C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" Slurp.vbproj /rebuild release >> %APSROOT%\dotnet.out

rem --------- Compile ApsimToSim
echo. >> %APSROOT%\dotnet.out
cd %APSROOT%\Apsrun\ApsimToSim
"C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" ApsimToSim.csproj /rebuild release >> %APSROOT%\dotnet.out


cd %APSROOT%
