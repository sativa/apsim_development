@echo off
set APSROOT=c:\development

rem --------- Compile VBGeneral
cd %APSROOT%\Shared\VBGeneral
echo ------Compiling  %APSROOT%\Shared\VBGeneral > %APSROOT%\dotnet.out
"C:\Program Files\Microsoft Visual Studio .NET 2003\Common7\IDE\devenv" VBGeneral.vbproj /rebuild debug >> %APSROOT%\dotnet.out
cd %APSROOT%

rem --------- Compile C#General
cd %APSROOT%\Shared\C#general
echo ------Compiling  %APSROOT%\Shared\C#general >> %APSROOT%\dotnet.out
"C:\Program Files\Microsoft Visual Studio .NET 2003\Common7\IDE\devenv" CSGeneral.csproj /rebuild debug >> %APSROOT%\dotnet.out
cd %APSROOT%

rem --------- Compile VBMet
cd %APSROOT%\Shared\VBMet
echo ------Compiling  %APSROOT%\Shared\VBMet >> %APSROOT%\dotnet.out
"C:\Program Files\Microsoft Visual Studio .NET 2003\Common7\IDE\devenv" VBMet.vbproj /rebuild debug >> %APSROOT%\dotnet.out
cd %APSROOT%

rem --------- Compile ApsimUI
cd %APSROOT%\ApsimUI\source
echo ------Compiling  %APSROOT%\ApsimUI >> %APSROOT%\dotnet.out
"C:\Program Files\Microsoft Visual Studio .NET 2003\Common7\IDE\devenv" ApsimUI.vbproj /rebuild debug >> %APSROOT%\dotnet.out
cd %APSROOT%

rem --------- Compile GetComponentDescription
cd %APSROOT%\Tools\GetComponentDescription
echo ------Compiling  %APSROOT%\Tools\GetComponentDescription >> %APSROOT%\dotnet.out
"C:\Program Files\Microsoft Visual Studio .NET 2003\Common7\IDE\devenv" GetComponentDescription.csproj /rebuild debug >> %APSROOT%\dotnet.out
cd %APSROOT%
