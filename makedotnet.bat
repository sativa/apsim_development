@echo off
set APSROOT=c:\development

rem --------- Compile ApsimUI
cd %APSROOT%\ApsimUI\source
echo ------Compiling  %APSROOT%\ApsimUI > %APSROOT%\dotnet.out
"C:\Program Files\Microsoft Visual Studio .NET 2003\Common7\IDE\devenv" ApsimUI.sln /rebuild debug >> %APSROOT%\dotnet.out
cd %APSROOT%
