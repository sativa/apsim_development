@echo off
set APSROOT=c:\development

rem --------- Compile All dot net stuff
cd %APSROOT%
echo ------Compiling  All dot net code > %APSROOT%\dotnet.out
"C:\Program Files\Microsoft Visual Studio .NET 2003\Common7\IDE\devenv" alldotnet.sln /rebuild debug >> %APSROOT%\dotnet.out
cd %APSROOT%
