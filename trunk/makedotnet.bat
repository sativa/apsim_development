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
