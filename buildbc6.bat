@echo off
REM Force a build of all APSIM modules + infrastructure

del /S bin\vcl60.* >NUL
del /S .\*.obj >NUL

makebc6.bat
