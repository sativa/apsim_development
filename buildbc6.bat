@echo off
REM Force a build of all APSIM modules + infrastructure

del /S apsim\*.obj >NUL
del /S apsbuild\*.obj >NUL
del /S apsrun\*.obj >NUL
del /S shared\apsimshared\*.obj >NUL
del /S shared\general\*.obj >NUL
del /S shared\componentinterface\*.obj >NUL
del /S shared\protocol\*.obj >NUL

makebc6.bat
