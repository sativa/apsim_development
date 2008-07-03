@echo off
set HERE=%CD%
set APSROOT=%CD%\..\..\..\..\

rem --------------------------------------------------------------
rem Extract the necessary paddocks from PaddockDetails.xml and put
rem into YieldProphet.xml. The names to extract are listed in 
rem PaddockNames.txt
rem --------------------------------------------------------------
cd %APSROOT%\Tools\ExtractXMLElements\bin\Debug\
ExtractXMLElements.exe %HERE%\PaddockDetails.xml %HERE%\PaddockNames.txt %HERE%\YieldProphet.xml

rem --------------------------------------------------------------
rem Turn the YieldProphet.xml file, created in the previous step,
rem into a YieldProphet.apsim file that is ready to run
rem --------------------------------------------------------------
cd c:\hol353\ApsimWork\YieldProphet\ProcessYPDirectory\bin\Debug
ProcessYPDirectory.exe %HERE%

rem --------------------------------------------------------------
rem Remove all references to SILO from the YieldProphet.apsim file
rem that was created in the previous step. NB the year on the
rem end of the next line.
rem --------------------------------------------------------------
cd %APSROOT%\Tools\ConvertSimsFromSILOToMetFile\bin\Debug
del /Q %HERE%\*.met
ConvertSimsFromSILOToMetFile.exe %HERE%\YieldProphet.apsim 2004

set HERE=
set APSROOT=
pause
