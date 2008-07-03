@echo off
set HERE=%CD%
set APSROOT=%CD%\..\..\..\..\

rem --------------------------------------------------------------
rem Extract the necessary paddocks from PaddockDetails.xml and put
rem into YieldProphet.xml. The names to extract are listed in 
rem PaddockNames.txt
rem --------------------------------------------------------------
cd %APSROOT%\Tools\ExtractXMLElements\bin\Release\
ExtractXMLElements.exe %HERE%\PaddockDetails.xml %HERE%\PaddockNames.txt %HERE%\YieldProphet.xml

rem --------------------------------------------------------------
rem Turn the YieldProphet.xml file, created in the previous step,
rem into a YieldProphet.apsim file that is ready to run
rem --------------------------------------------------------------
cd %APSROOT%\Apsim\YieldProphet\ProcessYPDirectory\bin\Release
ProcessYPDirectory.exe %HERE% %HERE%\..\Template.apsim

rem --------------------------------------------------------------
rem Remove all references to SILO from the YieldProphet.apsim file
rem that was created in the previous step. NB the year on the
rem end of the next line.
rem --------------------------------------------------------------
cd %APSROOT%\Tools\ConvertSimsFromSILOToMetFile\bin\Release
del /Q %HERE%\*.met
ConvertSimsFromSILOToMetFile.exe %HERE%\YieldProphet.apsim 2007

set HERE=
set APSROOT=
pause
