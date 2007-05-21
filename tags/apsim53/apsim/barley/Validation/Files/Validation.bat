pushd
cd \development\apsim\barley\validation\files

\development\bin\apsrun.exe /auto  \development\apsim\barley\validation\files\barley_validation.apsim  
c:\progra~1\R\R-2.4.0\bin\Rterm.exe --slave < validation-1.r

\development\bin\apsimreport.exe HB90Sow1.report         ../HB90Sow1.gif
\development\bin\apsimreport.exe HB90Sow2.report         ../HB90Sow2.gif
\development\bin\apsimreport.exe HB90Sow3.report         ../HB90Sow3.gif
\development\bin\apsimreport.exe HRS87-2MP.report        ../HRS87-2MP.gif
\development\bin\apsimreport.exe HRS87-3MP.report        ../HRS87-3MP.gif
\development\bin\apsimreport.exe HRSRO-DD.report         ../HRSRO-DD.gif
\development\bin\apsimreport.exe HRSRO-DI.report         ../HRSRO-DI.gif
\development\bin\apsimreport.exe HRSRO-II.report         ../HRSRO-II.gif
\development\bin\apsimreport.exe KthorpeRain05_5.report  ../KthorpeRain05_5.gif
\development\bin\apsimreport.exe Roma88-Sow1HP.report    ../Roma88Sow1HP.gif
\development\bin\apsimreport.exe Roma88-Sow1MP.report    ../Roma88Sow1MP.gif
\development\bin\apsimreport.exe Roma88-Sow2HP.report    ../Roma88Sow2HP.gif
\development\bin\apsimreport.exe Roma88-Sow2MP.report    ../Roma88Sow2MP.gif
\development\bin\apsimreport.exe Roma88-Sow3HP.report    ../Roma88Sow3HP.gif
\development\bin\apsimreport.exe Roma88-Sow3MP.report    ../Roma88Sow3MP.gif
\development\bin\apsimreport.exe Roma88-Sow4HP.report    ../Roma88Sow4HP.gif
\development\bin\apsimreport.exe Roma88-Sow4MP.report    ../Roma88Sow4MP.gif
\development\bin\apsimreport.exe Wellcamp93.report       ../wellcamp93.gif

\development\bin\apsimreport.exe barley_validation.report ../Barley_validation.gif

rem cd yieldprophet\2005
rem call runall.bat
rem \development\bin\apsimreport.exe YieldProphet2005.report ..\..\..\YieldProphet2005.gif

popd
