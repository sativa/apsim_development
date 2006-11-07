pushd
cd \development\apsim\wheat\validation\files

\development\bin\apsrun.exe /auto  \development\apsim\wheat\validation\files\wheatvalidation.apsim  

c:\progra~1\R\rw2010\bin\Rterm.exe --slave < validation-1.r

\development\bin\apsimreport.exe aps06.report ..\aps6\aps06.gif
\development\bin\apsimreport.exe aps06summary.report ..\aps6\aps06summary.gif
\development\bin\apsimreport.exe wheatvalidation.report ..\wheatvalidation.gif


cd yieldprophet\2005
call runall.bat
\development\bin\apsimreport.exe YieldProphet2005.report ..\YieldProphet2005.gif

popd