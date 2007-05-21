pushd
cd \development\apsim\wheat\validation\files

\development\bin\apsrun.exe /auto  \development\apsim\wheat\validation\files\wheatvalidation.apsim  

c:\progra~1\R\R-2.4.0\bin\Rterm.exe --slave < validation-1.r

mkdir ..\aps6
\development\bin\apsimreport.exe aps06.report ..\aps6\aps06.gif
\development\bin\apsimreport.exe aps06summary.report ..\aps6\aps06summary.gif

rem mkdir ..\aps14
rem \development\bin\apsimreport.exe aps14summary.report ..\aps14\aps14summary.gif

\development\bin\apsimreport.exe wheatvalidation.report ..\wheatvalidation.gif
rem mkdir ..\lincoln9192
rem \development\bin\apsimreport.exe lincoln9192.report ..\lincoln9192\lincoln9192.gif
rem \development\bin\apsimreport.exe lincoln9192Summary.report ..\lincoln9192\lincoln9192Summary.gif

rem mkdir ..\cunderdin
rem \development\bin\apsimreport.exe cunderdin.report ..\cunderdin\cunderdin.gif

mkdir ..\mer-73
\development\bin\apsimreport.exe merredin73.report ..\mer-73\merredin73.gif

cd yieldprophet\2005
call runall.bat
\development\bin\apsimreport.exe YieldProphet2005.report ..\..\..\YieldProphet2005.gif

popd