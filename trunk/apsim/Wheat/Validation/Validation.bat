pushd
cd \development\apsim\wheat\validation

rem no longer use rems to recreate simulation files with each build
rem c:\progra~1\rems\rems.exe wheat.mdb crop.tem met.tem validation

..\..\..\bin\apsrun.exe /auto  \development\apsim\wheat\validation\wheatvalidation.apsim  

c:\progra~1\R\rw2010\bin\Rterm.exe --slave < validation-1.r


..\..\..\bin\apsimreport.exe aps06.report graphs\aps6\aps06.gif
..\..\..\bin\apsimreport.exe aps06summary.report graphs\aps6\aps06summary.gif
..\..\..\bin\apsimreport.exe wheatvalidation.report graphs\wheatvalidation.gif

cd yieldprophet\2005
call runall.bat

popd