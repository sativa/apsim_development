pushd
cd \development\apsim\wheat\validation

rem no longer use rems to recreate simulation files with each build
rem c:\progra~1\rems\rems.exe wheat.mdb crop.tem met.tem validation

..\..\..\bin\apsrun.exe /auto  \development\apsim\wheat\validation\wheatvalidation.apsim  

c:\progra~1\R\rw2010\bin\Rterm.exe --slave < validation-1.r
popd