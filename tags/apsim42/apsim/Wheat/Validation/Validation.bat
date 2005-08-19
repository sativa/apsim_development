pushd
cd \development\apsim\wheat\validation
c:\progra~1\rems\rems.exe wheat.mdb crop.tem met.tem validation
..\..\..\bin\apsrun.exe /Q  validation.con  
c:\progra~1\R\rw2010\bin\Rterm.exe --slave < validation-1.r
popd