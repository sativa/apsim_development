pushd
cd \development\apsim\graz\Validation

\development\bin\apsrun.exe /auto  \development\apsim\graz\Validation\BSc4past.con

c:\progra~1\R\R-2.5.0\bin\Rterm.exe --slave < validation.r

popd