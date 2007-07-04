pushd
cd %APSROOT%\apsim\lupin\validation

%APSROOT%\bin\apsrun.exe /auto  %APSROOT%\apsim\lupin\validation\validation.con  
%APSROOT%\bin\apsimreport.exe validation.report LupinValidation.gif

mkdir ..\sensibility
%APSROOT%\bin\apsimreport.exe sensibility.report ..\sensibility\LupinSensibility.gif

popd