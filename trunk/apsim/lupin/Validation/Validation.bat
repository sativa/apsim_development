pushd
cd %APSROOT%\apsim\lupin\validation
%APSROOT%\bin\apsimreport.exe validation.report LupinValidation.gif
mkdir ..\sensibility
%APSROOT%\bin\apsimreport.exe sensibility.report ..\sensibility\LupinSensibility.gif
popd