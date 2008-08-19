set APSBIN=c:\development\Bin
set APSROOT=c:\development

pushd
cd %APSROOT%\apsim\Sorghum\Validation

cd .\Hermitage\HE1-8

"%APSBIN%\apsimreport.exe" HE1.report ..\..\HE1.gif

"%APSBIN%\apsimreport.exe" HE2.report ..\..\HE2.gif

"%APSBIN%\apsimreport.exe" HE3.report ..\..\HE3.gif

"%APSBIN%\apsimreport.exe" HE4.report ..\..\HE4.gif

"%APSBIN%\apsimreport.exe" HE5.report ..\..\HE5.gif

"%APSBIN%\apsimreport.exe" HE6.report ..\..\HE6.gif

"%APSBIN%\apsimreport.exe" HE7.report ..\..\HE7.gif

"%APSBIN%\apsimreport.exe" HE8.report ..\..\HE8.gif

cd ..\..\Icrisat\BW

"%APSBIN%\apsimreport.exe" BW5GxET1-4.report ..\..\BW5GxET1-4.gif
"%APSBIN%\apsimreport.exe" BW5GxET5-8.report ..\..\BW5GxET5-8.gif
"%APSBIN%\apsimreport.exe" BW5GxET9-12.report ..\..\BW5GxET9-12.gif
"%APSBIN%\apsimreport.exe" BW8GxET1-4.report ..\..\BW8GxET1-4.gif
"%APSBIN%\apsimreport.exe" BW8GxET5-8.report ..\..\BW8GxET5-8.gif
"%APSBIN%\apsimreport.exe" BW8GxET9-12.report ..\..\BW8GxET9-12.gif

cd ..\..\Lawes\LE
"%APSBIN%\apsimreport.exe" Sorghum_LE13_Buster.report ..\..\LE13_Buster.gif
"%APSBIN%\apsimreport.exe" Sorghum_LE13_M35-1.report ..\..\LE13_M35-1.gif
"%APSBIN%\apsimreport.exe" Sorghum_LE14.report ..\..\LE14.gif
"%APSBIN%\apsimreport.exe" Sorghum_LE17.report ..\..\LE17.gif
"%APSBIN%\apsimreport.exe" LE19_Buster.report ..\..\LE19_Buster.gif
"%APSBIN%\apsimreport.exe" LE19_CSH13R.report ..\..\LE19_CSH13R.gif
"%APSBIN%\apsimreport.exe" LE21_A35xQL36.report  ..\..\LE21_A35xQL36.gif
"%APSBIN%\apsimreport.exe" LE21_CSH13R.report    ..\..\LE21_CSH13R.gif
"%APSBIN%\apsimreport.exe" LE21_QL39xQL36.report ..\..\LE21_QL39xQL36.gif


cd %APSROOT%\apsim\Sorghum\Validation

md Predicted
copy /Y Hermitage\HE1-8\*.out Predicted
copy /Y Icrisat\BW\*.out Predicted
copy /Y Lawes\LE\*.out Predicted

"%APSBIN%\apsimreport.exe" SorghumValidation.report SorghumValidation.gif

cd ..\..
popd
