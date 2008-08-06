set APSBIN=c:\development\Bin

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

cd ..\..\Icrisat\BW5

"%APSBIN%\apsimreport.exe" BW5GxET1-4.report ..\..\BW5GxET1-4.gif
"%APSBIN%\apsimreport.exe" BW5GxET5-8.report ..\..\BW5GxET5-8.gif
"%APSBIN%\apsimreport.exe" BW5GxET9-12.report ..\..\BW5GxET9-12.gif

cd ..\..\Lawes\LE13
"%APSBIN%\apsimreport.exe" Sorghum_LE13_Buster.report ..\..\LE13_Buster.gif
"%APSBIN%\apsimreport.exe" Sorghum_LE13_M35-1.report ..\..\LE13_M35-1.gif

cd ..\LE14
"%APSBIN%\apsimreport.exe" Sorghum_LE14.report ..\..\LE14.gif

cd ..\LE15
"%APSBIN%\apsimreport.exe" Sorghum_LE15.report ..\..\LE15.gif

cd ..\LE17
"%APSBIN%\apsimreport.exe" Sorghum_LE17.report ..\..\LE17.gif

cd ..\LE19
"%APSBIN%\apsimreport.exe" LE19_Buster.report ..\..\LE19_Buster.gif
"%APSBIN%\apsimreport.exe" LE19_CSH13R.report ..\..\LE19_CSH13R.gif

cd ..\LE21
"%APSBIN%\apsimreport.exe" LE21_A35xQL36.report  ..\..\LE21_A35xQL36.gif
"%APSBIN%\apsimreport.exe" LE21_CSH13R.report    ..\..\LE21_CSH13R.gif
"%APSBIN%\apsimreport.exe" LE21_QL39xQL36.report ..\..\LE21_QL39xQL36.gif

cd ..\..
popd