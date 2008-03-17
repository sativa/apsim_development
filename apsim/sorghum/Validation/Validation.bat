pushd
cd \development\apsim\sorghum\Validation\Hermitage\HE1-8

\development\bin\APSRun.exe /auto %CD%\HE1.apsim
\development\bin\apsimreport.exe HE1.report ..\..\HE1.gif

\development\bin\APSRun.exe /auto %CD%\HE2.apsim
\development\bin\apsimreport.exe HE2.report ..\..\HE2.gif

\development\bin\APSRun.exe /auto %CD%\HE3.apsim
\development\bin\apsimreport.exe HE3.report ..\..\HE3.gif

\development\bin\APSRun.exe /auto %CD%\HE4.apsim
\development\bin\apsimreport.exe HE4.report ..\..\HE4.gif

\development\bin\APSRun.exe /auto %CD%\HE5.apsim
\development\bin\apsimreport.exe HE5.report ..\..\HE5.gif

\development\bin\APSRun.exe /auto %CD%\HE6.apsim
\development\bin\apsimreport.exe HE6.report ..\..\HE6.gif

\development\bin\APSRun.exe /auto %CD%\HE7.apsim
\development\bin\apsimreport.exe HE7.report ..\..\HE7.gif

\development\bin\APSRun.exe /auto %CD%\HE8.apsim
\development\bin\apsimreport.exe HE8.report ..\..\HE8.gif


cd \development\apsim\sorghum\Validation\Icrisat\BW5

\development\bin\APSRun.exe /auto %CD%\bw5.apsim
\development\bin\apsimreport.exe BW5GxET1-4.report ..\..\BW5GxET1-4.gif
\development\bin\apsimreport.exe BW5GxET5-8.report ..\..\BW5GxET5-8.gif
\development\bin\apsimreport.exe BW5GxET9-12.report ..\..\BW5GxET9-12.gif

cd \development\apsim\sorghum\Validation\Lawes\LE13
\development\bin\APSRun.exe /auto %CD%\LE13.apsim
\development\bin\apsimreport.exe Sorghum_LE13_Buster.report ..\..\LE13_Buster.gif
\development\bin\apsimreport.exe Sorghum_LE13_M35-1.report ..\..\LE13_M35-1.gif

cd \development\apsim\sorghum\Validation\Lawes\LE14
\development\bin\APSRun.exe /auto %CD%\LE14.apsim
\development\bin\apsimreport.exe Sorghum_LE14.report ..\..\LE14.gif

cd \development\apsim\sorghum\Validation\Lawes\LE15
\development\bin\APSRun.exe /auto %CD%\LE15.apsim
\development\bin\apsimreport.exe Sorghum_LE15.report ..\..\LE15.gif

cd \development\apsim\sorghum\Validation\Lawes\LE17
\development\bin\APSRun.exe /auto %CD%\LE17.apsim
\development\bin\apsimreport.exe Sorghum_LE17.report ..\..\LE17.gif

cd \development\apsim\sorghum\Validation\Lawes\LE19
\development\bin\APSRun.exe /auto %CD%\LE19.apsim
\development\bin\apsimreport.exe LE19_Buster.report ..\..\LE19_Buster.gif
\development\bin\apsimreport.exe LE19_CSH13R.report ..\..\LE19_CSH13R.gif

cd \development\apsim\sorghum\Validation\Lawes\LE21
\development\bin\APSRun.exe /auto %CD%\LE21.apsim
\development\bin\apsimreport.exe LE21_A35xQL36.report  ..\..\LE21_A35xQL36.gif
\development\bin\apsimreport.exe LE21_CSH13R.report    ..\..\LE21_CSH13R.gif
\development\bin\apsimreport.exe LE21_QL39xQL36.report ..\..\LE21_QL39xQL36.gif


popd