md %1
rename %1.zip temp.zip
move temp.zip %1
cd %1
..\unzip32 -j temp.zip
cd ..
pause