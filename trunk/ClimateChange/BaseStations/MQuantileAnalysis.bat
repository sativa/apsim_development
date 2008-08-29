@echo off
cd %~p1
echo inputFile ^<- "%~nx1" > Step1.R
echo outputFile ^<- "%~n1.quantileTrendChange.csv" >> Step1.R
echo source("mkQuantiles.R") >> Step1.R
"c:/Program Files/R/R-2.7.1/bin/rterm.exe" --slave --quiet < step1.R
del step1.R
pause