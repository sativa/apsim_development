@echo off

copy ..\..\YieldProphet\Validation\YieldProphet.report .

rem -----------------------------------------------------------
rem  2005
rem -----------------------------------------------------------
..\..\..\Bin\SetXMLValue YieldProphet.report Data/Filter/FilterString "Crop='Barley' and year=2005"
..\..\..\Bin\ApsimReport YieldProphet.report YieldProphet2005.jpg

rem -----------------------------------------------------------
rem  2006
rem -----------------------------------------------------------
..\..\..\Bin\SetXMLValue YieldProphet.report Data/Filter/FilterString "Crop='Barley' and year=2006"
..\..\..\Bin\ApsimReport YieldProphet.report YieldProphet2006.jpg

rem -----------------------------------------------------------
rem  2007
rem -----------------------------------------------------------
..\..\..\Bin\SetXMLValue YieldProphet.report Data/Filter/FilterString "Crop='Barley' and year=2007"
..\..\..\Bin\ApsimReport YieldProphet.report YieldProphet2007.jpg

rem -----------------------------------------------------------
rem  Overall
rem -----------------------------------------------------------
..\..\..\Bin\SetXMLValue YieldProphet.report Data/Filter/FilterString "Crop='Barley'"
..\..\..\Bin\ApsimReport YieldProphet.report YieldProphetOverall.jpg

del YieldProphet.report

