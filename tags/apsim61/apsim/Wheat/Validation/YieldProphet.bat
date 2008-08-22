@echo off

copy ..\..\YieldProphet\Validation\YieldProphet.report .

rem -----------------------------------------------------------
rem  2004
rem -----------------------------------------------------------
..\..\..\Bin\SetXMLValue YieldProphet.report Data/Filter/FilterString "Crop='Wheat' and year=2004"
..\..\..\Bin\ApsimReport YieldProphet.report YieldProphet2004.gif

rem -----------------------------------------------------------
rem  2005
rem -----------------------------------------------------------
..\..\..\Bin\SetXMLValue YieldProphet.report Data/Filter/FilterString "Crop='Wheat' and year=2005"
..\..\..\Bin\ApsimReport YieldProphet.report YieldProphet2005.gif

rem -----------------------------------------------------------
rem  2006
rem -----------------------------------------------------------
..\..\..\Bin\SetXMLValue YieldProphet.report Data/Filter/FilterString "Crop='Wheat' and year=2006"
..\..\..\Bin\ApsimReport YieldProphet.report YieldProphet2006.gif

rem -----------------------------------------------------------
rem  2007
rem -----------------------------------------------------------
..\..\..\Bin\SetXMLValue YieldProphet.report Data/Filter/FilterString "Crop='Wheat' and year=2007"
..\..\..\Bin\ApsimReport YieldProphet.report YieldProphet2007.gif

rem -----------------------------------------------------------
rem  Overall
rem -----------------------------------------------------------
..\..\..\Bin\SetXMLValue YieldProphet.report Data/Filter/FilterString "Crop='Wheat'"
..\..\..\Bin\ApsimReport YieldProphet.report YieldProphetOverall.gif

del YieldProphet.report

