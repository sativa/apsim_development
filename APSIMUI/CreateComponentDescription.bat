@echo off

echo ^<?xml version="1.0"?^> > %APSROOT%\ApsimUI\ComponentDescription.xml
echo ^<?xml-stylesheet type="text/xsl" href="../docs/shared/Variables.xsl"?^> >> %APSROOT%\ApsimUI\ComponentDescription.xml
echo ^<ComponentDescription^> >> %APSROOT%\ApsimUI\ComponentDescription.xml

%APSROOT%\bin\getcomponentdescription %APSROOT%\apsim\plant\lib\plant.dll wheat >> %APSROOT%\ApsimUI\ComponentDescription.xml
%APSROOT%\bin\getcomponentdescription %APSROOT%\apsim\sorghum\lib\sorghum.dll sorghum >> %APSROOT%\ApsimUI\ComponentDescription.xml
%APSROOT%\bin\getcomponentdescription %APSROOT%\apsim\soiln2\lib\soiln2.dll soiln2 >> %APSROOT%\ApsimUI\ComponentDescription.xml

c:
cd "c:\program files\farmwise"
%APSROOT%\bin\getcomponentdescription "c:\program files\farmwise\stock.dll" stock >> %APSROOT%\ApsimUI\ComponentDescription.xml
%APSROOT%\bin\getcomponentdescription "c:\program files\farmwise\pasture.dll" pasture >> %APSROOT%\ApsimUI\ComponentDescription.xml
%APSROOT%\bin\getcomponentdescription "c:\program files\farmwise\supplement.dll" supplement >> %APSROOT%\ApsimUI\ComponentDescription.xml
echo ^</ComponentDescription^> >> %APSROOT%\ApsimUI\ComponentDescription.xml


