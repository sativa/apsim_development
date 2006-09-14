<?xml version="1.0" encoding="iso-8859-1"?><!-- DWXMLSource="../../Documents and Settings/gra518/My Documents/testXML.xml" --><!DOCTYPE xsl:stylesheet  [
	<!ENTITY nbsp   "&#160;">
	<!ENTITY copy   "&#169;">
	<!ENTITY reg    "&#174;">
	<!ENTITY trade  "&#8482;">
	<!ENTITY mdash  "&#8212;">
	<!ENTITY ldquo  "&#8220;">
	<!ENTITY rdquo  "&#8221;"> 
	<!ENTITY pound  "&#163;">
	<!ENTITY yen    "&#165;">
	<!ENTITY euro   "&#8364;">
]>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html" encoding="iso-8859-1" doctype-public="-//W3C//DTD XHTML 1.0 Transitional//EN" doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"/>
<xsl:template match="/">

<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1"/>
<title>Howwet Report</title>
<style type="text/css">
<xsl:comment>
.style1 {
	font-size: 36px;
	font-weight: bold;
}
</xsl:comment>
</style>
</head>

<body>
<div align="center" class="style1">Howwet Report</div>
<hr />
<p>Simulation Period: <xsl:value-of select="Results/startDate"/> to <xsl:value-of select="Results/endDate"/></p>
<p>Simulation file name: <xsl:value-of select="Results/simulationFileName"/></p>
<p>Met file name: <xsl:value-of select="Results/metFileName"/></p>
<table width="340" border="1" bordercolor="#000000" bgcolor="#FFFFFF">
  <caption align="top">
    <strong>Soil Water    </strong>
  </caption>
  <tr>
    <td width="128">Starting Water:</td>
    <td width="202"><xsl:value-of select="Results/soilWaterStart"/></td>
    </tr>
  <tr>
    <td>Fallow rainfall: </td>
    <td><xsl:value-of select="Results/rainfall"/></td>
    </tr>
  <tr>
    <td>Evaporation:</td>
    <td><xsl:value-of select="Results/evaporation"/></td>
    </tr>
  <tr>
    <td>Runoff:</td>
    <td><xsl:value-of select="Results/runoff"/></td>
  </tr>
  <tr>
    <td>Drainage:</td>
    <td><xsl:value-of select="Results/drain"/></td>
  </tr>
  <tr>
    <td>End water: </td>
    <td><xsl:value-of select="Results/soilWaterEnd"/></td>
  </tr>
  <tr>
    <td><strong>Gain in water: </strong></td>
    <td><strong><xsl:value-of select="Results/fallowWaterGain"/></strong></td>
  </tr>
  <tr>
    <td><strong>Fallow efficiencey:</strong> </td>
    <td><strong><xsl:value-of select="Results/fallowWaterEfficiency"/></strong></td>
  </tr>
</table>
<p>&nbsp;</p>
<p align="center"><img src="../../Robert/Data/RainfallSWChart.gif" alt="Rainfall Soil Water Chart" width="998" height="400" /></p>
<hr />
<p>&nbsp;</p>
</body>
</html>

</xsl:template>
</xsl:stylesheet>