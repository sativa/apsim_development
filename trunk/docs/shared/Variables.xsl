<?xml version="1.0"?>
<xsl:stylesheet version = '1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>
<xsl:output method="html"/>

<!-- ============================================================================
     Match the top level node and setup the document.
     ============================================================================ -->
<xsl:template match="/">
   <html>
   <link rel="stylesheet" href="../docs/shared/apsim-web-style.css" type="text/css"/>
   <body>
   <xsl:apply-templates select="//variables"/>
   </body>
   </html>
</xsl:template>


<!-- ============================================================================
     Matches all 'variables' elements. Called twice, once for a table of contents
     and once for the actual variables
     ============================================================================ -->
<xsl:template match="variables">
   <table>
   <tr>
   <td><b>Name</b></td>
   <td><b>Description</b></td>
   <td><b>Units</b></td>
   </tr>
   <H2><a name="{name(..)}"><xsl:value-of select="name(..)"/></a> variables</H2>

   <xsl:apply-templates select="variable">
      <xsl:sort select="@name"/>
   </xsl:apply-templates>
   </table>
</xsl:template>


<!-- ============================================================================
     Matches all 'variable' elements. Simply output variable details.
     ============================================================================ -->
<xsl:template match="variable">
   <tr>
   <td><xsl:value-of select="@name"/></td>
   <td><xsl:value-of select="@description"/></td>
   <td><xsl:value-of select="@units"/></td>
   </tr>
</xsl:template>

</xsl:stylesheet>
