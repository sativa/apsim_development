<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:template match="/">
   <html>
   <link rel="stylesheet" href="shared/apsim-web-style.css" type="text/css"/>
   <h2>APSIM DocCentral</h2>
   <body>
   <table>
   <xsl:apply-templates/>
   </table>
   </body>
   </html>
</xsl:template>

<xsl:template match="Documentation">
   <tr>
   <td>
   <a href="{.}"><xsl:value-of select="@name"/></a>
   </td>
   </tr>

</xsl:template>


</xsl:stylesheet>

