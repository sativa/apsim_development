<?xml version="1.0" encoding="UTF-8" ?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template match="books">
    <html>
      <link rel="stylesheet" type="text/css" href="Howwet.css"></link>
      <body>
        <th>
          <xsl:value-of select="report|howwet|head|title"/>
        </th>
        <table>
          <tr>
            <td>
              <img src="RainfallSWChart.gif"/>
            </td>
          </tr>
        </table>
        <table>
          <xsl:apply-templates/>
        </table>
      </body>
    </html>
  </xsl:template>
  <xsl:template match="book">
    <tr>
      <td>
        <xsl:number/>
      </td>
      <xsl:apply-templates/>
    </tr>
  </xsl:template>
  <xsl:template match="author|title|price">
    <td>
      <xsl:value-of select="."/>
    </td>
  </xsl:template>
</xsl:stylesheet>

