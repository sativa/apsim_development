<?xml version="1.0"?>
<xsl:stylesheet version = '1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>
<xsl:output method="html"/>

<!-- ============================================================================
     Match the top level node and apply 'variables' template twice, once for
     a table of contents and once for the body of the document.
     ============================================================================ -->
<xsl:template match="/">
   <html>
   <link rel="stylesheet" href="../docs/shared/apsim-web-style.css" type="text/css"/>
   <body>
   <h2>Module documentation</h2>

   <!-- Create a table of contents -->
   <xsl:apply-templates select="//Documentation">
      <xsl:with-param name="table-of-contents" select="'yes'"/>
      <xsl:sort select="name(..)"/>
   </xsl:apply-templates>

   <xsl:apply-templates select="//Documentation">
      <xsl:with-param name="table-of-contents" select="'no'"/>
      <xsl:sort select="@name"/>
   </xsl:apply-templates>

   </body>
   </html>
</xsl:template>

<!-- ============================================================================
     Matches all 'variables' elements. Called twice, once for a table of contents
     and once for the actual variables
     ============================================================================ -->
<xsl:template match="Documentation">
   <xsl:param name="table-of-contents" select="'no'"/>
   <xsl:if test="name(..)!='Types'">
      <xsl:choose>
         <xsl:when test="$table-of-contents='yes'">
            <p>
            <a href="#{name(..)}"><xsl:value-of select="name(..)"/></a>
            </p>
         </xsl:when>

         <xsl:otherwise>
            <H2><a name="{name(..)}"><xsl:value-of select="name(..)"/></a></H2>
            <a href="{.}"><xsl:value-of select="@name"/> Documentation</a>
            <xsl:if test="../Documentation2">
               <p><a href="{../Documentation2}"><xsl:value-of select="../Documentation2/@name"/> Documentation</a></p>
            </xsl:if>
            <xsl:if test="../variables">
              <xsl:choose>
                  <xsl:when test="boolean(../variables/@link) = false">
                     <table>
                     <tr><td><b>Variables available for reporting</b></td></tr>
                     <tr>
                     <td><b>Name</b></td>
                     <td><b>Description</b></td>
                        <xsl:apply-templates select="../variables">
                           <xsl:sort select="@name"/>
                        </xsl:apply-templates>
                     </tr>
                     </table>
                  </xsl:when>
                  <xsl:otherwise>
                     <p><a href="{../variables/@link}">Link to variables for outputting</a></p>
                  </xsl:otherwise>
              </xsl:choose>
           </xsl:if>
      </xsl:otherwise>
      </xsl:choose>
   </xsl:if>
</xsl:template>

<!-- ============================================================================
     Matches all 'variable' elements. Simply output variable details.
     ============================================================================ -->
<xsl:template match="variable">
   <tr>
   <td><xsl:value-of select="@name"/></td>
   <td><xsl:value-of select="@description"/></td>
   </tr>
</xsl:template>

</xsl:stylesheet>
