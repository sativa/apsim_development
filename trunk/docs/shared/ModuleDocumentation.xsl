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
   <table>
   <xsl:apply-templates select="//variables">
      <xsl:with-param name="table-of-contents" select="'yes'"/>
      <xsl:sort select="@name"/>
   </xsl:apply-templates>
   </table>

   <xsl:apply-templates select="//variables">
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
<xsl:template match="variables">
   <xsl:param name="table-of-contents" select="'no'"/>
   <xsl:if test="name(..)!='Types'">
      <xsl:choose>
         <xsl:when test="$table-of-contents='yes'">
            <tr>
            <td>
            <xsl:choose>
               <xsl:when test="@link">
                  <a href="{@link}"><xsl:value-of select="name(..)"/> variables</a>
               </xsl:when>
               <xsl:otherwise>
                  <a href="#{name(..)}"><xsl:value-of select="name(..)"/> variables</a>
               </xsl:otherwise>
            </xsl:choose>
            </td>
            <xsl:if test="@sciencedoc">
               <td>
               <a href="{@sciencedoc}"><xsl:value-of select="@name"/> science documentation</a>
               </td>
            </xsl:if>
            </tr>

         </xsl:when>

         <xsl:otherwise>
            <xsl:if test="boolean(@link) = false">
               <H2><a name="{name(..)}">Variables for module <xsl:value-of select="name(..)"/></a></H2>
               <table>
               <tr>
               <td><b>Name</b></td>
               <td><b>Description</b></td>
                  <xsl:apply-templates>
                     <xsl:sort select="@name"/>
                  </xsl:apply-templates>
               </tr>
               </table>
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
