<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:template match="/">
<html>
<head>
<title>APSIM Documentation</title>
</head>
<body bgcolor="#F3F3F3">
<p><img border="0" src="shared\apsimsupport3.gif" width="336" height="61"></img></p>
<xsl:call-template name="general" />
<xsl:call-template name="interface-list" />
</body>
</html>
</xsl:template>


<xsl:template name="general">
<table>
   <tr>
      <td width="100%"> 
         <table border="0" width="100%" cellspacing="1" cellpadding="0" >
            <tr>
               <td width="100%" ><font face="Arial Narrow" size="5" color="#FF9900">General Documentation : </font><font face="Arial Narrow" size="2" >(Useful and general information about APSIM)</font></td>
            </tr>
            <xsl:variable name="doc_loc"><xsl:value-of select="unparsed-entity-uri('docs')"/></xsl:variable>
            
            <xsl:for-each select="document/component">
            <xsl:choose>
               <xsl:when test="@link='asp'">
                  <tr>
                     <td width="100%" ><font size="4" face="Arial Narrow"><a href="{@type}"><xsl:value-of select="@name"/></a></font><font size="3" face="Arial Narrow"> : <xsl:value-of select="@description"/></font><br></br></td>
                  </tr>
               </xsl:when>
               <xsl:otherwise>
                  <tr>
                     <td width="100%" ><font size="4" face="Arial Narrow"><a href="{$doc_loc}{@type}"><xsl:value-of select="@name"/></a></font><font size="3" face="Arial Narrow"> : <xsl:value-of select="@description"/></font><br></br></td>
                  </tr>
               </xsl:otherwise>   
            </xsl:choose>
            
            </xsl:for-each>
         </table>
      </td>
   </tr>
</table>
</xsl:template>

<xsl:template name="interface-list">
   <table>
      <tr>
         <td width="100%"> 
            <table border="0" width="100%" cellspacing="1" cellpadding="0" >
               <tr>
                  <td width="100%" ><font face="Arial Narrow" size="5" color="#FF9900">Module reporting variables : </font><font face="Arial Narrow" size="2" >(Listing of module interface documents)</font></td>
               </tr>
               <xsl:variable name="module_list"><xsl:value-of select="unparsed-entity-uri('released')"/></xsl:variable>
               <xsl:variable name="module_loc"><xsl:value-of select="unparsed-entity-uri('loc')"/></xsl:variable>
               <xsl:variable name="inrow" select='4'/>
               
               
               <table border="0" width="600">
                  <xsl:apply-templates select="document($module_list)//modules/component[position() mod $inrow = 1]">
                  <xsl:with-param name='inrow' select='$inrow'/>
                  <xsl:with-param name='module_loc' select='$module_loc'/>
                  <xsl:with-param name='select-type'>interface</xsl:with-param>
                  </xsl:apply-templates>
               </table>
               
               <tr>
                  <td width="100%" ><font face="Arial Narrow" size="5" color="#FF9900">Module science documentation : </font><font face="Arial Narrow" size="2" >(Listing of module science documents)</font></td>
               </tr>
               
               <table border="0" width="600">
               <xsl:apply-templates select="document($module_list)//modules/component[position() mod $inrow = 1]">
               <xsl:with-param name='inrow' select='$inrow'/>
               <xsl:with-param name='module_loc' select='$module_loc'/>
               <xsl:with-param name='select-type'>science</xsl:with-param>
               </xsl:apply-templates>
               </table>
               
            </table>
         </td>
      </tr>
   </table>
</xsl:template>


<xsl:template match="component">
<xsl:param name="inrow"/>
<xsl:param name="module_loc"/>
<xsl:param name="select-type"/>
<tr>
<xsl:choose>
   <xsl:when test="$select-type='interface'">
      <td width="25%" ><font size="3" face="Arial Narrow"><a href="{$module_loc}{@name}/{@interface}"><xsl:value-of select="@name"/></a></font></td>
   </xsl:when>
   <xsl:otherwise>
      <td width="25%" ><font size="3" face="Arial Narrow"><a href="{$module_loc}{@name}/docs/{@science}"><xsl:value-of select="@name"/></a></font></td>
   </xsl:otherwise>   
</xsl:choose>
<xsl:apply-templates select="following::component[position() &lt; $inrow]" mode='cell'>
<xsl:with-param name="module_loc" select='$module_loc'/>
<xsl:with-param name="select-type" select='$select-type'/>
</xsl:apply-templates>
</tr>
</xsl:template>


<xsl:template match="component" mode='cell'>
<xsl:param name="inrow"/>
<xsl:param name="module_loc"/>
<xsl:param name="select-type"/>
<xsl:choose>
   <xsl:when test="$select-type='interface'">
      <td width="25%" ><font size="3" face="Arial Narrow"><a href="{$module_loc}{@name}/{@interface}"><xsl:value-of select="@name"/></a></font></td>
   </xsl:when>
   <xsl:otherwise>
      <td width="25%" ><font size="3" face="Arial Narrow"><a href="{$module_loc}{@name}/docs/{@science}"><xsl:value-of select="@name"/></a></font></td>
   </xsl:otherwise>   
</xsl:choose>
</xsl:template>

   
</xsl:stylesheet>

