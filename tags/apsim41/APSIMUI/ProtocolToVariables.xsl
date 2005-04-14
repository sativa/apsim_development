<?xml version="1.0"?>
<xsl:stylesheet version = '1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:output method="xml"/>
<xsl:template match="describecomp">
      <component name="{@name}">
         <variables>
   <xsl:apply-templates select="property"/>
         </variables>
         <events>
   <xsl:apply-templates select="event"/>
         </events>
      </component>

</xsl:template>

<xsl:template match="property">
      <variable name="{@name}" description="{@description}" array="{type/@array}" units="{type/@unit}"/>
</xsl:template>
<xsl:template match="event">
      <event name="{@name}" description="{@description}"/>
</xsl:template>

</xsl:stylesheet>
