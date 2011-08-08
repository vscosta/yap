<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="text" version="1.0"/>
  <xsl:template match="/">
    <xsl:for-each select="/doxygen/compounddef/sectiondef[@kind='func']/memberdef[starts-with(@id,'group__TaskModel') and not(starts-with(name,'operator')) and name!='tiebreak' and name!='wait' and not(starts-with(@id,'group__TaskModelMiniModel'))]">
      <xsl:value-of select="type"/>
      <xsl:text> </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:for-each select="param">
	<xsl:if test="position()=1"><xsl:text>(</xsl:text></xsl:if>
	<xsl:if test="position()!=1"><xsl:text>,</xsl:text></xsl:if>
	<xsl:value-of select="type"/>
	<xsl:for-each select="defval">
	  <xsl:text>=</xsl:text>
	  <xsl:value-of select="."/>
	</xsl:for-each>
      </xsl:for-each>
      <xsl:text>);
</xsl:text>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>
