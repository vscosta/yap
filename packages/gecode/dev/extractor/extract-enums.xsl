<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="text" version="1.0"/>
  <xsl:template match="/">
    <xsl:text>ENUM_CLASSES = []

</xsl:text>
    <xsl:for-each select="/doxygen/compounddef/sectiondef[@kind='enum']/memberdef">
      <xsl:text>class </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>(object):
</xsl:text>
      <xsl:text>    TYPE = '</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>'
    ENUM = [</xsl:text>
      <xsl:for-each select="enumvalue">
	<xsl:if test="position()!=1"><xsl:text>,</xsl:text></xsl:if>
	<xsl:text>'</xsl:text>
	<xsl:value-of select="name"/>
	<xsl:text>'</xsl:text>
      </xsl:for-each>
      <xsl:text>]

ENUM_CLASSES.append(</xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>)

</xsl:text>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>
