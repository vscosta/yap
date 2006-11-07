<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
	version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:xhtml="http://www.w3.org/1999/xhtml">

<xsl:output
	method="xml"
    indent="yes"
    encoding="utf-8"
	omit-xml-declaration="yes"/>

<xsl:template match="node()|@*">
	<xsl:copy>
		<xsl:apply-templates select="node()|@*"/>
	</xsl:copy>
</xsl:template>

<xsl:template match="xhtml:html">
		<xsl:apply-templates select="node()|@*"/>
</xsl:template>

<xsl:template match="xhtml:head">
</xsl:template>

<xsl:template match="xhtml:body">
		<xsl:apply-templates select="node()|@*"/>
</xsl:template>

<xsl:template match="xhtml:div[@class='top-right']">
	<div class="top-right"> 
		<span class="chapter"/> 
	</div>
</xsl:template>

<xsl:template match="xhtml:div[@class='footer']">
</xsl:template>

<xsl:template match="xhtml:h1/xhtml:a[@id]">
	<span>
		<xsl:attribute name="id">
			<xsl:value-of select="@id"/>
		</xsl:attribute> 
		<xsl:value-of select="."/>
	</span>
</xsl:template>

<xsl:template match="xhtml:h2/xhtml:a[@id]">
	<span>
		<xsl:attribute name="id">
			<xsl:value-of select="@id"/>
		</xsl:attribute> 
		<xsl:value-of select="."/>
	</span>
</xsl:template>

<xsl:template match="xhtml:h3/xhtml:a[@id]">
	<span>
		<xsl:attribute name="id">
			<xsl:value-of select="@id"/>
		</xsl:attribute> 
		<xsl:value-of select="."/>
	</span>
</xsl:template>

<xsl:template match="xhtml:h4/xhtml:a[@id]">
	<span>
		<xsl:attribute name="id">
			<xsl:value-of select="@id"/>
		</xsl:attribute> 
		<xsl:value-of select="."/>
	</span>
</xsl:template>

</xsl:stylesheet>
