<?xml version="1.0"?>
<xsl:stylesheet
	version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">


<xsl:output
	method="html"
	doctype-public="-//W3C//DTD HTML 4.01//EN"
	doctype-system="http://www.w3.org/TR/html4/strict.dtd"/>


<!-- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Object oriented extension to Prolog
%  Release 2.9.3
%
%  Copyright (c) 1998-2002 Paulo Moura.  All Rights Reserved.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-->


<xsl:template match="/">
	<html>
	<head>
		<title><xsl:value-of select="logtalk/entity/name" /></title>
		<link rel="stylesheet" href="logtalk.css" type="text/css" />
	</head>
	<body>
		<hr />
		<h4 class="type"><xsl:value-of select="logtalk/entity/type" /></h4>
		<h1 class="code"><xsl:value-of select="logtalk/entity/name" /></h1>
		<xsl:apply-templates select="logtalk/entity" />
		<hr />
		<xsl:apply-templates select="logtalk/relations" />
		<hr />
		<xsl:apply-templates select="logtalk/predicates" />
		<hr />
	</body>
	</html>
</xsl:template>


<xsl:template match="logtalk/entity">
	<xsl:if test="comment">
		<blockquote><xsl:value-of select="comment" /></blockquote>
	</xsl:if>
	<dl>
	<xsl:if test="authors">
		<dt>authors:</dt>
			<dd><code><xsl:value-of select="authors" /></code></dd>
	</xsl:if>
	<xsl:if test="version">
		<dt>version:</dt>
			<dd><code><xsl:value-of select="version" /></code></dd>
	</xsl:if>
	<xsl:if test="date">
		<dt>date:</dt>
			<dd><code><xsl:value-of select="date" /></code></dd>
	</xsl:if>
	</dl>
	<dl>
		<dt>compilation:</dt>
			<dd><code><xsl:value-of select="compilation" /></code></dd>
	</dl>
	<dl>
	<xsl:for-each select="info">
		<dt><xsl:value-of select="key" />:</dt>
			<dd><code><xsl:value-of select="value" /></code></dd>
	</xsl:for-each>
	</dl>
</xsl:template>


<xsl:template match="logtalk/relations">
	<xsl:choose>
		<xsl:when test="*">
			<xsl:if test="implements">
			<dl>
			<dt>implements:</dt>
				<xsl:apply-templates select="implements" />
			</dl>
			</xsl:if>
			<xsl:if test="imports">
			<dl>
			<dt>imports:</dt>
				<xsl:apply-templates select="imports" />
			</dl>
			</xsl:if>
			<xsl:if test="extends">
			<dl>
			<dt>extends:</dt>
				<xsl:apply-templates select="extends" />
			</dl>
			</xsl:if>
			<xsl:if test="instantiates">
			<dl>
			<dt>instantiates:</dt>
				<xsl:apply-templates select="instantiates" />
			</dl>
			</xsl:if>
			<xsl:if test="specializes">
			<dl>
			<dt>specializes:</dt>
				<xsl:apply-templates select="specializes" />
			</dl>
			</xsl:if>
			<xsl:if test="uses">
			<dl>
			<dt>uses:</dt>
				<xsl:apply-templates select="uses" />
			</dl>
			</xsl:if>
			<xsl:if test="calls">
			<dl>
			<dt>calls:</dt>
				<xsl:apply-templates select="calls" />
			</dl>
			</xsl:if>
		</xsl:when>
		<xsl:otherwise>	
			<h4 class="code">(no dependencies on other files)</h4>
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>


<xsl:template match="logtalk/relations/uses">
	<dd><code><a><xsl:attribute name="href"><xsl:value-of select="file" />.xml</xsl:attribute><xsl:value-of select="name" /></a></code></dd>
</xsl:template>


<xsl:template match="logtalk/relations/calls">
	<dd><code><a><xsl:attribute name="href"><xsl:value-of select="file" />.xml</xsl:attribute><xsl:value-of select="name" /></a></code></dd>
</xsl:template>


<xsl:template match="logtalk/relations/*">
	<dd><code><xsl:value-of select="scope" /><xsl:text> </xsl:text><a><xsl:attribute name="href"><xsl:value-of select="file" />.xml</xsl:attribute><xsl:value-of select="name" /></a></code></dd>
</xsl:template>


<xsl:template match="logtalk/predicates">
	<h1>Public interface</h1>
	<xsl:choose>
		<xsl:when test="public/predicate">
			<xsl:apply-templates select="public/predicate" />
		</xsl:when>
		<xsl:when test="/logtalk/relations/*">		
			<h4 class="code">(see related entities)</h4>
		</xsl:when>
		<xsl:otherwise>
			<h4 class="code">(none)</h4>
		</xsl:otherwise>
	</xsl:choose>
	<h1>Protected interface</h1>
	<xsl:choose>
		<xsl:when test="protected/predicate">
			<xsl:apply-templates select="protected/predicate" />
		</xsl:when>
		<xsl:when test="/logtalk/relations/*">		
			<h4 class="code">(see related entities)</h4>
		</xsl:when>
		<xsl:otherwise>
			<h4 class="code">(none)</h4>
		</xsl:otherwise>
	</xsl:choose>
	<h1>Private predicates</h1>
	<xsl:choose>
		<xsl:when test="private/predicate">
			<xsl:apply-templates select="private/predicate" />
		</xsl:when>
		<xsl:when test="/logtalk/relations/*">		
			<h4 class="code">(see related entities)</h4>
		</xsl:when>
		<xsl:otherwise>
			<h4 class="code">(none)</h4>
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>


<xsl:template match="*/predicate">
	<h4 class="code"><xsl:value-of select="name" /></h4>
	<xsl:if test="comment">
		<blockquote><xsl:value-of select="comment" /></blockquote>
	</xsl:if>
	<dl class="predicate">
		<dt>compilation:</dt>
			<dd><code><xsl:value-of select="compilation" /></code></dd>
		<xsl:if test="template">
		<dt>template:</dt>
			<dd><code><xsl:value-of select="template" /></code></dd>
		</xsl:if>
		<xsl:if test="meta">
		<dt>metapredicate template:</dt>
			<dd><code><xsl:value-of select="meta" /></code></dd>
		</xsl:if>
		<xsl:if test="mode">
		<dt>mode - number of solutions:</dt>
		<xsl:for-each select="mode">
			<dd><code><xsl:value-of select="template" /><xsl:text> - </xsl:text><xsl:value-of select="solutions" /></code></dd>
		</xsl:for-each>
		</xsl:if>
	</dl>
	<dl class="predicate">
		<xsl:for-each select="info">
		<dt><xsl:value-of select="key" />:</dt>
			<dd><code><xsl:value-of select="value" /></code></dd>
		</xsl:for-each>
	</dl>
</xsl:template>


</xsl:stylesheet>
