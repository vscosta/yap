<?xml version="1.0"?>

<xsl:stylesheet
	version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">


<xsl:output method="xml"/>


<!-- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Object oriented extension to Prolog
%  Release 2.15.1
%
%  Copyright (c) 1998-2003 Paulo Moura.  All Rights Reserved.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-->


<xsl:template match="/">
	<TeXML>
		<cmd name="documentclass">
			<opt>10pt</opt>
			<parm>report</parm>
		</cmd>
		<env name="document">
			<cmd name="hrule"/>
			<env name="flushright">
				<cmd name="textbf"><parm><xsl:value-of select="logtalk/entity/type" /></parm></cmd>
			</env>
			<cmd name="section*">
				<parm><xsl:value-of select="logtalk/entity/name" /></parm>
			</cmd>
			<xsl:apply-templates/>
		</env>
	</TeXML>
</xsl:template>


<xsl:template match="logtalk/entity">
	<xsl:if test="comment">
		<env name="quote"><xsl:value-of select="comment" /></env>
	</xsl:if>
	<xsl:if test="author">
		<cmd name="medskip"/>
		<cmd name="noindent"/>
		author:
		<cmd name="par"/>
		<cmd name="texttt"><parm><xsl:value-of select="author" /></parm></cmd>
		<cmd name="par"/>
	</xsl:if>
	<xsl:if test="version">
		<cmd name="medskip"/>
		<cmd name="noindent"/>
		version:
		<cmd name="par"/>
		<cmd name="texttt"><parm><xsl:value-of select="version" /></parm></cmd>
		<cmd name="par"/>
	</xsl:if>
	<xsl:if test="date">
		<cmd name="medskip"/>
		<cmd name="noindent"/>
		date:
		<cmd name="par"/>
		<cmd name="texttt"><parm><xsl:value-of select="date" /></parm></cmd>
		<cmd name="par"/>
	</xsl:if>
	<cmd name="medskip"/>
	<cmd name="noindent"/>
	compilation:
	<cmd name="par"/>
	<cmd name="texttt"><parm><xsl:value-of select="compilation" /></parm></cmd>
	<cmd name="par"/>
	<xsl:for-each select="info">
		<cmd name="medskip"/>
		<cmd name="noindent"/>
		<xsl:value-of select="key" />:
		<cmd name="par"/>
		<cmd name="texttt"><parm><xsl:value-of select="value" /></parm></cmd>
		<cmd name="par"/>
	</xsl:for-each>
</xsl:template>


<xsl:template match="logtalk/relations">
	<cmd name="bigskip"/>
	<cmd name="bigskip"/>
	<cmd name="hrule"/>
	<xsl:choose>
		<xsl:when test="*">
			<xsl:if test="implements">
				<cmd name="bigskip"/>
				<cmd name="noindent"/>
				implements:
				<cmd name="par"/>
				<xsl:apply-templates select="implements" />
				<cmd name="par"/>
			</xsl:if>
			<xsl:if test="imports">
				<cmd name="bigskip"/>
				<cmd name="noindent"/>
				imports:
				<cmd name="par"/>
				<xsl:apply-templates select="imports" />
				<cmd name="par"/>
			</xsl:if>
			<xsl:if test="extends">
				<cmd name="bigskip"/>
				<cmd name="noindent"/>
				extends:
				<cmd name="par"/>
				<xsl:apply-templates select="extends" />
				<cmd name="par"/>
			</xsl:if>
			<xsl:if test="instantiates">
				<cmd name="bigskip"/>
				<cmd name="noindent"/>
				instantiates:
				<cmd name="par"/>
				<xsl:apply-templates select="instantiates" />
				<cmd name="par"/>
			</xsl:if>
			<xsl:if test="specializes">
				<cmd name="bigskip"/>
				<cmd name="noindent"/>
				specializes:
				<cmd name="par"/>
				<xsl:apply-templates select="specializes" />
				<cmd name="par"/>
			</xsl:if>
			<xsl:if test="uses">
				<cmd name="bigskip"/>
				<cmd name="noindent"/>
				uses:
				<cmd name="par"/>
				<xsl:apply-templates select="uses" />
				<cmd name="par"/>
			</xsl:if>
			<xsl:if test="calls">
				<cmd name="bigskip"/>
				<cmd name="noindent"/>
				calls:
				<cmd name="par"/>
				<xsl:apply-templates select="calls" />
				<cmd name="par"/>
			</xsl:if>
		</xsl:when>
		<xsl:otherwise>	
			<cmd name="bigskip"/>
			<cmd name="noindent"/>
			<cmd name="textsf"><parm>(no dependencies on other files)</parm></cmd>
			<cmd name="par"/>
		</xsl:otherwise>
	</xsl:choose>
	<cmd name="bigskip"/>
	<cmd name="hrule"/>
	<cmd name="bigskip"/>
</xsl:template>


<xsl:template match="logtalk/relations/uses">
	<cmd name="texttt"><parm><xsl:value-of select="name" /></parm></cmd>
</xsl:template>


<xsl:template match="logtalk/relations/calls">
	<cmd name="texttt"><parm><xsl:value-of select="name" /></parm></cmd>
</xsl:template>


<xsl:template match="logtalk/relations/*" xml:space="preserve">
	<cmd name="texttt"><parm><xsl:value-of select="scope" /> <xsl:value-of select="name" /></parm></cmd>
</xsl:template>


<xsl:template match="logtalk/predicates">
	<cmd name="section*">
		<parm>Public interface</parm>
	</cmd>
	<xsl:choose>
		<xsl:when test="public/predicate">
			<xsl:apply-templates select="public/predicate" />
		</xsl:when>
		<xsl:when test="/logtalk/relations/*">		
			<cmd name="texttt"><parm>(see related entities)</parm></cmd>
		</xsl:when>
		<xsl:otherwise>
			<cmd name="texttt"><parm>(none)</parm></cmd>
		</xsl:otherwise>
	</xsl:choose>
	<cmd name="section*">
		<parm>Protected interface</parm>
	</cmd>
	<xsl:choose>
		<xsl:when test="protected/predicate">
			<xsl:apply-templates select="protected/predicate" />
		</xsl:when>
		<xsl:when test="/logtalk/relations/*">		
			<cmd name="texttt"><parm>(see related entities)</parm></cmd>
		</xsl:when>
		<xsl:otherwise>
			<cmd name="texttt"><parm>(none)</parm></cmd>
		</xsl:otherwise>
	</xsl:choose>
	<cmd name="section*">
		<parm>Private predicates</parm>
	</cmd>
	<xsl:choose>
		<xsl:when test="private/predicate">
			<xsl:apply-templates select="private/predicate" />
		</xsl:when>
		<xsl:when test="/logtalk/relations/*">		
			<cmd name="texttt"><parm>(see related entities)</parm></cmd>
		</xsl:when>
		<xsl:otherwise>
			<cmd name="texttt"><parm>(none)</parm></cmd>
		</xsl:otherwise>
	</xsl:choose>
	<cmd name="bigskip"/>
	<cmd name="bigskip"/>
	<cmd name="hrule"/>
</xsl:template>


<xsl:template match="*/predicate">
	<cmd name="subsection*">
		<parm><xsl:value-of select="name" /></parm>
	</cmd>
	<xsl:if test="comment">
		<env name="quote"><xsl:value-of select="comment" /></env>
	</xsl:if>
	<cmd name="medskip"/>
	<cmd name="noindent"/>
	compilation:
	<cmd name="par"/>
	<cmd name="texttt"><parm><xsl:value-of select="compilation" /></parm></cmd>
	<cmd name="par"/>
	<xsl:if test="template">
		<cmd name="medskip"/>
		<cmd name="noindent"/>
		template:
		<cmd name="par"/>
		<cmd name="texttt"><parm><xsl:value-of select="template" /></parm></cmd>
		<cmd name="par"/>
	</xsl:if>
	<xsl:if test="meta">
		<cmd name="medskip"/>
		<cmd name="noindent"/>
		metapredicate template:
		<cmd name="par"/>
		<cmd name="texttt"><parm><xsl:value-of select="meta" /></parm></cmd>
		<cmd name="par"/>
	</xsl:if>
	<xsl:if test="mode">
		<cmd name="medskip"/>
		<cmd name="noindent"/>
		mode <cmd name="textendash"/> <cmd name="space"/> number of solutions:
		<cmd name="par"/>
		<xsl:for-each select="mode">
			<cmd name="texttt"><parm><xsl:value-of select="template" /></parm></cmd>
			<cmd name="space"/> <cmd name="textendash"/> <cmd name="space"/>
			<cmd name="texttt"><parm><xsl:value-of select="solutions" /></parm></cmd>
		</xsl:for-each>
	</xsl:if>
	<xsl:for-each select="info">
		<cmd name="noindent"/>
		<cmd name="medskip"/>
		<xsl:value-of select="key" />:
		<cmd name="par"/>
		<cmd name="texttt"><parm><xsl:value-of select="value" /></parm></cmd>
		<cmd name="par"/>
	</xsl:for-each>
</xsl:template>


</xsl:stylesheet>
