<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
	version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:fo="http://www.w3.org/1999/XSL/Format">


<!-- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Object oriented extension to Prolog
%  Release 2.19.1
%
%  Copyright (c) 1998-2004 Paulo Moura.  All Rights Reserved.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-->


<xsl:output indent="yes"/>


<xsl:template match ="/">

	<fo:root xmlns:fo="http://www.w3.org/1999/XSL/Format">

		<fo:layout-master-set>
			<fo:simple-page-master
					master-name="simple"
					page-height="297mm" 
					page-width="210mm"
					margin-top="15mm" 
					margin-bottom="15mm" 
					margin-left="25mm" 
					margin-right="25mm">
				<fo:region-body margin-top="15mm" margin-bottom="15mm"/>
				<fo:region-before extent="15mm"/>
				<fo:region-after extent="15mm"/>
			</fo:simple-page-master>
		</fo:layout-master-set>

		<fo:page-sequence master-reference="simple">
		
			<fo:static-content flow-name="xsl-region-before">
				<fo:block>
					<fo:leader leader-pattern="rule" leader-length.optimum="100%"/>
				</fo:block>
				<fo:block
						text-align="end" 
						font-size="9pt" 
						font-family="sans-serif"
						font-weight="bold">
					<xsl:value-of select="logtalk/entity/type"/>: <xsl:value-of select="logtalk/entity/name"/>
				</fo:block>
			</fo:static-content> 

			<fo:static-content flow-name="xsl-region-after">
				<fo:block>
					<fo:leader leader-pattern="rule" leader-length.optimum="100%"/>
				</fo:block>
				<fo:block
						text-align="end" 
						font-size="9pt" 
						font-family="sans-serif"
						font-weight="bold">
					<fo:page-number/> of <fo:page-number-citation ref-id="end"/>
				</fo:block>
			</fo:static-content> 
	
			<fo:flow flow-name="xsl-region-body">
    	   		<fo:block
    	   				font-size="18pt" 
    	        	    font-family="sans-serif" 
    	        	    font-weight="bold"
    	        	    space-after="8pt">
    	    		<xsl:value-of select="logtalk/entity/name"/>
    	    	</fo:block>
 				<xsl:apply-templates select="logtalk/entity"/>
				<xsl:apply-templates select="logtalk/relations"/>
				<xsl:apply-templates select="logtalk/predicates"/>
				<fo:block id="end"/>
			</fo:flow>

		</fo:page-sequence>

	</fo:root>

</xsl:template>


<xsl:template match="logtalk/entity">

	<xsl:if test="comment">
		<fo:block
				margin-left="10mm"
				font-size="10pt"
				font-family="serif" 
				font-style="italic" 
				space-after="8pt">
			<xsl:value-of select="comment"/>
		</fo:block>
	</xsl:if>

	<xsl:if test="author">
		<fo:block
				font-size="10pt"
				font-family="serif" 
				keep-with-next="always">
			author:
		</fo:block>
		<fo:block
				font-size="9pt"
				font-family="monospace"
				margin-left="10mm">
			<xsl:value-of select="author"/>
		</fo:block>
	</xsl:if>

	<xsl:if test="version">
		<fo:block
				font-size="10pt"
				font-family="serif" 
				keep-with-next="always">
			version:
		</fo:block>
		<fo:block
				font-size="9pt"
				font-family="monospace"
				margin-left="10mm">
			<xsl:value-of select="version"/>
		</fo:block>
	</xsl:if>

	<xsl:if test="date">
		<fo:block
				font-size="10pt"
				font-family="serif" 
				keep-with-next="always">
			date:
		</fo:block>
		<fo:block
				font-size="9pt"
				font-family="monospace"
				margin-left="10mm">
			<xsl:value-of select="date"/>
		</fo:block>
	</xsl:if>

	<fo:block
			font-size="10pt"
			font-family="serif"
			space-before="8pt"
			keep-with-next="always">
		compilation:
	</fo:block>
	<fo:block
			font-size="9pt"
			font-family="monospace"
			margin-left="10mm" 
			space-after="8pt">
		<xsl:value-of select="compilation"/>
	</fo:block>

	<xsl:if test="info">
		<xsl:for-each select="info">
       		<fo:block
					font-size="10pt"
					font-family="serif" 
					keep-with-next="always">
     			<xsl:value-of select="key"/>:
     		</fo:block>
			<fo:block
					font-size="9pt"
					font-family="monospace"
					margin-left="10mm">
				<xsl:value-of select="value"/>
			</fo:block>
		</xsl:for-each>
	</xsl:if>

</xsl:template>


<xsl:template match="logtalk/relations">
	<xsl:choose>
		<xsl:when test="*">
			<xsl:if test="implements">
				<fo:block
						font-size="10pt"
						font-family="serif" 
						keep-with-next="always">
					implements:
				</fo:block>
				<xsl:apply-templates select="implements"/>
			</xsl:if>
			<xsl:if test="imports">
				<fo:block
						font-size="10pt"
						font-family="serif" 
						keep-with-next="always">
					imports:
				</fo:block>
				<xsl:apply-templates select="imports"/>
			</xsl:if>
			<xsl:if test="extends">
				<fo:block
						font-size="10pt"
						font-family="serif" 
						keep-with-next="always">
					extends:
				</fo:block>
				<xsl:apply-templates select="extends"/>
			</xsl:if>
			<xsl:if test="instantiates">
				<fo:block
						font-size="10pt"
						font-family="serif" 
						keep-with-next="always">
					instantiates:
				</fo:block>
				<xsl:apply-templates select="instantiates"/>
			</xsl:if>
			<xsl:if test="specializes">
				<fo:block
						font-size="10pt"
						font-family="serif" 
						keep-with-next="always">
					specializes:
				</fo:block>
				<xsl:apply-templates select="specializes"/>
			</xsl:if>
			<xsl:if test="uses">
				<fo:block
						font-size="10pt"
						font-family="serif" 
						keep-with-next="always">
					uses:
				</fo:block>
				<xsl:apply-templates select="uses"/>
			</xsl:if>
			<xsl:if test="calls">
				<fo:block
						font-size="10pt"
						font-family="serif" 
						keep-with-next="always">
					calls:
				</fo:block>
				<xsl:apply-templates select="calls"/>
			</xsl:if>
		</xsl:when>
		<xsl:otherwise>	
			<fo:block
					font-size="10pt"
					font-family="serif" 
					keep-with-next="always">
				(no dependencies on other files)
			</fo:block>
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>


<xsl:template match="logtalk/relations/uses">
	<fo:block
			font-size="9pt"
			font-family="monospace"
			margin-left="10mm">
		<xsl:value-of select="name"/>
	</fo:block>
</xsl:template>


<xsl:template match="logtalk/relations/calls">
	<fo:block
			font-size="9pt"
			font-family="monospace"
			margin-left="10mm">
		<xsl:value-of select="name"/>
	</fo:block>
</xsl:template>


<xsl:template match="logtalk/relations/*">
	<fo:block
			font-size="9pt"
			font-family="monospace"
			margin-left="10mm">
		<xsl:value-of select="scope"/><xsl:text> </xsl:text><xsl:value-of select="name"/>
	</fo:block>
</xsl:template>


<xsl:template match="logtalk/predicates">

	<fo:block
			font-size="14pt" 
			font-family="sans-serif" 
			font-weight="bold" 
			keep-with-next="always"
			space-before="18pt">
		Public interface
	</fo:block>
	<xsl:choose>
		<xsl:when test="public/predicate">
			<xsl:apply-templates select="public/predicate"/>
		</xsl:when>
		<xsl:when test="/logtalk/relations/*">		
			<fo:block
					font-size="10pt" 
					font-family="serif" 
					font-style="italic"
					space-before="10pt">
				(see related entities)
			</fo:block>
		</xsl:when>
		<xsl:otherwise>
			<fo:block
					font-size="10pt" 
					font-family="serif" 
					font-style="italic"
					space-before="10pt">
				(none)
			</fo:block>
		</xsl:otherwise>
	</xsl:choose>

	<fo:block
			font-size="14pt" 
			font-family="sans-serif" 
			font-weight="bold" 
			keep-with-next="always"
			space-before="18pt">
		Protected interface
	</fo:block>
	<xsl:choose>
		<xsl:when test="protected/predicate">
			<xsl:apply-templates select="protected/predicate"/>
		</xsl:when>
		<xsl:when test="/logtalk/relations/*">		
			<fo:block
					font-size="10pt" 
					font-family="serif" 
					font-style="italic"
					space-before="10pt">
				(see related entities)
			</fo:block>
		</xsl:when>
		<xsl:otherwise>
			<fo:block
					font-size="10pt" 
					font-family="serif" 
					font-style="italic"
					space-before="10pt">
				(none)
			</fo:block>
		</xsl:otherwise>
	</xsl:choose>

	<fo:block
			font-size="14pt" 
			font-family="sans-serif" 
			font-weight="bold" 
			keep-with-next="always"
			space-before="18pt">
		Private predicates
	</fo:block>
	<xsl:choose>
		<xsl:when test="private/predicate">
			<xsl:apply-templates select="private/predicate"/>
		</xsl:when>
		<xsl:when test="/logtalk/relations/*">		
			<fo:block
					font-size="10pt" 
					font-family="serif" 
					font-style="italic"
					space-before="10pt">
				(see related entities)
			</fo:block>
		</xsl:when>
		<xsl:otherwise>
			<fo:block
					font-size="10pt" 
					font-family="serif" 
					font-style="italic"
					space-before="10pt">
				(none)
			</fo:block>
		</xsl:otherwise>
	</xsl:choose>

</xsl:template>


<xsl:template match="*/predicate">

	<fo:block
			font-size="12pt" 
			font-family="sans-serif" 
			font-weight="bold" 
			keep-with-next="always"
			space-before="10pt">
		<xsl:value-of select="name"/>
	</fo:block>

	<xsl:if test="comment">
		<fo:block
				margin-left="10mm"
				font-size="10pt" 
				font-family="serif" 
				font-style="italic"
				space-before="4pt" 
				space-after="8pt">
			<xsl:value-of select="comment"/>
		</fo:block>
	</xsl:if>

	<fo:block
			font-size="10pt"
			font-family="serif"
			keep-with-next="always">
		compilation:
	</fo:block>
	<fo:block
			font-size="9pt"
			font-family="monospace"
			margin-left="10mm">
		<xsl:value-of select="compilation"/>
	</fo:block>

	<xsl:if test="template">
      	<fo:block
				font-size="10pt"
				font-family="serif" 
				keep-with-next="always">
     		template:
     	</fo:block>
		<fo:block
				font-size="9pt"
				font-family="monospace"
				margin-left="10mm">
			<xsl:value-of select="template"/>
		</fo:block>
	</xsl:if>

	<xsl:if test="meta">
      	<fo:block
				font-size="10pt"
				font-family="serif" 
				keep-with-next="always">
     		metapredicate template:
     	</fo:block>
		<fo:block
				font-size="9pt"
				font-family="monospace"
				margin-left="10mm">
			<xsl:value-of select="meta"/>
		</fo:block>
	</xsl:if>

	<xsl:if test="mode">
       	<fo:block
				font-size="10pt"
				font-family="serif" 
				keep-with-next="always">
     		mode - number of solutions:
     	</fo:block>
		<xsl:for-each select="mode">
			<fo:block
					font-size="9pt"
					font-family="monospace"
					margin-left="10mm">
				<xsl:value-of select="template"/> - <xsl:value-of select="solutions"/>
			</fo:block>
		</xsl:for-each>
	</xsl:if>

	<xsl:if test="exceptions">
       	<fo:block
				font-size="10pt"
				font-family="serif" 
				keep-with-next="always">
     		exceptions:
     	</fo:block>
		<xsl:for-each select="exceptions/exception">
       		<fo:block
					font-size="10pt"
					font-family="serif" 
					keep-with-next="always"
					margin-left="10mm">
     			<xsl:value-of select="condition" />:
     		</fo:block>
			<fo:block
					font-size="9pt"
					font-family="monospace"
					margin-left="20mm">
				<xsl:value-of select="term" />
			</fo:block>
		</xsl:for-each>
	</xsl:if>

	<xsl:if test="info">
		<xsl:for-each select="info">
       		<fo:block
					font-size="10pt"
					font-family="serif" 
					keep-with-next="always">
    	 			<xsl:value-of select="key"/>:
    	 		</fo:block>
			<fo:block
					font-size="9pt"
					font-family="monospace"
					margin-left="10mm">
				<xsl:value-of select="value"/>
			</fo:block>
		</xsl:for-each>
	</xsl:if>

</xsl:template>


</xsl:stylesheet>
