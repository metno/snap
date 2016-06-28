<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet version="1.0" 
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text"/>
<xsl:key name="isoreg" match="Item" use="@ItemName"/>

<xsl:template match="CBRN_Sourceterm">
<xsl:apply-templates select="Header"/>	
<xsl:apply-templates select="TimeOfInitialRelease"/>	
<xsl:apply-templates select="ItemsTot"/>	
<xsl:apply-templates select="Source"/>	
</xsl:template>

<xsl:template match="Header">
<xsl:value-of select="./Place/Location/@Latitude"/><xsl:text> LATITUDE&#10;</xsl:text>
<xsl:value-of select="./Place/Location/@Longitude"/><xsl:text> LONGITUDE&#10;</xsl:text>
</xsl:template>

<xsl:template match="TimeOfInitialRelease">
<xsl:value-of select="."/><xsl:text> RELEASE START&#10;</xsl:text>
</xsl:template>

<xsl:template match="ItemsTot">
<xsl:value-of select="@Number"/><xsl:text> ISOTOPES&#10;</xsl:text>
<xsl:for-each select="Item">
	<xsl:value-of select="@ItemNum"/><xsl:text> </xsl:text><xsl:value-of select="@ItemName"/><xsl:text>&#10;</xsl:text>	
</xsl:for-each>
</xsl:template>

<xsl:template match="Source">
<xsl:value-of select="count(//ReleaseInterval)"/><xsl:text> RELEASE INTERVALS&#10;</xsl:text>
<xsl:for-each select="TimeDependent/ReleaseInterval">
	<xsl:text>INTERVAL&#10;</xsl:text>
	<xsl:apply-templates select="SourceTime"/>
	<xsl:apply-templates select="SourcePosition"/>
	<xsl:apply-templates select="SourceStrength"/>
</xsl:for-each>	
</xsl:template>

<xsl:template match="SourceTime">
<xsl:variable name="days">
	<xsl:choose>
 	 <xsl:when test="contains(@EndTime,'D')">
			<xsl:value-of select="substring-after(substring-before(@EndTime,'D'),'P')"/>
 	 </xsl:when>
 	 <xsl:otherwise>
			<xsl:value-of select="0"/>
 	 </xsl:otherwise>
	</xsl:choose>
</xsl:variable>
<xsl:variable name="hours">
	<xsl:choose>
		<xsl:when test="contains(@EndTime,'H')">
			<xsl:value-of select="substring-after(substring-before(@EndTime,'H'),'T')"/>
  	</xsl:when>
  	<xsl:otherwise>
			<xsl:text>0</xsl:text>
  	</xsl:otherwise>
	</xsl:choose>
</xsl:variable>	
<xsl:variable name="minutes">
	<xsl:choose>
  	<xsl:when test="contains(@EndTime,'M')">
  		<xsl:choose>
  			<xsl:when test="contains(@EndTime,'H')">
					<xsl:value-of select="substring-after(substring-before(@EndTime,'M'),'H')"/>
				</xsl:when>
  			<xsl:otherwise>
					<xsl:value-of select="substring-after(substring-before(@EndTime,'M'),'T')"/>
  			</xsl:otherwise>
  		</xsl:choose>
  	</xsl:when>
  	<xsl:otherwise>
			<xsl:text>0</xsl:text>
  	</xsl:otherwise>
	</xsl:choose>
</xsl:variable>
<xsl:value-of select="concat(($days * 24 + $hours),' ',($minutes))"/>
<xsl:text> HOUR,MINUTE&#10;</xsl:text>
</xsl:template>

<xsl:template match="SourcePosition">
	<xsl:choose>
  	<xsl:when test="@HeightAboveGroundMax &gt; 0">
  		<xsl:value-of select="@HeightAboveGround"/><xsl:text> </xsl:text><xsl:value-of select="@HeightAboveGroundMax"/>
  	</xsl:when>
 		<xsl:otherwise>
  		<xsl:text>0 </xsl:text><xsl:value-of select="@HeightAboveGround"/>
 		</xsl:otherwise>
	</xsl:choose>
 	<xsl:text> RELEASE HEIGHT MIN,MAX&#10;</xsl:text>	
</xsl:template>

<xsl:template match="SourceStrength">
	<xsl:value-of select="key('isoreg',@ItemName)/@ItemNum"/><xsl:text> </xsl:text><xsl:value-of select="round(./BinStrength/@Value)"/><xsl:text>&#10;</xsl:text>
</xsl:template>

</xsl:stylesheet>