<?xml version="1.0" encoding="UTF-8" standalone="no"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.1"
    xmlns:btxml="https://github.com/MarkusLeupold/bibtex-xml">
<xsl:output method="html" html-version="5" encoding="UTF-8" indent="yes"
    doctype-system="about:legacy-compat" omit-xml-declaration="yes"/>

<xsl:template match="/">
    <html lang="en">
        <head>
            <title>Bibliography database</title>
            <link rel="stylesheet" href="../style.css"/>
        </head>
        <body>
            <header>
                <h1>Bibliography database</h1>
                <nav>
                    <ul>
                        <li>
                            <a href="#all-sources">
                                All sources
                            </a>
                        </li>
                        <li>
                            <a href="#collection-overview">
                                Collection overview
                            </a>
                        </li>
                    </ul>
                </nav>
            </header>
            <main>
                <xsl:apply-templates select="*"/>
            </main>
        </body>
    </html>
</xsl:template>

<xsl:variable name="collection-overview-reference-prefix"
    select="'collection-overview-'"/>

<xsl:template match="btxml:database">
    <section class="all-sources">
        <h2>
            All sources
            <a id="all-sources"/>
        </h2>
        <p>
            These are all sources contained in the database (the rows fold open
            to reveal more information when you click on them):
        </p>
        <table>
            <thead class="sticky">
                <tr>
                    <th>Type</th><th>Description</th>
                </tr>
            </thead>
            <tbody>
                <xsl:for-each select="//btxml:entry">
                    <xsl:sort select="@id"/>
                    <xsl:element name="tr">
                        <xsl:attribute name="class">
                            <xsl:text>entry </xsl:text>
                            <xsl:value-of select="@type"/>
                        </xsl:attribute>
                        <td>
                            <span class="plaintextmarkup openingbracket">
                                <xsl:text>[</xsl:text>
                            </span>
                            <xsl:call-template name="entry-type"/>
                            <span class="plaintextmarkup closingbracket">
                                <xsl:text>]</xsl:text>
                            </span>
                        </td>
                        <td><xsl:apply-templates select="."/></td>
                    </xsl:element>
                </xsl:for-each>
            </tbody>
        </table>
    </section>
    <hr/>
    <section class="collection-overview">
        <h2>
            Collection overview
            <a id="collection-overview"/>
        </h2>
        <p>
            This is an overview of the sources which are contained in some kind
            of collection, e.g. a book. Each section is a group of one
            collection and its children.
        </p>
        <xsl:call-template name="collection-overview"/>
    </section>
</xsl:template>

<xsl:template match="btxml:referencedValue">
    <!-- Expand a referencedValue to a single string. -->
    <xsl:variable name="name" select="@name"/>
    <!-- Pull the expanded value from the last declaration of the referenced
         string which was made before this referencedValue. -->
    <xsl:for-each select="preceding::btxml:string[@name=$name][1]">
        <xsl:call-template name="compute-value"/>
    </xsl:for-each>
</xsl:template>

<xsl:template match="btxml:literalValue">
    <!-- Expand a literalValue to a single string (i.e. its text content) -->
    <xsl:value-of select="."/>
</xsl:template>

<xsl:template name="compute-value">
    <!-- Expand composed BibTeX-XML values to their string representation.

         Values in BibTeX-XML are composed of literal and referenced values.
         To be able to use values as a selection criterion, we have to expand
         the value to a single string, which this template does.

         This template can be used to set the value of a variable to the string
         representation of a field's value. At the call of this template, the
         current node set must already contain the field or string node(s) which
         value(s) should be expanded. If the node set contains more than one
         node, the single expanded values will be concatenated. -->

    <xsl:apply-templates select="*"/>
</xsl:template>

<xsl:template name="entry-type">
    <xsl:element name="span">
        <xsl:attribute name="class">
            <xsl:text>entry-type </xsl:text>
            <xsl:value-of select="@type"/>
        </xsl:attribute>
        <xsl:apply-templates select="@type"/>
    </xsl:element>
</xsl:template>

<xsl:template name="field-name">
    <xsl:element name="span">
        <xsl:attribute name="class">
            <xsl:text>field-name </xsl:text>
            <xsl:value-of select="@name"/>
        </xsl:attribute>
        <xsl:apply-templates select="@name"/>
    </xsl:element>
</xsl:template>

<xsl:template name="field-value">
    <span class="field-value">
        <xsl:call-template name="format-field-value"/>
    </span>
</xsl:template>

<xsl:template match="btxml:field">
    <div class="field">
        <xsl:call-template name="field-name"/>
        <xsl:text>: </xsl:text>
        <xsl:call-template name="field-value"/>
    </div>
</xsl:template>

<xsl:template name="format-field-value">
    <xsl:choose>
        <xsl:when test="@name='title'">
            <cite class="value title">
                <xsl:call-template name="compute-value"/>
            </cite>
        </xsl:when>
        <xsl:otherwise>
            <xsl:element name="span">
                <xsl:attribute name="class">
                    <xsl:text>value </xsl:text>
                    <xsl:value-of select="@name"/>
                </xsl:attribute>
                <xsl:call-template name="compute-value"/>
            </xsl:element>
        </xsl:otherwise>
    </xsl:choose>
</xsl:template>

<xsl:template match="btxml:entry">
    <xsl:element name="details">
        <xsl:attribute name="id">
            <xsl:value-of select="@id"/>
        </xsl:attribute>
        <xsl:attribute name="class">
            <xsl:text>entry </xsl:text>
            <xsl:value-of select="@type"/>
        </xsl:attribute>
        <summary>
            <span>
                <xsl:for-each select="btxml:field[@name='author']">
                    <xsl:call-template name="format-field-value"/>
                </xsl:for-each>
                <xsl:text>. </xsl:text>
                <xsl:for-each select="btxml:field[@name='title']">
                    <xsl:call-template name="format-field-value"/>
                </xsl:for-each>
                <xsl:text>.</xsl:text>
            </span>
        </summary>
        <table>
            <xsl:for-each select="*[not(@name='author' or @name='title')]">
                <tr>
                    <th>
                        <xsl:call-template name="field-name"/>
                        <span class="plaintextmarkup colon">
                            <xsl:text>:</xsl:text>
                        </span>
                    </th>
                    <td><xsl:call-template name="field-value"/></td>
                </tr>
            </xsl:for-each>
        </table>
    </xsl:element>
</xsl:template>


<xsl:template name="collection-overview">
    <!-- Build an overview of all sources which are part of a book. The sources
         are grouped by the title of their parent book. -->

    <!-- To only print every collection once, we have to do some pretty
         complicated things in this template. These are the steps we wanna take:
          1. Get all collection titles by collecting all booktitle fields from
             the source document. Because there can be multiple entry elements
             with the same booktitle field (sources from the same collection),
             we have to filter out duplicates.
          2. For each collection title, select all incollection or inproceedings
             entries with this collection title in their booktitle field.
          3. Print a section, containing the collection title as a headline and
             the corresponding sources as content.

         This stylesheet is designed to work with XPath 1.0 where we have no
         distinct-values() function. Therefore, we need to use a functional
         programming approach to keep track of which collection titles are
         already processed. A counter is used to select one title after the
         other.
         With each call of this template, the complete list of collection titles
         is created from scratch, because in XSLT 1.1 it is not possible to pass
         node lists as parameters when calling a template.
         When the titles have been created, they get sorted. At this point, it
         seems to be impossible to expand the values, so we are a bit lazy and
         use only the unexpanded values. (This will have some weird result when
         a field's first value element is a referencedValue element, because the
         sorting algorithm will take its first character from somewhere in the
         middle of the actual expanded value.)
         After sorting, we take only the n-th title from the title list, expand
         it and store it inside a variable. We have successfully obtained the
         n-th collection title from the database. Remember, we only did that to
         check if we have already processed this title.
         We test if the obtained title is contained in our list of already
         processed titles (i.e. 'excludetitles'). If yes, we step up the counter
         and make a recursive call to process the next title. Otherwise, we need
         to process the title. This is done like follows:
          1. Search for an entry, which has the current collection title inside
             its normal title field. If such an entry exists, it has to be the
             actual collection which the entries referencing the current title
             are coming from. To provide some information about the collection,
             we print its entry. If we can't find any matching entry, print a
             small message that no further information about the collection is
             available.
          2. Select all field elements where the name attribute is 'booktitle'.
          3. For each of these, expand its value and store it in a variable.
             Test, if the expanded booktitle is equal to the booktitle, that we
             are currently processing. If they are equal, we have found the
             booktitle field of an entry which must belong to the current
             collection. Print the parent element, which is this exact entry.
         After printing each matching entry, step up the counter, concatenate
         the current collection title to the list of already processed titles
         and make a recursive call to process the next title.

         When the counter exceeds the total number of booktitle fields, we are
         done. Print nothing and return.
    -->

    <!-- excludetitles - The titles of books to exclude from the book overview,
         seperated by a comma without extra white space. This is mainly for the
         internal functionality of this template, but you can also pass a few
         titles to this parameter manually to affect the result. -->
    <xsl:param name="excludetitles" select="''"/>
    <!-- sourceindex - The counter which tells the template, at which source it
         should begin creating the overview. It is stepped up at each recursive
         call. -->
    <xsl:param name="sourceindex" select="1"/>
    <!-- booktitlecount - The total abount of excludetitles
         The default value will be used at the first call of this template. Each
         subsequent (recursive) call will pass this value as a parameter, so
         it doesn't have to be calculated on and on again. -->
    <xsl:param name="booktitlecount"
        select="count(//btxml:field[@name='booktitle'])"/>

    <xsl:if test="$sourceindex &lt;= $booktitlecount">
        <xsl:variable name="currenttitle">
            <!-- Use a for-each for calling the 'compute-value' template with
                 exactly one node. (for-each will change the context to only the
                 one single node we want) -->
            <xsl:for-each
                select="/descendant::btxml:field[
                            @name='booktitle'
                            and (    parent::*/@type='inproceedings'
                                  or parent::*/@type='incollection'
                                )
                        ]">
                <xsl:sort select="."/>
                <xsl:if test="position()=$sourceindex">
                    <xsl:call-template name="compute-value"/>
                </xsl:if>
            </xsl:for-each>
        </xsl:variable>
        <xsl:if test="not(contains($excludetitles, $currenttitle))">
            <section class="book">
                <h3><xsl:value-of select="$currenttitle"/></h3>
                <!-- This block for specific information about the collection
                     contains a mistake: The collection is searched for by its
                     unexpanded title but must be searched for by its expanded
                     title. -->
                <xsl:choose>
                    <xsl:when
                        test="0 &lt;
                              count( //field[ @name='title'
                                              and text()=$currenttitle
                                            ]
                                     /parent::*
                                   )">
                        <h4>
                            <xsl:text>
                                Sources that are contained in this collection:
                            </xsl:text>
                        </h4>
                        <xsl:apply-templates
                            select="//field[ @name='title'
                                             and text()=$currenttitle
                                           ]
                                    /parent::*"/>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:element name="p">
                            No specific information about this collection is
                            available. These are the known sources contained in
                            it:
                        </xsl:element>
                    </xsl:otherwise>
                </xsl:choose>
                <xsl:for-each
                    select="//btxml:entry[    @type='inproceedings'
                                           or @type='incollection'
                                         ]
                            /btxml:field[@name='booktitle']">
                    <xsl:variable name="t">
                        <xsl:call-template name="compute-value"/>
                    </xsl:variable>
                    <xsl:if test="$currenttitle=$t">
                        <xsl:apply-templates select="parent::*"/>
                    </xsl:if>
                </xsl:for-each>
            </section>
        </xsl:if>
        <xsl:call-template name="collection-overview">
            <xsl:with-param name="excludetitles"
                select="concat($excludetitles, ',', $currenttitle)"/>
            <xsl:with-param name="sourceindex" select="$sourceindex+1"/>
            <xsl:with-param name="booktitlecount" select="$booktitlecount"/>
        </xsl:call-template>
    </xsl:if>
</xsl:template>


<!-- A few simple string conversions for better language -->

<xsl:template match="btxml:entry[@type='article']/@type">
    <xsl:text>Article</xsl:text>
</xsl:template>
<xsl:template match="btxml:entry[@type='book']/@type">
    <xsl:text>Book</xsl:text>
</xsl:template>
<xsl:template match="btxml:entry[@type='booklet']/@type">
    <xsl:text>Booklet</xsl:text>
</xsl:template>
<xsl:template match="btxml:entry[@type='conference']/@type">
    <xsl:text>In conference proceedings</xsl:text>
</xsl:template>
<xsl:template match="btxml:entry[@type='inbook']/@type">
    <xsl:text>In a book</xsl:text>
</xsl:template>
<xsl:template match="btxml:entry[@type='incollection']/@type">
    <xsl:text>In a collection</xsl:text>
</xsl:template>
<xsl:template match="btxml:entry[@type='inproceedings']/@type">
    <xsl:text>In proceedings</xsl:text>
</xsl:template>
<xsl:template match="btxml:entry[@type='manual']/@type">
    <xsl:text>Manual</xsl:text>
</xsl:template>
<xsl:template match="btxml:entry[@type='mastersthesis']/@type">
    <xsl:text>Master&#8217;s thesis</xsl:text>
</xsl:template>
<xsl:template match="btxml:entry[@type='misc']/@type">
    <xsl:text>Miscellaneous</xsl:text>
</xsl:template>
<xsl:template match="btxml:entry[@type='phdthesis']/@type">
    <xsl:text>PhD thesis</xsl:text>
</xsl:template>
<xsl:template match="btxml:entry[@type='proceedings']/@type">
    <xsl:text>Proceedings</xsl:text>
</xsl:template>
<xsl:template match="btxml:entry[@type='techreport']/@type">
    <xsl:text>Tech report</xsl:text>
</xsl:template>
<xsl:template match="btxml:entry[@type='unpublished']/@type">
    <xsl:text>Unpublished</xsl:text>
</xsl:template>

<xsl:template match="btxml:field[@name='address']/@name">
    <xsl:text>Address</xsl:text>
</xsl:template>
<xsl:template match="btxml:field[@name='author']/@name">
    <xsl:text>Author</xsl:text>
</xsl:template>
<xsl:template match="btxml:field[@name='booktitle']/@name">
    <xsl:text>Contained in</xsl:text>
</xsl:template>
<xsl:template match="btxml:field[@name='chapter']/@name">
    <xsl:text>Chapter</xsl:text>
</xsl:template>
<xsl:template match="btxml:field[@name='edition']/@name">
    <xsl:text>Edition</xsl:text>
</xsl:template>
<xsl:template match="btxml:field[@name='editor']/@name">
    <xsl:text>Editor(s)</xsl:text>
</xsl:template>
<xsl:template match="btxml:field[@name='howpublished']/@name">
    <xsl:text>How it is published</xsl:text>
</xsl:template>
<xsl:template match="btxml:field[@name='institution']/@name">
    <xsl:text>Institution</xsl:text>
</xsl:template>
<xsl:template match="btxml:field[@name='isbn']/@name">
    <xsl:text>ISBN</xsl:text>
</xsl:template>
<xsl:template match="btxml:field[@name='journal']/@name">
    <xsl:text>In journal</xsl:text>
</xsl:template>
<xsl:template match="btxml:field[@name='month']/@name">
    <xsl:text>Month of publication</xsl:text>
</xsl:template>
<xsl:template match="btxml:field[@name='note']/@name">
    <xsl:text>Note</xsl:text>
</xsl:template>
<xsl:template match="btxml:field[@name='number']/@name">
    <xsl:text>Number</xsl:text>
</xsl:template>
<xsl:template match="btxml:field[@name='organization']/@name">
    <xsl:text>Organization</xsl:text>
</xsl:template>
<xsl:template match="btxml:field[@name='pages']/@name">
    <xsl:text>Page numbers</xsl:text>
</xsl:template>
<xsl:template match="btxml:field[@name='publisher']/@name">
    <xsl:text>Publisher</xsl:text>
</xsl:template>
<xsl:template match="btxml:field[@name='school']/@name">
    <xsl:text>School/University</xsl:text>
</xsl:template>
<xsl:template match="btxml:field[@name='series']/@name">
    <xsl:text>Series</xsl:text>
</xsl:template>
<xsl:template match="btxml:field[@name='title']/@name">
    <xsl:text>Title</xsl:text>
</xsl:template>
<xsl:template match="btxml:field[@name='type']/@name">
    <xsl:text>Type</xsl:text>
</xsl:template>
<xsl:template match="btxml:field[@name='volume']/@name">
    <xsl:text>Volume</xsl:text>
</xsl:template>
<xsl:template match="btxml:field[@name='year']/@name">
    <xsl:text>Year of publication</xsl:text>
</xsl:template>

</xsl:stylesheet>
