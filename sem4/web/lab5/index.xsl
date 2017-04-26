<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:template match="/">


<html>
    <head>
    <!-- Latest compiled and minified CSS -->
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u" crossorigin="anonymous"></link>

    <!-- Optional theme -->
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css" integrity="sha384-rHyoN1iRsVXV4nD0JutlnGaslCJuC7uwjduW9SVrLvRYooPp2bWYgmgJQIXwl/Sp" crossorigin="anonymous"></link>
    </head>

    <body>

        <div class="col-md-3"></div>
        <div class="col-md-6">
          <div align="center">
              <h1><span class="label label-primary">My CD Collection</span></h1>
          </div>
          <br />

          <table class="table table-bordered table-condensed">
            <tr>
              <th style="text-align:left">Title</th>
              <th style="text-align:left">Artist</th>
            </tr>
            <xsl:for-each select="catalog/cd">
              <xsl:sort select="artist"/>

            <tr>
              <td class="alert alert-success"><xsl:value-of select="title"/></td>
              <td class="alert alert-info"><xsl:value-of select="artist"/></td>
            </tr>
            </xsl:for-each>
        </table>
      </div>
      <div class="col-md-3"></div>
    </body>
</html>
</xsl:template>
</xsl:stylesheet>


