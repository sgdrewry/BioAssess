# BioAssess


This package furthers the functions of rgbif, an R package that gives the user access to Global Biodiversity Informational Facility's data.
Once GBIF's data is download, this package allows the user to comprensively assess biodiversity within a set location.

The first step for using this package is to define your location where you wish to assess biodiversity. Visit https://geojson.io/#map=2/0/20, select a polygon shape, and create a polygon.
Once your polygon is created, save it as a WKT by clicking Save in the top left corner then selecting WKT. This will download your polygon's WKT.

rgbif can next be used to download the GBIF data present within this polygon based on desired parameters, such as date. 
