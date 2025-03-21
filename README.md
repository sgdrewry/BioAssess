# BioAssess

This package offers a comprehensive assessment of biodiversity at a specified location anywhere in the United States, providing a detailed overview of biodiversity and associated risks. It supports the identification of areas where risks to biodiversity can be minimized and opportunities to enhance biodiversity can be maximized

This package extends the functionality of the rgbif R package, which provides access to the Global Biodiversity Information Facility (GBIF) data. Once GBIF data is downloaded, this package enables users to conduct a comprehensive biodiversity assessment for a specified location.

The first step in using this package is to define the location for your biodiversity assessment. Visit geojson.io, select a polygon shape, and create your desired polygon. Once the polygon is created, save it as a WKT by clicking 'Save' in the top left corner and selecting 'WKT'. This will download your polygon's WKT file.

rgbif can next be used to download the GBIF data present within this polygon based on desired parameters, such as date. 
