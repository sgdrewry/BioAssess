# BioAssess

This package offers a comprehensive assessment of biodiversity at user-specified locations anywhere in the United States, providing a detailed overview of biodiversity and associated risks. It supports the identification of areas where risks to biodiversity can be minimized and opportunities to enhance biodiversity can be maximized

This package extends the functionality of the rgbif R package, which provides access to the Global Biodiversity Information Facility (GBIF) data. Once GBIF data is downloaded, this package enables users to conduct a comprehensive biodiversity assessment for that location.

The first step in using this package is to define the location for your biodiversity assessment. Visit geojson.io, select a polygon shape, and create your desired polygon. Once the polygon is created, save it as a WKT by clicking 'Save' in the top left corner and selecting 'WKT'. This will download your polygon's WKT file.

Next, you can use the rgbif package to download GBIF data within your defined polygon. This is done using the functions occ_download, occ_download_wait, occ_download_get, and occ_download_import. A GBIF account is required to access these functions; you can create one at https://gbif.org/. Once your account is set up, you can run the following code:



polygon <- "(your download WKT)" 

key <- occ_download(
  pred_in("hasGeospatialIssue", "FALSE"),
  pred_in("occurrenceStatus", "PRESENT"),
  pred_in("hasCoordinate", "TRUE"),
  pred_within(polygon),
  pred_gte("year", 2008),  # Filter for years greater than or equal to 2008
  pred_lte("year", 2024),  # Filter for years less than or equal to 2024
  format = "DWCA",
  user = "username",       # Username for GBIF
  pwd = "password",        # Password for GBIF
  email = "email"         # Email for GBIF
)

There are other presets available for occ_download, depening on the filters the user wishes to impose on the data. For the purposes of this package, we recommend the arguments specified above. 

meta <- occ_download_wait(key, status_ping = 5, curlopts = list(), quiet = FALSE)
meta_get <- occ_download_get(key)
meta_data <- occ_download_import(meta_get)

There are certain columns that are necessary for BioAssess's functions. We will select those columns from the meta data.

Data <- meta_data %>% select(scientificName, decimalLatitude, decimalLongitude, issue, basisOfRecord, individualCount, kingdom, phylum, order, family, genus, species, verbatimScientificName, taxonRank, year, month, day, license, class, collectionCode, institutionCode, countryCode, gbifID, coordinateUncertaintyInMeters, datasetKey)



Now, we can assess biodiversity using the BioAssess package.

We need to filter the data to ensure all species including in the polygon are accurate. For this, we will use the function filter_biodiversitydata. This function combines CoordinateCleaner's clean_coordinates function with some additional filtering options.








We will generate an information sheet for the polygon, which will include details about all species found within the defined area.

