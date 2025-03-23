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

The function clean_coordinates from CoordinateCleaner can filter biodiversity data from GBIF based on their coordinates. This function focuses on removing potentially erronously georeferenced data, such as coordiantes close to country centroids, capitals, and biodiversity institutions. Biodiversity institutions (zoos, botanic gardens, etc) can inaccurately inflat a polyon's biodiversity. Thus, CoordinateCleaner offers an option to remove any observations within a specified range of these institutions. The package has a dataframe, "institutions", that lists many of these biodiversity institutions. We have found that, on a smaller scale that we are interested in, there are institutions that are missing from the list. We have created a function that allows users to add institutions to CoordinateCleaner's dataframe "institutions".

install.packages("CoordinateCleaner")
library("CoordinateCleaner")
data("institutions")

Remove any institutions without coordinates
institutions <- institutions %>% drop_na(decimalLongitude)

add_biodiversity_institution(name, longitude, latitude, city, country, address, source, institution_type, geocoding_precision, geocoding_issue, geocoding_source, inside_protected_area) 

Really, the only arguments needed are longitude, latitude, and name. The other arguments are to keep things organized.

name = (character) Name of the biodiversity institution
longitude = (numeric) Longitude of the center of the institution
latitude = (numeric) Longitude of the center of the institution
city = (character) City the institution is found in
country = (character) Country the institution is found in
address = (character) Address of the institution
source = ?
institution_type = (character) Institution type. Ex: zoo, herbarium, botanic garden
geocoding_precision = ?
geocoding_issue = ?
geocoding_source = ?
inside_protected_area = ? 


Once all biodiversity instiutions found in the polygon have been added to the institutions dataframe, we can now filter the GBIF data with the function filter_biodiversitydata. This function incorporates the filtering that the CoordinateCleaner's function clean_coordinates does, with additional functionality that filters based on coordinate uncertainity and basis of records. See filter_biodiversitydata's documentation for further information.

The output of filter_biodiversitydata is two lists, one containing the filtered data and one containing the flagged data. 

As an example of calling the function with data:
result <- filter_biodiversitydata(data = Data, tests = c("capitals", "centroids", "equal", "institutions", "zeros"), inst_ref = institutions)

Access the filtered data
metadata_filtered_data <- result$filtered_data

Access the flagged data
metadata_flagged_data <- result$flagged_data

The function generatinginfosheet takes this filtered data and creates an information sheet for the polygon containing all of the biodiversity data. This function outputs a dataframe with the species present in the polygon. The dataframe includes whether the species is consideed invasive in the location specified, according to the Global Database of Invasive Species and the USGS Nonindigenous Aquatic Species database. The information is location-specific, so the user needs to specific which state(s) their polygon exists in (up to four states). The dataframe will also including information about endangerment statuses for the species, according to the ICUN and NatureServe. Because statuses depend heavily on location, the information sheet will have an ICUN status, a NatureServe global status, and NatureServe state-specific status(es). See the documentation for generatinginfosheet for further information.


Because each species on the information sheet will have more than one endangerment status, the function comparingstatuses was created to help the user pick a "final" status for each species. There are two options for comparison of endangerment statuses, "least concern" and "most concern". If "most concern" is chosen, a column will be added to the information sheet which outputs the endangerment status of most concern for each species to be used as their "final" status. If "least concern" is chosen, this column will output the status that is of least concern for each species. See the documentation for comparingstatuses for further information.
