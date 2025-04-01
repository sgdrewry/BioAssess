#' filter_biodiversitydata
#'
#' This function filters GBIF data.
#' 
#' @param data data.frame. Containing geographical coordinates and species names.
#' @param lon character string. The column with the longitude coordinates. Default = “decimalLongitude”.
#' @param lat character string. The column with the latitude coordinates. Default = “decimalLatitude”.
#' @param species a character string. A vector of the same length as rows in x, with the species identity for each record. If NULL, tests must not include the "outliers" or "duplicates" tests.
#' @param countries a character string. The column with the country assignment of each record in three letter ISO code. Default = “countrycode”. If missing, the countries test is skipped.
#' @param tests tests	a vector of character strings, indicating which tests to run. See details for all tests available. Default = c("capitals", "centroids", "equal", "gbif", "institutions", "outliers", "seas", "zeros")
#' @param captials_rad numeric. The radius around capital coordinates in meters. Default = 10000.
#' @param centroids_rad numeric. The radius around centroid coordinates in meters. Default = 1000.
#' @param centroids_detail a character string. If set to ‘country’ only country (adm-0) centroids are tested, if set to ‘provinces’ only province (adm-1) centroids are tested. Default = ‘both’.
#' @param inst_rad numeric. The radius around biodiversity institutions coordinates in metres. Default = 100.
#' @param outliers_method The method used for outlier testing. See details.
#' @param outliers_mtp numeric. The multiplier for the interquartile range of the outlier test. If NULL outliers.td is used. Default = 5.
#' @param outliers_td numeric. The minimum distance of a record to all other records of a species to be identified as outlier, in km. Default = 1000.
#' @param outliers_size numeric. The minimum number of records in a dataset to run the taxon-specific outlier test. Default = 7.
#' @param range_rad buffer around natural ranges. Default = 0.
#' @param zeros_rad numeric. The radius around 0/0 in degrees. Default = 0.5.
#' @param captials_ref a data.frame with alternative reference data for the country capitals test. If missing, the countryref dataset is used. Alternatives must be identical in structure.
#' @param centroids_ref a data.frame with alternative reference data for the centroid test. If NULL, the countryref dataset is used. Alternatives must be identical in structure.
#' @param country_ref a SpatVector as alternative reference for the countries test. If NULL, the rnaturalearth:ne_countries('medium', returnclass = "sf") dataset is used.
#' @param country_refcol the column name in the reference dataset, containing the relevant ISO codes for matching. Default is to "iso_a3_eh" which referes to the ISO-3 codes in the reference dataset. See notes.
#' @param country_buffer numeric. Units are in meters. If provided, a buffer is created around each country polygon.
#' @param inst_ref data.frame with alternative reference data for the biodiversity institution test. If NULL, the institutions dataset is used. Alternatives must be identical in structure.
#' @param range_ref a SpatVector of species natural ranges. Required to include the 'ranges' test. See cc_iucn for details.
#' @param seas_ref a SpatVector as alternative reference for the seas test. If NULL, the rnaturalearth::ne_download(scale = 110, type = 'land', category = 'physical', returnclass = "sf") dataset is used.
#' @param seas_scale The scale of the default landmass reference. Must be one of 10, 50, 110. Higher numbers equal higher detail. Default = 50.
#' @param seas_buffer numeric. Units are in meters. If provided, a buffer is created around sea polygon.
#' @param urban_ref a SpatVector as alternative reference for the urban test. If NULL, the test is skipped. See details for a reference gazetteers.
#' @param aohi_rad numeric. The radius around aohi coordinates in meters. Default = 1000.
#' @param value a character string defining the output value. See the value section for details. one of ‘spatialvalid’, ‘summary’, ‘clean’. Default = ‘spatialvalid’.
#' @param verbose logical. If TRUE reports the name of the test and the number of records flagged.
#' @param report logical or character. If TRUE a report file is written to the working directory, summarizing the cleaning results. If a character, the path to which the file should be written. Default = FALSE.
#' @param coordinate_uncertainty_threshold numeric. Threshold for coordinate uncertainty to be filtered out, in meters. Default = 1600 meters (1 mile).
#' @param human_observation logical. If FALSE human observations will be filtered out. Default = TRUE.
#' @param observation logical. If FALSE  observations will be filtered out. Default = TRUE.
#' @param preserved_specimen logical. If FALSE preserved specimen will be filtered out. Default = TRUE.
#' @param machine_observation logical. If FALSE machine observations will be filtered out. Default = TRUE.
#' @param material_sample logical. If FALSE material samples will be filtered out. Default = TRUE.
#' @param living_specimen logical. If FALSE living specimen will be filtered out. Default = TRUE.
#' @param occurrence logical. If FALSE occurrences will be filtered out. Default = TRUE.
#' @param na_treatment_precision logical. If FALSE NAs for coordinate precision will be filtered out. Default = TRUE.
#' @param na_treatment_base logical. If FALSE NAs for basis of record will be filtered out. Default = TRUE.
#' @param output_incertae_file character string. Name of the file to output Incertae sedis records to.
#' @param output_clean_file character string. Name of the file to output the clean records to.
#' @param output_flagged_file character string. Name of the file to output the flagged records to.
#' 
#' @return A dataframe with a filtered biodiversity data.
#' 
#' @export
filter_biodiversitydata <- function(data,
                                    lon = "decimalLongitude",
                                    lat = "decimalLatitude",
                                    species = "species",
                                    countries = NULL,
                                    tests = c("capitals", "centroids", "equal", "gbif", "institutions", "outliers", "seas","zeros"),
                                    capitals_rad = 10000,
                                    centroids_rad = 1000,
                                    centroids_detail = "both",
                                    inst_rad = 100,
                                    outliers_method = "quantile",
                                    outliers_mtp = 5,
                                    outliers_td = 1000,
                                    outliers_size = 7,
                                    range_rad = 0,
                                    zeros_rad = 0.5,
                                    capitals_ref = NULL,
                                    centroids_ref = NULL,
                                    country_ref = NULL,
                                    country_refcol = "iso_a3",
                                    country_buffer = NULL,
                                    inst_ref = NULL,
                                    range_ref = NULL,
                                    seas_ref = NULL,
                                    seas_scale = 50,
                                    seas_buffer = NULL,
                                    urban_ref = NULL,
                                    aohi_rad = NULL,
                                    value = "spatialvalid",
                                    verbose = TRUE,
                                    report = FALSE,
                                    coordinate_uncertainty_threshold = 1610,  # Default threshold in meters 1610 meters ~ 1 mile
                                    human_observation = TRUE, 
                                    observation = TRUE, 
                                    preserved_specimen = TRUE, 
                                    machine_observation = TRUE, 
                                    material_sample = TRUE, 
                                    living_specimen = TRUE, 
                                    occurrence = TRUE,
                                    na_treatment_precision = TRUE,  # Default: treat NA as valid
                                    na_treatment_base = TRUE,  # Default: treat NA as valid
                                    output_incertae_file = "incertaesedis.csv", 
                                    output_clean_file = "cleanrecords.csv", 
                                    output_flagged_file = "flaggedrecords.csv") {     
  
  ## Step 1: Coordinate_cleaner
  inst_ref$decimalLongitude <- as.numeric(as.character(inst_ref$decimalLongitude))
  inst_ref$decimalLatitude <- as.numeric(as.character(inst_ref$decimalLatitude))
  inst_ref <- inst_ref %>% drop_na(decimalLongitude)
  data <- as.data.frame(data)
  data$decimalLongitude <- as.numeric(data$decimalLongitude)
  data$decimalLatitude <- as.numeric(data$decimalLatitude)
  clean_coordinates_data <- clean_coordinates(x = data,
                                              lon = lon,
                                              lat = lat,
                                              species = species,
                                              countries = countries,
                                              tests = tests,
                                              capitals_rad = capitals_rad,
                                              centroids_rad = centroids_rad,
                                              centroids_detail = centroids_detail,
                                              inst_rad = inst_rad,
                                              outliers_method = as.character(outliers_method),
                                              outliers_mtp = outliers_mtp,
                                              outliers_td = outliers_td,
                                              outliers_size = outliers_size,
                                              range_rad = range_rad,
                                              zeros_rad = zeros_rad,
                                              capitals_ref = capitals_ref,
                                              centroids_ref =  centroids_ref,
                                              country_ref = country_ref,
                                              country_refcol = as.character(country_refcol),
                                              country_buffer = country_buffer,
                                              inst_ref = inst_ref,
                                              range_ref = range_ref,
                                              seas_ref = seas_ref,
                                              seas_scale = seas_scale,
                                              seas_buffer = seas_buffer,
                                              urban_ref = urban_ref,
                                              aohi_rad = aohi_rad,
                                              value = as.character(value),
                                              verbose = verbose,
                                              report = report)
  
  ## Step 2: Filter based on basis of record and coordinate precision
  # Combine the options into a list
  basis_of_record_options <- c(
    if (human_observation) "HUMAN_OBSERVATION",
    if (observation) "OBSERVATION",
    if (preserved_specimen) "PRESERVED_SPECIMEN",
    if (machine_observation) "MACHINE_OBSERVATION",
    if (material_sample) "MATERIAL_SAMPLE",
    if (living_specimen) "LIVING_SPECIMEN",
    if (occurrence) "OCCURRENCE"
  )
  
  # Apply filtering and create new columns
  clean_coordinates_data$coordinateUncertaintyInMeters <- as.numeric(clean_coordinates_data$coordinateUncertaintyInMeters)
  clean_coordinates_data  <- clean_coordinates_data  %>%
    mutate(
      coordinate_precision = coordinateUncertaintyInMeters <= coordinate_uncertainty_threshold |
        (is.na(coordinateUncertaintyInMeters) & na_treatment_precision),
      
      record_basis = basisOfRecord %in% basis_of_record_options |
        (is.na(basisOfRecord) & na_treatment_base)
    )
  
  ## Step 3: Filter out 'incertae sedis' and process clean and flagged records
  # Filter 'incertae sedis' records
  incertae_sedis <- which(clean_coordinates_data [, 1] == "incertae sedis")
  incertae_sedis_count <- length(incertae_sedis)
  
  if (incertae_sedis_count > 0) {
    #TODO -- Remove the rows with 'incertae sedis' from the dataset as adding to own dataframe to save (loop through and remove and add to new dataframe)
    incertae_sedis_rows <- clean_coordinates_data [incertae_sedis, ]
    write.csv(incertae_sedis_rows, output_incertae_file, row.names = FALSE)
    message(incertae_sedis_count, " 'incertae sedis' records found and saved to: ", output_incertae_file)
    
    # Remove these rows from the dataset
    
  } else {
    message("No 'incertae sedis' records found.")
  }
  clean_coordinates_noIS_data  <- clean_coordinates_data [-incertae_sedis, ]
  # Step 4: Flag records based on coordinate_precision, record_basis, and .summary
  
  # Flag records where any of the conditions are not met
  flagged_data <- clean_coordinates_noIS_data %>%
    filter(!coordinate_precision | !record_basis | !clean_coordinates_noIS_data$.summary)
  
  # Create clean data by selecting records where all conditions are met
  clean_data <- clean_coordinates_noIS_data %>%
    filter(coordinate_precision & record_basis & clean_coordinates_noIS_data$.summary)
  
  # Count flagged records based on coordinate_precision and record_basis
  flagged_precision_count <- nrow(flagged_data %>% filter(!coordinate_precision))
  flagged_base_count <- nrow(flagged_data %>% filter(!record_basis))
  
  # Output the counts of flagged records for these two categories
  message(flagged_precision_count, " records were flagged for coordinate precision.")
  message(flagged_base_count, " records were flagged for basis of record.")
  
  # Save the clean data to a CSV
  clean_count <- nrow(clean_data)
  clean_data <- clean_data[, !(names(clean_data) %in% c(".val", ".equ", ".zer", ".cap", ".cen", ".inst", ".summary", "coordinate_precision", "record_basis"))]
  write.csv(clean_data, output_clean_file, row.names = FALSE)
  message(clean_count, " clean records saved to: ", output_clean_file)
  
  # Save the flagged data to a CSV
  flagged_count <- nrow(flagged_data)
  write.csv(flagged_data, output_flagged_file, row.names = FALSE)
  message(flagged_count, " flagged records saved to: ", output_flagged_file)
  
  # Return both filtered and flagged datasets
  return(list(filtered_data = clean_data, flagged_data = flagged_data))
  
  # Print summary information
  message("Number of records in filtered dataset: ", filtered_count)
  message("Number of records in flagged dataset: ", flagged_count)
}
