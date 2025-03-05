#' GeneratingInfoSheet
#'
#' This function generates an information sheet for all of the species in the polygon.
#'
#' @param data data.frame. Should be the filtered_data from filter_biodiversity_data.
#' @param state1 character string. The state the polygon is in. Only need states if mark_invasive_species, natureserve_statuses, or ICUNRedList_statuses are marked TRUE.
#' @param state2 character string. If the polygon is in 2 states, the second state the polygon is in.
#' @param state3 character string.  If the polygon is in 3 states, the third state the polygon is in.
#' @param state4 character string. If the piolygon is in 4 states, the fourth state the polygon is in. The limit is 4 states.
#' @param mark_invasive_species logical. When true, invasive species will be identified and marked
#' @param include_USinvasives_list logical. When true, the USInvasive list will be included when identifying/marking invasive species. The US Invasive List is simply a list of the most invasive species in the US. Not state-specific.
#' @param natureserve_statuses logical. When true, Natureserve statuses will be added to each species, for each state as well as the global status.
#' @param ICUNRedList_statuses logical. When true, ICUN Red List statuses will be added to each species.
#'
#' @return A dataframe with information for each species.
#'
#' @export

generatinginfosheet <- function(data, mark_invasive_species = TRUE, include_USinvasives_list = TRUE, state1 = NULL, state2 = NULL, state3 = NULL, state4 = NULL, natureserve_statuses = TRUE, ICUNRedList_statuses = TRUE) {
  
  if (is.null(data) || !is.data.frame(data)) {
    stop("The input data must be a non-null data frame.")
  }
  
  # Check if the necessary columns are present in the data
  required_columns <- c("scientificName", "taxonRank", "individualCount")
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop("The following required columns are missing in the input data: ", paste(missing_columns, collapse = ", "))
  }
  
  ## Fix BOLDs
  for (i in 1:nrow(data)) {
    if (grepl("^BOLD:", data[i, 1])) {
      if (!is.na(data[i, 12])) {  # Check species
        data[i, 1] <- data[i, 12]
      } else if (!is.na(data[i, 11])) {  # Check genus
        data[i, 1] <- data[i, 11]
      } else if (!is.na(data[i, 10])) {  # Check family
        data[i, 1] <- data[i, 10]
      } else if (!is.na(data[i, 9])) {  # Check order
        data[i, 1] <- data[i, 9]
      } else if (!is.na(data[i, 8])) {  # Check phylum
        data[i, 1] <- data[i, 8]
      } else if (!is.na(data[i, 7])) {  # Check kingdom
        data[i, 1] <- data[i, 7]
      }
    }
  }
  
  
  # Step 1: Get the number of unique species
  num_unique_species <- length(unique(data$scientificName))
  message(paste(num_unique_species, "unique species found"))
  
  # Step 2: Get the sorted unique species data
  unique_species <- data %>% distinct(scientificName, .keep_all=TRUE)
  sorted_unique_species <- unique_species[order(unique_species$scientificName, decreasing=FALSE),]
  
  # Step 3: Create InfoSheet from the sorted_unique_species data
  InfoSheet <- as.data.frame(cbind(
    sorted_unique_species$scientificName,
    sorted_unique_species$taxonRank,
    sorted_unique_species$family,
    sorted_unique_species$class,
    sorted_unique_species$datasetKey
  ))
  
  # Check if InfoSheet is created successfully
  message("Info sheet generated")
  
  
  # Set column names for the InfoSheet
  colnames(InfoSheet) <- c("Scientific Name", "Taxon Rank", "Family", "Class", "Dataset Key")
  
  # Step 4: Create a matrix with the 1st column as the unique species and 2nd column with occurrence count
  data$individualCount <- as.numeric(data$individualCount)
  unique_species_list <- InfoSheet$`Scientific Name`
  occurrence_matrix <- as.matrix(unique_species_list)
  occurrence_count <- matrix(data=as.numeric(rep(0,length(occurrence_matrix))))
  occurrence_count_matrix <- cbind(occurrence_matrix, occurrence_count)
  
  # Step 5: Sort entire occurrence matrix based on species names
  sorted_data <- data[order(data$scientificName, decreasing=FALSE),]
  
  # Step 6: Loop through the sorted data to produce the occurrence count
  current_species <- as.character("")
  x <- 0
  total <- 0
  
  for (i in 1:nrow(sorted_data)) {
    # Update species occurrence count
    if (sorted_data$scientificName[i] != current_species) {
      x <- x + 1
      current_species <- sorted_data$scientificName[i]
    }
    
    # Add the individual count if it's available
    if (is.na(sorted_data$individualCount[i]) == FALSE) {
      occurrence_count_matrix[x,2] <- as.numeric(occurrence_count_matrix[x,2]) + 
        as.numeric(sorted_data$individualCount[i])
      total <- total + as.numeric(sorted_data$individualCount[i])
    } else {
      # If individual count is missing, increment by 1
      occurrence_count_matrix[x,2] <- as.numeric(occurrence_count_matrix[x,2]) + as.numeric(1)
      total <- total + 1
    }
  }
  
  # Check if the total count matches the sum of occurrences
  message("Total occurrences counted: ", total)
  
  # Check if the totals match
  check_total <- sum(is.na(sorted_data$individualCount)) + 
    sum(sorted_data$individualCount, na.rm=TRUE)
  if (check_total != total) {
    message("Warning: Total occurrences do not match the expected occurrences. Something went wrong when adding occurrence counts to Info Sheet.")
  }
  
  # Step 7: Update the InfoSheet with the occurrence counts
  InfoSheet <- as.data.frame(cbind(InfoSheet, occurrence_count_matrix[,2]))
  
  # Check the updated InfoSheet
  message("Occurrence counts added to info sheet")
  
  # Update column names
  colnames(InfoSheet) <- c("Scientific Name", "Taxon Rank", "Family", "Class", "Dataset Key","Individual Count")
  
  ## Checking for invasive species
  if (mark_invasive_species == TRUE) {
    if (is.null(state1)) {
      stop("Error: Must provide at least a state 1 if natureserve_statuses is set to TRUE.") 
    }
    if (is.null(state2) && is.null(state3) && is.null(state4)) {
      
      # Construct the name of the state-specific invasive species list
      NASinvasive_list_name <- paste("NAS", state1,"2024", sep = "")
      # Dynamically get the invasive species list for the given state
      NASInvasiveSpeciesList <- get(NASinvasive_list_name)
      NASInvasiveSpeciesList <- as.character(NASInvasiveSpeciesList$`Scientific Name`)
      
      # Construct the name of the state-specific invasive species list
      EDDinvasive_list_name <- paste("EDDMaps", state1,"2024", sep = "")
      # Dynamically get the invasive species list for the given state
      EDDInvasiveSpeciesList <- get(EDDinvasive_list_name)
      EDDInvasiveSpeciesList <- as.character(EDDInvasiveSpeciesList$SciName)
      EDDInvasiveSpeciesList <- unique(EDDInvasiveSpeciesList)
      
      InvasiveSpeciesList <- c(NASInvasiveSpeciesList, EDDInvasiveSpeciesList)
      
      if (include_USinvasives_list) {
        if (!exists("InvasiveSpeciesUS")) {
          stop("Error: United States invasive species list (InvasiveSpeciesUS) is not available.")
        }
        # Get the U.S. invasive species list and merge it with the state-specific list
        # CANNOT HAVE ANY NAs
        USInvasiveSpeciesList <- get("InvasiveSpeciesUS")
        InvasiveSpeciesList <- union(InvasiveSpeciesList, USInvasiveSpeciesList) # Merge the lists
        InvasiveSpeciesList <- unique(InvasiveSpeciesList)
      }
      
      # Initialize a counter for the invasive species found
      invasive_count <- 0
      
      InfoSheet <- cbind(InfoSheet[, 1:5], NewColumn = NA, InfoSheet[, -(1:5)])
      
      # Update column names
      colnames(InfoSheet) <- c("Scientific Name", "Taxon Rank", "Family", "Class", "Dataset Key", "Invasive", "Individual Count")
      
      
      # Iterate over the species list
      for (i in 1:length(InvasiveSpeciesList)) {
        # Search for occurrences of each species name in the first column (scientific name)
        occ <- grep(InvasiveSpeciesList[i], InfoSheet[, 1], ignore.case = TRUE)
        
        # If occurrences are found, mark the 11th column as TRUE (indicating invasive)
        if (length(occ) > 0) {
          # Update the 11th column for each occurrence found
          for (j in occ) {
            InfoSheet[j, "Invasive"] <- TRUE
          }
          # Increment the counter by the number of occurrences found
          invasive_count <- invasive_count + 1
        }
      }
      
      # Print how many invasive species were found and marked
      message(paste(invasive_count, "invasive species were found and marked"))
      
      
      
    } else {
      if (is.null(state2) && !is.null(state3)) {
        stop("Error: Must provide a 2nd state if a 3rd state is provided when mark_invasive_species is set to TRUE.")
      }
      if (is.null(state3) && !is.null(state4)) {
        stop("Error: Must provide a 3rd state if a 4th state is provided when mark_invasive_species is set to TRUE.")
      }
      if (is.null(state2) && is.null(state3) && !is.null(state4)) {
        stop("Error: Must provide a 2nd state and 3rd state if a 4th state is provided when mark_invasive_species is set to TRUE.")
      }  
      else {
        
        # Construct the name of the state-specific invasive species list for state 1
        NASinvasive_list_nameState1 <- paste("NAS", state1,"2024", sep = "")
        # Dynamically get the invasive species list for the given state
        NASInvasiveSpeciesListState1 <- get(NASinvasive_list_nameState1)
        NASInvasiveSpeciesListState1 <- as.character(NASInvasiveSpeciesListState1$`Scientific Name`)
        
        # Construct the name of the state-specific invasive species list for state 2
        NASinvasive_list_nameState2 <- paste("NAS", state2,"2024", sep = "")
        # Dynamically get the invasive species list for the given state
        NASInvasiveSpeciesListState2 <- get(NASinvasive_list_nameState2)
        NASInvasiveSpeciesListState2 <- as.character(NASInvasiveSpeciesListState2$`Scientific Name`)
        
        if (!is.null(state3)) {
          # Construct the name of the state-specific invasive species list for state 3
          NASinvasive_list_nameState3 <- paste("NAS", state3,"2024", sep = "")
          # Dynamically get the invasive species list for the given state
          NASInvasiveSpeciesListState3 <- get(NASinvasive_list_nameState3)
          NASInvasiveSpeciesListState3 <- as.character(NASInvasiveSpeciesListState3$`Scientific Name`)
        }
        
        if (!is.null(state4)) {
          # Construct the name of the state-specific invasive species list for state 4
          NASinvasive_list_nameState4 <- paste("NAS", state4,"2024", sep = "")
          # Dynamically get the invasive species list for the given state
          NASInvasiveSpeciesListState4 <- get(NASinvasive_list_nameState4)
          NASInvasiveSpeciesListState4 <- as.character(NASInvasiveSpeciesListState4$`Scientific Name`)
        }
        
        # Construct the name of the state-specific invasive species list for state 1
        EDDinvasive_list_nameState1 <- paste("EDDMaps", state1,"2024", sep = "")
        # Dynamically get the invasive species list for the given state
        EDDInvasiveSpeciesListState1 <- get(EDDinvasive_list_nameState1)
        EDDInvasiveSpeciesListState1 <- as.character(EDDInvasiveSpeciesListState1$`SciName`)
        EDDInvasiveSpeciesListState1 <- unique(EDDInvasiveSpeciesListState1)
        
        # Construct the name of the state-specific invasive species list for state 2
        EDDinvasive_list_nameState2 <- paste("EDDMaps", state2,"2024", sep = "")
        # Dynamically get the invasive species list for the given state
        EDDInvasiveSpeciesListState2 <- get(EDDinvasive_list_nameState2)
        EDDInvasiveSpeciesListState2 <- as.character(EDDInvasiveSpeciesListState2$`SciName`)
        EDDInvasiveSpeciesListState2 <- unique(EDDInvasiveSpeciesListState2)
        
        if (!is.null(state3)) {
          # Construct the name of the state-specific invasive species list for state 3
          EDDinvasive_list_nameState3 <- paste("EDDMaps", state3,"2024", sep = "")
          # Dynamically get the invasive species list for the given state
          EDDInvasiveSpeciesListState3 <- get(EDDinvasive_list_nameState3)
          EDDInvasiveSpeciesListState3 <- as.character(EDDInvasiveSpeciesListState3$`SciName`)
          EDDInvasiveSpeciesListState3 <- unique(EDDInvasiveSpeciesListState3)
        }
        
        if (!is.null(state4)) {
          # Construct the name of the state-specific invasive species list for state 4
          EDDinvasive_list_nameState4 <- paste("EDDMaps", state4,"2024", sep = "")
          # Dynamically get the invasive species list for the given state
          EDDInvasiveSpeciesListState4 <- get(EDDinvasive_list_nameState4)
          EDDInvasiveSpeciesListState4 <- as.character(EDDInvasiveSpeciesListState4$`SciName`)
          EDDInvasiveSpeciesListState4 <- unique(EDDInvasiveSpeciesListState4)
        }
        
        if (is.null(state3) && is.null(state4)) {
          InvasiveSpeciesList <- c(NASInvasiveSpeciesListState1, EDDInvasiveSpeciesListState1, NASInvasiveSpeciesListState2, EDDInvasiveSpeciesListState2)
        }
        if (!is.null(state3) && is.null(state4)) {
          InvasiveSpeciesList <- c(NASInvasiveSpeciesListState1, EDDInvasiveSpeciesListState1, NASInvasiveSpeciesListState2, EDDInvasiveSpeciesListState2, NASInvasiveSpeciesListState3, EDDInvasiveSpeciesListState3)
        }
        if (!is.null(state3) && !is.null(state4)) {
          InvasiveSpeciesList <- c(NASInvasiveSpeciesListState1, EDDInvasiveSpeciesListState1, NASInvasiveSpeciesListState2, EDDInvasiveSpeciesListState2, NASInvasiveSpeciesListState3, EDDInvasiveSpeciesListState3, NASInvasiveSpeciesListState4, EDDInvasiveSpeciesListState4)
        }
        
        
        if (include_USinvasives_list) {
          if (!exists("InvasiveSpeciesUS")) {
            stop("Error: United States invasive species list (InvasiveSpeciesUS) is not available.")
          }
          # Get the U.S. invasive species list and merge it with the state-specific list
          # CANNOT HAVE ANY NAs
          USInvasiveSpeciesList <- get("InvasiveSpeciesUS")
          InvasiveSpeciesList <- unique(na.omit(c(InvasiveSpeciesList, USInvasiveSpeciesList))) # Merge the lists
        }
        
        # Initialize a counter for the invasive species found
        invasive_count <- 0
        
        InfoSheet <- cbind(InfoSheet[, 1:5], NewColumn = NA, InfoSheet[, -(1:5)])
        
        # Update column names
        colnames(InfoSheet) <- c("Scientific Name", "Taxon Rank", "Family", "Class", "Dataset Key", "Invasive", "Individual Count")
        
        
        # Iterate over the species list
        for (i in 1:length(InvasiveSpeciesList)) {
          # Search for occurrences of each species name in the first column (scientific name)
          occ <- grep(InvasiveSpeciesList[i], InfoSheet[, 1], ignore.case = TRUE)
          
          # If occurrences are found, mark the 11th column as TRUE (indicating invasive)
          if (length(occ) > 0) {
            # Update the 11th column for each occurrence found
            for (j in occ) {
              InfoSheet[j, "Invasive"] <- TRUE
            }
            # Increment the counter by the number of occurrences found
            invasive_count <- invasive_count + 1
          }
        }
        
        # Print how many invasive species were found and marked
        message(paste(invasive_count, "invasive species were found and marked"))
        
        
      }
    }  
  }
  
  ##NatureServe Statuses
  if (natureserve_statuses== TRUE) {
    if (is.null(state1)) {
      stop("Error: Must provide at least a state 1 if natureserve_statuses is set to TRUE.") 
    }
    if (is.null(state2) && is.null(state3) && is.null(state4)) {
      library(tidyr)
      
      # Construct the name of the state-specific invasive species list
      NatureServe_name <- paste("NatureServe", state1, "2024",sep = "")
      # Dynamically get the invasive species list for the given state
      NatureServeSheet <- get(NatureServe_name) 
      
      
      # Step 4: Select relevant columns
      species_rank <- NatureServeSheet[, c(1, 2, 7, 11)]
      species_rank <- as.data.frame(species_rank)
      
      # Step 2: Process Info Sheet
      InfoSheet <- data.frame(InfoSheet)
      InfoSheet <- cbind(InfoSheet[, 1:2], NewColumn = NA, NewColumn = NA, InfoSheet[, -(1:2)])
      # Assume 'state' is the user-defined variable
      state_column_name <- paste(state1, "NatureServe Status")
      
      if (mark_invasive_species == TRUE) {
        colnames(InfoSheet) <- c("Scientific Name", "Taxon Rank", state_column_name, 
                                 "NatureServe Rounded Global Rank", "Family", "Class", 
                                 "Dataset Key", "Invasive", "Individual Count")
      } else {
        colnames(InfoSheet) <- c("Scientific Name", "Taxon Rank", state_column_name, 
                                 "NatureServe Rounded Global Rank", "Family", "Class", 
                                 "Dataset Key", "Individual Count")
      }
      
      for (i in 1:nrow(species_rank)) {
        rank <- grep(species_rank[i, 2], InfoSheet[, 1], ignore.case = TRUE)
        for (j in rank) {
          InfoSheet[j, 3] <- species_rank[i, 4]
        }
      }
      
      message("NatureServe State-specific statuses added to info sheet")
      
      # Step 3: Add NatureServe Global Status
      for (i in 1:nrow(species_rank)) {
        rank <- grep(species_rank[i, 2], InfoSheet[, 1], ignore.case = TRUE)
        for (j in rank) {
          InfoSheet[j, 4] <- species_rank[i, 3]
        }
      }
      
      message("NatureServe Global statuses added to info sheet")
      
      
    } else {
      if (is.null(state2) && !is.null(state3)) {
        stop("Error: Must provide a 2nd state if a 3rd state is provided when natureserve_statuses is set to TRUE.")
      }
      if (is.null(state3) && !is.null(state4)) {
        stop("Error: Must provide a 3rd state if a 4th state is provided when natureserve_statuses is set to TRUE.")
      }
      if (is.null(state2) && is.null(state3) && !is.null(state4)) {
        stop("Error: Must provide a 2nd state and 3rd state if a 4th state is provided when natureserve_statuses is set to TRUE.")
      }  
      else {
        library(tidyr)
        
        
        # Construct the name of the state-specific invasive species list for state 1
        NatureServe_nameState1 <- paste("NatureServe", state1, "2024",sep = "")
        # Dynamically get the invasive species list for the given state
        NatureServeSheetState1 <- get(NatureServe_nameState1) 
        species_rankState1 <- NatureServeSheetState1[, c(1, 2, 7, 11)]
        species_rankState1 <- as.data.frame(species_rankState1)
        
        # Construct the name of the state-specific invasive species list for state 2
        NatureServe_nameState2 <- paste("NatureServe", state2, "2024",sep = "") 
        # Dynamically get the invasive species list for the given state
        NatureServeSheetState2 <- get(NatureServe_nameState2) 
        species_rankState2 <- NatureServeSheetState2[, c(1, 2, 7, 11)]
        species_rankState2 <- as.data.frame(species_rankState2)
        
        # Construct the name of the state-specific invasive species list for state 3
        if (!is.null(state3)) {
          NatureServe_nameState3 <- paste("NatureServe", state3, "2024",sep = "")
          # Dynamically get the invasive species list for the given state
          NatureServeSheetState3 <- get(NatureServe_nameState3) 
          species_rankState3 <- NatureServeSheetState3[, c(1, 2, 7, 11)]
          species_rankState3 <- as.data.frame(species_rankState3)
        }
        
        # Construct the name of the state-specific invasive species list for state 4
        if (!is.null(state4)) {
          NatureServe_nameState4 <- paste("NatureServe", state4, "2024",sep = "")
          # Dynamically get the invasive species list for the given state
          NatureServeSheetState4 <- get(NatureServe_nameState4) 
          species_rankState4 <- NatureServeSheetState4[, c(1, 2, 7, 11)]
          species_rankState4 <- as.data.frame(species_rankState4)
        }
        
        
        
        if (is.null(state3) && is.null(state4)) {
          InfoSheet <- cbind(InfoSheet[, 1:2], NewColumn = NA, NewColumn = NA, NewColumn = NA, InfoSheet[, -(1:2)])
          # Assume 'state' is the user-defined variable
          state1_column_name <- paste(state1, "NatureServe Status")
          state2_column_name <- paste(state2, "NatureServe Status")
          if (mark_invasive_species == TRUE) {
            colnames(InfoSheet) <- c("Scientific Name", "Taxon Rank", state1_column_name, state2_column_name,
                                     "NatureServe Rounded Global Rank", "Family", "Class", 
                                     "Dataset Key", "Invasive", "Individual Count")
          } else {
            colnames(InfoSheet) <- c("Scientific Name", "Taxon Rank", state1_column_name, state2_column_name,
                                     "NatureServe Rounded Global Rank", "Family", "Class", 
                                     "Dataset Key", "Individual Count")
          }
          
          for (i in 1:nrow(species_rankState1)) {
            rank <- grep(species_rankState1[i, 2], InfoSheet[, 1], ignore.case = TRUE)
            for (j in rank) {
              InfoSheet[j, 3] <- species_rankState1[i, 4]
            }
          }
          
          message("NatureServe State-specific statuses for state 1 added to info sheet")
          
          for (i in 1:nrow(species_rankState2)) {
            rank <- grep(species_rankState2[i, 2], InfoSheet[, 1], ignore.case = TRUE)
            for (j in rank) {
              InfoSheet[j, 4] <- species_rankState2[i, 4]
            }
          }
          
          message("NatureServe State-specific statuses for state 2 added to info sheet")
          
          
          
          #Looking for global ranks in state1's sheet
          for (i in 1:nrow(species_rankState1)) {
            rank <- grep(species_rankState1[i,2], InfoSheet[ , 1], ignore.case=TRUE)
            for (j in rank) {
              InfoSheet[j,5] <- species_rankState1[i,3]
            }
          }
          
          #Looking for global ranks in state2's sheet
          for (i in 1:nrow(species_rankState2)) {
            rank3 <- grep(species_rankState2[i,2], InfoSheet[ , 1], ignore.case=TRUE)
            for (j in rank3) {
              if (is.na(InfoSheet[j,5]) == TRUE) {
                InfoSheet[j,5] <- species_rankState2[i,3]
              }
            }
          }
          message("NatureServe Global statuses added to info sheet")
        }
        
        
        if (!is.null(state3) && is.null(state4)) {
          InfoSheet <- cbind(InfoSheet[, 1:2], NewColumn = NA, NewColumn = NA, NewColumn = NA, NewColumn = NA, InfoSheet[, -(1:2)])
          # Assume 'state' is the user-defined variable
          state1_column_name <- paste(state1, "NatureServe Status")
          state2_column_name <- paste(state2, "NatureServe Status")
          state3_column_name <- paste(state3, "NatureServe Status")
          if (mark_invasive_species == TRUE) {
            colnames(InfoSheet) <- c("Scientific Name", "Taxon Rank", state1_column_name, state2_column_name, state3_column_name,
                                     "NatureServe Rounded Global Rank", "Family", "Class", 
                                     "Dataset Key", "Invasive", "Individual Count")
          } else {
            colnames(InfoSheet) <- c("Scientific Name", "Taxon Rank", state1_column_name, state2_column_name, state3_column_name,
                                     "NatureServe Rounded Global Rank", "Family", "Class", 
                                     "Dataset Key", "Individual Count")
          }
          
          for (i in 1:nrow(species_rankState1)) {
            rank <- grep(species_rankState1[i, 2], InfoSheet[, 1], ignore.case = TRUE)
            for (j in rank) {
              InfoSheet[j, 3] <- species_rankState1[i, 4]
            }
          }
          
          message("NatureServe State-specific statuses for state 1 added to info sheet")
          
          for (i in 1:nrow(species_rankState2)) {
            rank <- grep(species_rankState2[i, 2], InfoSheet[, 1], ignore.case = TRUE)
            for (j in rank) {
              InfoSheet[j, 4] <- species_rankState2[i, 4]
            }
          }
          
          message("NatureServe State-specific statuses for state 2 added to info sheet")
          
          for (i in 1:nrow(species_rankState3)) {
            rank <- grep(species_rankState3[i, 2], InfoSheet[, 1], ignore.case = TRUE)
            for (j in rank) {
              InfoSheet[j, 5] <- species_rankState3[i, 4]
            }
          }
          
          message("NatureServe State-specific statuses for state 3 added to info sheet")
          
          #Looking for global ranks in state1's sheet
          for (i in 1:nrow(species_rankState1)) {
            rank <- grep(species_rankState1[i,2], InfoSheet[ , 1], ignore.case=TRUE)
            for (j in rank) {
              InfoSheet[j,6] <- species_rankState1[i,3]
            }
          }
          
          #Looking for global ranks in state2's sheet
          for (i in 1:nrow(species_rankState2)) {
            rank3 <- grep(species_rankState2[i,2], InfoSheet[ , 1], ignore.case=TRUE)
            for (j in rank3) {
              if (is.na(InfoSheet[j,6]) == TRUE) {
                InfoSheet[j,6] <- species_rankState2[i,3]
              }
            }
          }
          
          #Looking for global ranks in state3's sheet
          for (i in 1:nrow(species_rankState3)) {
            rank3 <- grep(species_rankState3[i,2], InfoSheet[ , 1], ignore.case=TRUE)
            for (j in rank3) {
              if (is.na(InfoSheet[j,6]) == TRUE) {
                InfoSheet[j,6] <- species_rankState3[i,3]
              }
            }
          }
          
          message("NatureServe Global statuses added to info sheet")
          
        }
        
        if (!is.null(state3) && !is.null(state4)) {
          InfoSheet <- cbind(InfoSheet[, 1:2], NewColumn = NA, NewColumn = NA, NewColumn = NA, NewColumn = NA, NewColumn = NA, InfoSheet[, -(1:2)])
          # Assume 'state' is the user-defined variable
          state1_column_name <- paste(state1, "NatureServe Status")
          state2_column_name <- paste(state2, "NatureServe Status")
          state3_column_name <- paste(state3, "NatureServe Status")
          state4_column_name <- paste(state4, "NatureServe Status")
          if (mark_invasive_species == TRUE) {
            colnames(InfoSheet) <- c("Scientific Name", "Taxon Rank", state1_column_name, state2_column_name, state3_column_name, state4_column_name,
                                     "NatureServe Rounded Global Rank", "Family", "Class", 
                                     "Dataset Key", "Invasive", "Individual Count")
          } else {
            colnames(InfoSheet) <- c("Scientific Name", "Taxon Rank", state1_column_name, state2_column_name, state3_column_name, state4_column_name,
                                     "NatureServe Rounded Global Rank", "Family", "Class", 
                                     "Dataset Key", "Individual Count")
          }
          for (i in 1:nrow(species_rankState1)) {
            rank <- grep(species_rankState1[i, 2], InfoSheet[, 1], ignore.case = TRUE)
            for (j in rank) {
              InfoSheet[j, 3] <- species_rankState1[i, 4]
            }
          }
          
          message("NatureServe State-specific statuses for state 1 added to info sheet")
          
          for (i in 1:nrow(species_rankState2)) {
            rank <- grep(species_rankState2[i, 2], InfoSheet[, 1], ignore.case = TRUE)
            for (j in rank) {
              InfoSheet[j, 4] <- species_rankState2[i, 4]
            }
          }
          
          message("NatureServe State-specific statuses for state 2 added to info sheet")
          
          for (i in 1:nrow(species_rankState3)) {
            rank <- grep(species_rankState3[i, 2], InfoSheet[, 1], ignore.case = TRUE)
            for (j in rank) {
              InfoSheet[j, 5] <- species_rankState3[i, 4]
            }
          }
          
          message("NatureServe State-specific statuses for state 3 added to info sheet")
          
          for (i in 1:nrow(species_rankState4)) {
            rank <- grep(species_rankState4[i, 2], InfoSheet[, 1], ignore.case = TRUE)
            for (j in rank) {
              InfoSheet[j, 6] <- species_rankState4[i, 4]
            }
          }
          
          message("NatureServe State-specific statuses for state 4 added to info sheet")
          
          #Looking for global ranks in state1's sheet
          for (i in 1:nrow(species_rankState1)) {
            rank <- grep(species_rankState1[i,2], InfoSheet[ , 1], ignore.case=TRUE)
            for (j in rank) {
              InfoSheet[j,7] <- species_rankState1[i,3]
            }
          }
          
          #Looking for global ranks in state2's sheet
          for (i in 1:nrow(species_rankState2)) {
            rank3 <- grep(species_rankState2[i,2], InfoSheet[ , 1], ignore.case=TRUE)
            for (j in rank3) {
              if (is.na(InfoSheet[j,7]) == TRUE) {
                InfoSheet[j,7] <- species_rankState2[i,3]
              }
            }
          }
          
          #Looking for global ranks in state3's sheet
          for (i in 1:nrow(species_rankState3)) {
            rank3 <- grep(species_rankState3[i,2], InfoSheet[ , 1], ignore.case=TRUE)
            for (j in rank3) {
              if (is.na(InfoSheet[j,7]) == TRUE) {
                InfoSheet[j,7] <- species_rankState3[i,3]
              }
            }
          }
          
          #Looking for global ranks in state4's sheet
          for (i in 1:nrow(species_rankState4)) {
            rank3 <- grep(species_rankState4[i,2], InfoSheet[ , 1], ignore.case=TRUE)
            for (j in rank3) {
              if (is.na(InfoSheet[j,7]) == TRUE) {
                InfoSheet[j,7] <- species_rankState4[i,3]
              }
            }
          }
          
          message("NatureServe Global statuses added to info sheet")
          
          
        }
      }
    }
    
  }   
  
  ## ICUN Red List Statuses
  if (ICUNRedList_statuses == TRUE) { 
    if (is.null(state1)) {
      stop("Error: Must provide at least a state 1 if natureserve_statuses is set to TRUE.") 
    }
    if (is.null(state2) && is.null(state3) && is.null(state4)) {
      if (mark_invasive_species == TRUE && natureserve_statuses == TRUE) {
        InfoSheet <- data.frame(InfoSheet)
        InfoSheet <- cbind(InfoSheet[, 1:4], NewColumn = NA, InfoSheet[, -(1:4)])
        state1_column_name <- paste(state1, "NatureServe Status")
        colnames(InfoSheet) <- c("Scientific Name", "Taxon Rank", state1_column_name, 
                                 "NatureServe Rounded Global Rank","ICUN Red List Status", "Family", "Class", 
                                 "Dataset Key", "Invasive", "Individual Count")
        for (i in 1:nrow(ICUNRedList)) {
          rank <- grep(ICUNRedList[i, 3], InfoSheet[, 1], ignore.case = TRUE)
          for (j in rank) {
            InfoSheet[j, 5] <- ICUNRedList[i, 4]
          }
        }
        message("ICUN Red List statuses added to info sheet")  
      } else if (mark_invasive_species == TRUE && natureserve_statuses == FALSE) {
        InfoSheet <- data.frame(InfoSheet)
        InfoSheet <- cbind(InfoSheet[, 1:2], NewColumn = NA, InfoSheet[, -(1:2)])
        colnames(InfoSheet) <- c("Scientific Name", "Taxon Rank", "ICUN Red List Status", "Family", "Class", 
                                 "Dataset Key", "Invasive", "Individual Count")  
        for (i in 1:nrow(ICUNRedList)) {
          rank <- grep(ICUNRedList[i, 3], InfoSheet[, 1], ignore.case = TRUE)
          for (j in rank) {
            InfoSheet[j, 3] <- ICUNRedList[i, 4]
          }
        }
        message("ICUN Red List statuses added to info sheet")      
      } else if (mark_invasive_species == FALSE && natureserve_statuses == TRUE) {
        InfoSheet <- data.frame(InfoSheet)
        InfoSheet <- cbind(InfoSheet[, 1:4], NewColumn = NA, InfoSheet[, -(1:4)])
        state1_column_name <- paste(state1, "NatureServe Status")
        colnames(InfoSheet) <- c("Scientific Name", "Taxon Rank", state1_column_name, 
                                 "NatureServe Rounded Global Rank", "ICUN Red List Status", "Family", "Class", 
                                 "Dataset Key", "Individual Count")  
        for (i in 1:nrow(ICUNRedList)) {
          rank <- grep(ICUNRedList[i, 3], InfoSheet[, 1], ignore.case = TRUE)
          for (j in rank) {
            InfoSheet[j, 5] <- ICUNRedList[i, 4]
          }
        }
        message("ICUN Red List statuses added to info sheet")    
      } else if (mark_invasive_species == FALSE && natureserve_statuses == FALSE) {
        InfoSheet <- data.frame(InfoSheet)
        InfoSheet <- cbind(InfoSheet[, 1:2], NewColumn = NA, InfoSheet[, -(1:2)])
        colnames(InfoSheet) <- c("Scientific Name", "Taxon Rank", "ICUN Red List Status", "Family", "Class", 
                                 "Dataset Key","Individual Count")  
        for (i in 1:nrow(ICUNRedList)) {
          rank <- grep(ICUNRedList[i, 3], InfoSheet[, 1], ignore.case = TRUE)
          for (j in rank) {
            InfoSheet[j, 3] <- ICUNRedList[i, 4]
          }
        }
        message("ICUN Red List statuses added to info sheet")  
      }
    } else {
      if (is.null(state2) && !is.null(state3)) {
        stop("Error: Must provide a 2nd state if a 3rd state is provided when ICUNRedList_statuses is set to TRUE.")
      }
      if (is.null(state3) && !is.null(state4)) {
        stop("Error: Must provide a 3rd state if a 4th state is provided when ICUNRedList_statuses is set to TRUE.")
      }
      if (is.null(state2) && is.null(state3) && !is.null(state4)) {
        stop("Error: Must provide a 2nd state and 3rd state if a 4th state is provided when ICUNRedList_statuses is set to TRUE.")
      }  
      else {
        if (mark_invasive_species == TRUE && natureserve_statuses == TRUE) {
          if (is.null(state3) && is.null(state4)) {
            InfoSheet <- data.frame(InfoSheet)
            InfoSheet <- cbind(InfoSheet[, 1:5], NewColumn = NA, InfoSheet[, -(1:5)])
            colnames(InfoSheet) <- c("Scientific Name", "Taxon Rank", state1_column_name, state2_column_name,
                                     "NatureServe Rounded Global Rank","ICUN Red List Status", "Family", "Class", 
                                     "Dataset Key", "Invasive", "Individual Count")
            for (i in 1:nrow(ICUNRedList)) {
              rank <- grep(ICUNRedList[i, 3], InfoSheet[, 1], ignore.case = TRUE)
              for (j in rank) {
                InfoSheet[j, 6] <- ICUNRedList[i, 4]
              }
            }  
          }
          if (!is.null(state3) && is.null(state4)) {
            InfoSheet <- data.frame(InfoSheet)
            InfoSheet <- cbind(InfoSheet[, 1:6], NewColumn = NA, InfoSheet[, -(1:6)])
            colnames(InfoSheet) <- c("Scientific Name", "Taxon Rank", state1_column_name, state2_column_name, state3_column_name,
                                     "NatureServe Rounded Global Rank","ICUN Red List Status", "Family", "Class", 
                                     "Dataset Key", "Invasive", "Individual Count")
            for (i in 1:nrow(ICUNRedList)) {
              rank <- grep(ICUNRedList[i, 3], InfoSheet[, 1], ignore.case = TRUE)
              for (j in rank) {
                InfoSheet[j, 7] <- ICUNRedList[i, 4]
              }
            }  
          }
          if (!is.null(state3) && !is.null(state4)) {
            InfoSheet <- data.frame(InfoSheet)
            InfoSheet <- cbind(InfoSheet[, 1:7], NewColumn = NA, InfoSheet[, -(1:7)])
            colnames(InfoSheet) <- c("Scientific Name", "Taxon Rank", state1_column_name, state2_column_name, state3_column_name, state4_column_name,
                                     "NatureServe Rounded Global Rank","ICUN Red List Status", "Family", "Class", 
                                     "Dataset Key", "Invasive", "Individual Count")
            for (i in 1:nrow(ICUNRedList)) {
              rank <- grep(ICUNRedList[i, 3], InfoSheet[, 1], ignore.case = TRUE)
              for (j in rank) {
                InfoSheet[j, 8] <- ICUNRedList[i, 4]
              }
            }  
          }
          message("ICUN Red List statuses added to info sheet")  
        } else if (mark_invasive_species == TRUE && natureserve_statuses == FALSE) {
          InfoSheet <- data.frame(InfoSheet)
          InfoSheet <- cbind(InfoSheet[, 1:2], NewColumn = NA, InfoSheet[, -(1:2)])
          colnames(InfoSheet) <- c("Scientific Name", "Taxon Rank", "ICUN Red List Status", "Family", "Class", 
                                   "Dataset Key", "Invasive", "Individual Count")  
          for (i in 1:nrow(ICUNRedList)) {
            rank <- grep(ICUNRedList[i, 3], InfoSheet[, 1], ignore.case = TRUE)
            for (j in rank) {
              InfoSheet[j, 3] <- ICUNRedList[i, 4]
            }
          }
          message("ICUN Red List statuses added to info sheet")      
        } else if (mark_invasive_species == FALSE && natureserve_statuses == TRUE) {
          if (is.null(state3) && is.null(state4)) {
            InfoSheet <- data.frame(InfoSheet)
            InfoSheet <- cbind(InfoSheet[, 1:5], NewColumn = NA, InfoSheet[, -(1:5)])
            colnames(InfoSheet) <- c("Scientific Name", "Taxon Rank", state1_column_name, state2_column_name,
                                     "NatureServe Rounded Global Rank", "ICUN Red List Status", "Family", "Class", 
                                     "Dataset Key", "Individual Count")  
            for (i in 1:nrow(ICUNRedList)) {
              rank <- grep(ICUNRedList[i, 3], InfoSheet[, 1], ignore.case = TRUE)
              for (j in rank) {
                InfoSheet[j, 6] <- ICUNRedList[i, 4]
              }
            }  
          }
          if (!is.null(state3) && is.null(state4)) {
            InfoSheet <- data.frame(InfoSheet)
            InfoSheet <- cbind(InfoSheet[, 1:6], NewColumn = NA, InfoSheet[, -(1:6)])
            colnames(InfoSheet) <- c("Scientific Name", "Taxon Rank", state1_column_name, state2_column_name, state3_column_name,
                                     "NatureServe Rounded Global Rank", "ICUN Red List Status", "Family", "Class", 
                                     "Dataset Key", "Individual Count")  
            for (i in 1:nrow(ICUNRedList)) {
              rank <- grep(ICUNRedList[i, 3], InfoSheet[, 1], ignore.case = TRUE)
              for (j in rank) {
                InfoSheet[j, 7] <- ICUNRedList[i, 4]
              }
            }  
          }
          if (!is.null(state3) && !is.null(state4)) {
            InfoSheet <- data.frame(InfoSheet)
            InfoSheet <- cbind(InfoSheet[, 1:7], NewColumn = NA, InfoSheet[, -(1:7)])
            colnames(InfoSheet) <- c("Scientific Name", "Taxon Rank", state1_column_name, state2_column_name, state3_column_name, state4_column_name,
                                     "NatureServe Rounded Global Rank", "ICUN Red List Status", "Family", "Class", 
                                     "Dataset Key", "Individual Count")  
            for (i in 1:nrow(ICUNRedList)) {
              rank <- grep(ICUNRedList[i, 3], InfoSheet[, 1], ignore.case = TRUE)
              for (j in rank) {
                InfoSheet[j, 8] <- ICUNRedList[i, 4]
              }
            }  
          }
          message("ICUN Red List statuses added to info sheet")    
          
        } else if (mark_invasive_species == FALSE && natureserve_statuses == FALSE) {
          InfoSheet <- data.frame(InfoSheet)
          InfoSheet <- cbind(InfoSheet[, 1:2], NewColumn = NA, InfoSheet[, -(1:2)])
          colnames(InfoSheet) <- c("Scientific Name", "Taxon Rank", "ICUN Red List Status", "Family", "Class", 
                                   "Dataset Key","Individual Count")  
          for (i in 1:nrow(ICUNRedList)) {
            rank <- grep(ICUNRedList[i, 3], InfoSheet[, 1], ignore.case = TRUE)
            for (j in rank) {
              InfoSheet[j, 3] <- ICUNRedList[i, 4]
            }
          }
          message("ICUN Red List statuses added to info sheet")  
        }
        
      }
    }
  }
  # Return the modified data frame
  return(InfoSheet)
}
