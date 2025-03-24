#' Comparingstatuses
#' 
#' This function compares the species' statuses from different organizations
#' to determine the finsl status to be used for each species.
#'
#' @param data data.frame. Needs to be the output from generatinginfosheet, with statuses.
#' @param state1 character string. The state the polygon is in.
#' @param state2 character string. If the polygon is in 2 states, the second state the polygon is in.
#' @param state3 character string. If the polygon is in 3 states, the third state the polygon is in.
#' @param state4 character string. If the polygon is in 4 states, the fourth state the polygon is in. The limit is 4 states.
#' @param comparison character string. Either "most concern" or "least concern". If "most concern", the final_status will be the status that is of the most concern. If "least concern", the final_status will be the status of least concern.
#' 
#' @return A dataframe with a Final status for each species.
#' 
#' @export

comparingstatuses <- function(data, state1 = NULL, state2 = NULL, state3 = NULL, state4 = NULL, comparison = NULL) {
  # Ensure the comparison parameter is valid
  if (!comparison %in% c("least concern", "most concern")) {
    stop("Invalid comparison value. Use 'least concern' or 'most concern'.")
  }
  
  ## NatureServe State Levels
  NatureServe_statelevels <- c("SX" ,"SXB" , "SXB,S1N" ,"SXB,S2N" ,"SXB,S3N", "SXB,S4N", "SXB,S5N" ,"SXB,SNAN", "SXB,SXM" ,"SXB,SXN" ,"SXB,SNRN" ,"SXM", "SXN", "SXN,SNRB" ,"SH" ,"SHB" ,"SHB,S1N", "SHB,S2N" ,"SHB,S3N" ,"SHB,S4N" ,"SHB,S5N" ,"SHB,SHN" ,"SHB,SNRN" ,"SHM", "SHN", "SHN,SNRB" ,"S1" ,"S1B" ,"S1B,S1M" ,"S1B,S1N" ,"S1B,S1N,S1M" ,"S1B,S1N,S2M", "S1B,S1N,S3M" ,"S1B,S1N,S4M" ,"S1B,S1N,S5M" ,"S1B,S1N,SUM" ,"S1B,S2M" ,"S1B,S2N", "S1B,S2N,S1M", "S1B,S2N,S2M", "S1B,S2N,S3M", "S1B,S2N,S4M", "S1B,S2N,S5M", "S1B,S2N,SUM", "S1B,S3M", "S1B,S3N" ,"S1B,S3N,S1M" ,"S1B,S3N,S2M" ,"S1B,S3N,S3M", "S1B,S3N,S4M", "S1B,S3N,S5M", "S1B,S3N,SUM" ,"S1B,S4M" ,"S1B,S4N" ,"S1B,S4N,S1M", "S1B,S4N,S2M", "S1B,S4N,S3M" ,"S1B,S4N,S4M" ,"S1B,S4N,S5M" ,"S1B,S4N,SUM", "S1B,S5M" ,"S1B,S5N" ,"S1B,S5N,S1M" ,"S1B,S5N,S2M" ,"S1B,S5N,S3M" ,"S1B,S5N,S4M", "S1B,S5N,S5M" ,"S1B,S5N,SUM" ,"S1B,SNAN" ,"S1B,SNRM" ,"S1B,SNRN", "S1B,SUM", "S1M" ,"S1N" ,"S1N,S1M" ,"S1N,S2M" ,"S1N,S3M" ,"S1N,S4M", "S1N,S5N" ,"S1S2", "SNAB,S1N" ,"S2" ,"S2B" ,"S2B,S1M" ,"S2B,S1N" ,"S2B,S1N,S1M" ,"S2B,S1N,S2M", "S2B,S1N,S3M" ,"S2B,S1N,S4M" ,"S2B,S1N,S5M", "S2B,S1N,SUM" ,"S2B,S2M" ,"S2B,S2N" ,"S2B,S2N,S1M" ,"S2B,S2N,S2M" ,"S2B,S2N,S3M", "S2B,S2N,S4M", "S2B,S2N,S5M", "S2B,S2N,SUM" ,"S2B,S3M" ,"S2B,S3N", "S2B,S3N,S1M", "S2B,S3N,S2M", "S2B,S3N,S3M" ,"S2B,S3N,S4M", "S2B,S3N,S5M" ,"S2B,S3N,SUM", "S2,S4N", "S2B,S4M" ,"S2B,S4N" ,"S2B,S4N,S1M" ,"S2B,S4N,S2M", "S2B,S4N,S3M" ,"S2B,S4N,S4M", "S2B,S4N,S5M" ,"S2B,S4N,SUM" ,"S2B,S5M" ,"S2B,S5N", "S2B,S5N,S1M" ,"S2B,S5N,S2M", "S2B,S5N,S3M" ,"S2B,S5N,S4M" ,"S2B,S5N,S5M", "S2B,S5N,SUM" ,"S2B,SNRN", "S2B,SNRM", "S2B,SNAN", "S2B,SUM" ,"S2M" ,"S2N" ,"S2N,S1M" ,"S2N,S2M" ,"S2N,S3M", "S2N,S4M" ,"S2N,S5M" ,"SNAB,S2N" ,"S3" ,"S3B" ,"S3B,S1M", "S3B,S1N" ,"S3B,S1N,S1M", "S3B,S1N,S2M", "S3B,S1N,S3M" ,"S3B,S1N,S4M", "S3B,S1N,S5M", "S3B,S1N,SUM", "S3B,S2M", "S3B,S2N" ,"S3B,S2N,S1M", "S3B,S2N,S2M" ,"S3B,S2N,S3M", "S3B,S2N,S4M", "S3B,S2N,S5M", "S3B,S2N,SUM" ,"S3B,S3M" ,"S3B,S3N" ,"S3B,S3N,S1M" ,"S3B,S3N,S2M" , "S3B,S3N,S3M" ,"S3B,S3N,S4M" ,"S3B,S3N,S5M" ,"S3B,S3N,SUM" ,"S3B,S4M" ,"S3B,S4N", "S3N,S4B", "S3N,S5B", "S4N,S3B", "S3B,S4N,S1M" ,"S3B,S4N,S2M" ,"S3B,S4N,S3M", "S3B,S4N,S4M" ,"S3B,S4N,S5M", "S3B,S4N,SUM" ,"S3B,S5M" ,"S3B,S5N", "S3B,S5N,S1M" ,"S3B,S5N,S2M" ,"S3B,S5N,S3M", "S3B,S5N,S4M", "S3B,S5N,S5M", "S3B,S5N,SUM" ,"S3B,SNAN" ,"S3B,SNRM" ,"S3B,SNRN", "S3B,SUM","S3M" ,"S3N", "S3N,S1M" ,"S3N,S2M" ,"S3N,S3M" ,"S3N,S4M" ,"S3N,S5M", "SNAB,S3N", "S4" ,"S4B" ,"S4B,S1M" ,"S4B,S1N" ,"S4B,S1N,S1M" ,"S4B,S1N,S2M", "S4B,S1N,S3M", "S4B,S1N,S4M" ,"S4B,S1N,S5M" ,"S4B,S1N,SUM" ,"S4B,S2M" ,"S4B,S2N", "S4B,S2N,S1M" ,"S4B,S2N,S2M", "S4B,S2N,S3M" ,"S4B,S2N,S4M" ,"S4B,S2N,S5M", "S4B,S2N,SUM" ,"S4B,S3M" ,"S4B,S3N", "S4B,S3N,S1M" ,"S4B,S3N,S2M" ,"S4B,S3N,S3M", "S4B,S3N,S4M" ,"S4B,S3N,S5M" ,"S4B,S3N,SUM", "S4B,S4M", "S4B,S4N", "S4N,S4B", "S4B,S4N,S1M", "S4B,S4N,S2M" ,"S4B,S4N,S3M" ,"S4B,S4N,S4M" ,"S4B,S4N,S5M", "S4B,S4N,SUM", "S4B,S5M" , "S4N,S5B", "S4B,S5N" ,"S4B,S5N,S1M" ,"S4B,S5N,S2M", "S4B,S5N,S3M" ,"S4B,S5N,S4M", "S4B,S5N,S5M" ,"S4B,S5N,SUM" ,"S4B,SNAN" ,"S4B,SNRM" ,"S4B,SNRN" ,"S4B,SUM", "S4M" ,"S4N" ,"S4N,S1M", "S4N,S2M" ,"S4N,S3M" ,"S4N,S4M" ,"S4N,S5M" ,"SNAB,S4N", "S5" ,"S5B" ,"S5B,S1M", "S5B,S1N" ,"S5B,S1N,S1M" ,"S5B,S1N,S2M", "S5B,S1N,S3M", "S5B,S1N,S4M" ,"S5B,S1N,S5M", "S5B,S1N,SUM" ,"S5B,S2M" ,"S5B,S2N" ,"S5B,S2N,S1M", "S5B,S2N,S2M", "S5B,S2N,S3M", "S5B,S2N,S4M", "S5B,S2N,S5M" ,"S5B,S2N,SUM", "S5B,S3M" ,"S5B,S3N" ,"S5B,S3N,S1M" ,"S5B,S3N,S2M" ,"S5B,S3N,S3M", "S5B,S3N,S4M", "S5B,S3N,S5M" ,"S5B,S3N,SUM" ,"S5B,S4M" ,"S5B,S4N" ,"S5B,S4N,S1M" ,"S5B,S4N,S2M", "S5B,S4N,S3M", "S5B,S4N,S4M" ,"S5B,S4N,S5M", "S5B,S4N,SUM" ,"S5B,S5M" ,"S5B,S5N", "S5B,S5N,S1M" ,"S5B,S5N,S2M", "S5B,S5N,S3M", "S5B,S5N,S4M" ,"S5B,S5N,S5M", "S5B,S5N,SUM" ,"S5B,SNAN" ,"S5B,SNRM", "S5B,SNRN", "S5N,SNRB" ,"S5B,SUM" ,"S5M" ,"S5N", "S5N,S1M" ,"S5N,S2M", "S5N,S3M" ,"S5N,S4M" ,"S5N,S5M", "SNAB,S5N" ,"SU", "SUB", "SUB, SUM" ,"SUB,SUN" ,"SUB,SNAM" ,"SUM" ,"SUN" ,"SUN, SUM", "SNR" ,"SNRB", "SNRB,SNRM" ,"SNRB,SNRN" ,"SNRB,SNRN,SNRM" ,"SNRM" ,"SNRN" ,"SNRN, SNRM" ,"SNA" ,"SNAB" ,"SNAM" ,"SNAN" ,NA )
  NatureServeStateFactors <- factor(NatureServe_statelevels, levels = NatureServe_statelevels)
  
  ## NatureServe Global Levels
  # Replace all "S" with "G"
  NatureServe_globallevels <- gsub("S", "G", NatureServe_statelevels)
  NatureServeGlobalFactors <- factor(NatureServe_globallevels, levels = NatureServe_globallevels)
  
  ## ICUN Levels
  ICUNlevels <- rep(".", 254)
  # Assign specific values to the designated positions
  ICUNlevels[1] <- "Extinct"
  ICUNlevels[15] <- "Extinct in the Wild"
  ICUNlevels[27] <- "Critically Endangered"
  ICUNlevels[83] <- "Endangered"
  ICUNlevels[140] <- "Vulnerable"
  ICUNlevels[197] <- "Near Threatened"
  ICUNlevels[254] <- "Least Concern"
  
  # for 1 state
  if (is.null(state2) && is.null(state3) && is.null(state4)) {
    if (comparison == "most concern") {
      
      data$Final_status <- NA
      
      # Loop through each row
      cppFunction('
        NumericVector final_status(Dataframe data, CharacterVector NatureServe_statelevels, CharacterVector NatureServe_globallevels, CharacterVector ICUNlevels) {
          for (int i = 0; i < data.nrow(); i++) {
            // Extract the statuses for the current row
            std::string state1NatureServeStatus = as<std::string>(data(i, 2));
            std::string GlobalNatureServeStatus = as<std::string>(data(i, 3));
            std::string ICUNStatus = as<std::string>(data(i, 4));
            
            // Map statuses to numeric values using their respective level systems
            std::vector<int> numeric_values = {
              state1NatureServeStatus.empty() ? INT_MAX : std::distance(NatureServe_statelevels.begin(), std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state1NatureServeStatus)),
              GlobalNatureServeStatus.empty() ? INT_MAX : std::distance(NatureServe_globallevels.begin(), std::find(NatureServe_globallevels.begin(), NatureServe_globallevels.end(), GlobalNatureServeStatus)),
              ICUNStatus.empty() ? INT_MAX : std::distance(ICUNlevels.begin(), std::find(ICUNlevels.begin(), ICUNlevels.end(), ICUNStatus))
            };
            
            // Find the minimum numeric value
            int min_value = *std::min_element(numeric_values.begin(), numeric_values.end());
            
            // Assign the corresponding status or NA if all are Inf
            if (min_value == INT_MAX) {
              data(i, 5) = NA_STRING;
            } else {
              // Determine which level system the minimum value belongs to
              if (std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state1NatureServeStatus) != NatureServe_statelevels.end()) {
                data(i, 5) = NatureServe_statelevels[min_value];
              } else if (std::find(NatureServe_globallevels.begin(), NatureServe_globallevels.end(), GlobalNatureServeStatus) != NatureServe_globallevels.end()) {
                data(i, 5) = NatureServe_globallevels[min_value];
              } else if (std::find(ICUNlevels.begin(), ICUNlevels.end(), ICUNStatus) != ICUNlevels.end()) {
                data(i, 5) = ICUNlevels[min_value];
              }
            }
          }
          return data;
        }
      ')
      return final_status(data, NatureServe_statelevels, NatureServe_globallevels, ICUNlevels)
    }
    if (comparison == "least concern") {
      # Initialize the Final_status column
      data$Final_status <- NA
      # Loop through each row
      cppFunction('
        NumericVector final_status(Dataframe data, CharacterVector NatureServe_statelevels, CharacterVector NatureServe_globallevels, CharacterVector ICUNlevels) {
          for (int i = 0; i < data.nrow(); i++) {
            // Extract the statuses for the current row
            std::string state1NatureServeStatus = as<std::string>(data(i, 2));
            std::string GlobalNatureServeStatus = as<std::string>(data(i, 3));
            std::string ICUNStatus = as<std::string>(data(i, 4));
            
            int state1NatureServe_value = std::distance(NatureServe_statelevels.begin(), std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state1NatureServeStatus));
            if (state1NatureServe_value != NA_INTEGER && state1NatureServe_value > 309) {
              state1NatureServe_value = 0;
            }
            
            int GlobalNatureServe_value = std::distance(NatureServe_globallevels.begin(), std::find(NatureServe_globallevels.begin(), NatureServe_globallevels.end(), GlobalNatureServeStatus));
            if (GlobalNatureServe_value != NA_INTEGER && GlobalNatureServe_value > 309) {
              GlobalNatureServe_value = 0;
            }
            
            // Map statuses to numeric values using their respective level systems
            std::vector<int> numeric_values = {
              state1NatureServe_value,
              GlobalNatureServe_value,
              ICUNStatus.empty() ? 0 : std::distance(ICUNlevels.begin(), std::find(ICUNlevels.begin(), ICUNlevels.end(), ICUNStatus))
            };
            
            // Find the maximum numeric value
            int max_value = *std::max_element(numeric_values.begin(), numeric_values.end());
            
            // Determine which level system the maximum value belongs to
            if (std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state1NatureServeStatus) != NatureServe_statelevels.end()) {
              data(i, 5) = NatureServe_statelevels[max_value];
            } else if (std::find(NatureServe_globallevels.begin(), NatureServe_globallevels.end(), GlobalNatureServeStatus) != NatureServe_globallevels.end()) {
              data(i, 5) = NatureServe_globallevels[max_value];
            } else if (std::find(ICUNlevels.begin(), ICUNlevels.end(), ICUNStatus) != ICUNlevels.end()) {
              data(i, 5) = ICUNlevels[max_value];
            }
          }
          return data;
        }
      ')
      return final_status(data, NatureServe_statelevels, NatureServe_globallevels, ICUNlevels)
    }
  }
  # for 2 states
  if (!is.null(state2) && is.null(state3) && is.null(state4)) {
    if (comparison == "most concern") {
      # Initialize the Final_status column
      data$Final_status <- NA
      
      # Loop through each row
      cppFunction('
        NumericVector final_status(Dataframe data, CharacterVector NatureServe_statelevels, CharacterVector NatureServe_globallevels, CharacterVector ICUNlevels) {
          for (int i = 0; i < data.nrow(); i++) {
            // Extract the statuses for the current row
            std::string state1NatureServeStatus = as<std::string>(data(i, 3));
            std::string state2NatureServeStatus = as<std::string>(data(i, 4));
            std::string GlobalNatureServeStatus = as<std::string>(data(i, 5));
            std::string ICUNStatus = as<std::string>(data(i, 6));
            
            // Map statuses to numeric values using their respective level systems
            std::vector<int> numeric_values = {
              state1NatureServeStatus.empty() ? INT_MAX : std::distance(NatureServe_statelevels.begin(), std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state1NatureServeStatus)),
              state2NatureServeStatus.empty() ? INT_MAX : std::distance(NatureServe_statelevels.begin(), std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state2NatureServeStatus)),
              GlobalNatureServeStatus.empty() ? INT_MAX : std::distance(NatureServe_globallevels.begin(), std::find(NatureServe_globallevels.begin(), NatureServe_globallevels.end(), GlobalNatureServeStatus)),
              ICUNStatus.empty() ? INT_MAX : std::distance(ICUNlevels.begin(), std::find(ICUNlevels.begin(), ICUNlevels.end(), ICUNStatus))
            };
            
            // Find the minimum numeric value
            int min_value = *std::min_element(numeric_values.begin(), numeric_values.end());
            
            // Assign the corresponding status or NA if all are Inf
            if (min_value == INT_MAX) {
              data(i, 7) = NA_STRING;
            } else {
              // Determine which level system the minimum value belongs to
              if (std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state1NatureServeStatus) != NatureServe_statelevels.end()) {
                data(i, 7) = NatureServe_statelevels[min_value];
              } else if (std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state2NatureServeStatus) != NatureServe_statelevels.end()) {
                data(i, 7) = NatureServe_statelevels[min_value];
              } else if (std::find(NatureServe_globallevels.begin(), NatureServe_globallevels.end(), GlobalNatureServeStatus) != NatureServe_globallevels.end()) {
                data(i, 7) = NatureServe_globallevels[min_value];
              } else if (std::find(ICUNlevels.begin(), ICUNlevels.end(), ICUNStatus) != ICUNlevels.end()) {
                data(i, 7) = ICUNlevels[min_value];
              }
            }
          }
          return data;
        }
      ')
      return final_status(data, NatureServe_statelevels, NatureServe_globallevels, ICUNlevels)
    }
    if (comparison == "least concern") {         
      # Initialize the Final_status column
      data$Final_status <- NA
      
      # Loop through each row
      cppFunction('
        NumericVector final_status(Dataframe data, CharacterVector NatureServe_statelevels, CharacterVector NatureServe_globallevels, CharacterVector ICUNlevels) {
          for (int i = 0; i < data.nrow(); i++) {
            // Extract the statuses for the current row
            std::string state1NatureServeStatus = as<std::string>(data(i, 3));
            std::string state2NatureServeStatus = as<std::string>(data(i, 4));
            std::string GlobalNatureServeStatus = as<std::string>(data(i, 5));
            std::string ICUNStatus = as<std::string>(data(i, 6));
            
            int state1NatureServe_value = std::distance(NatureServe_statelevels.begin(), std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state1NatureServeStatus));
            if (state1NatureServe_value != NA_INTEGER && state1NatureServe_value > 309) {
              state1NatureServe_value = 0;
            }
            
            int state2NatureServe_value = std::distance(NatureServe_statelevels.begin(), std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state2NatureServeStatus));
            if (state2NatureServe_value != NA_INTEGER && state2NatureServe_value > 309) {
              state2NatureServe_value = 0;
            }
            
            int GlobalNatureServe_value = std::distance(NatureServe_globallevels.begin(), std::find(NatureServe_globallevels.begin(), NatureServe_globallevels.end(), GlobalNatureServeStatus));
            if (GlobalNatureServe_value != NA_INTEGER && GlobalNatureServe_value > 309) {
              GlobalNatureServe_value = 0;
            }
            
            // Map statuses to numeric values using their respective level systems
            std::vector<int> numeric_values = {
              state1NatureServe_value,
              state2NatureServe_value,
              GlobalNatureServe_value,
              ICUNStatus.empty() ? 0 : std::distance(ICUNlevels.begin(), std::find(ICUNlevels.begin(), ICUNlevels.end(), ICUNStatus))
            };
            
            // Find the maximum numeric value
            int max_value = *std::max_element(numeric_values.begin(), numeric_values.end());
            
            // Determine which level system the maximum value belongs to
            if (std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state1NatureServeStatus) != NatureServe_statelevels.end()) {
              data(i, 7) = NatureServe_statelevels[max_value];
            } else if (std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state2NatureServeStatus) != NatureServe_statelevels.end()) {
              data(i, 7) = NatureServe_statelevels[max_value];
            } else if (std::find(NatureServe_globallevels.begin(), NatureServe_globallevels.end(), GlobalNatureServeStatus) != NatureServe_globallevels.end()) {
              data(i, 7) = NatureServe_globallevels[max_value];
            } else if (std::find(ICUNlevels.begin(), ICUNlevels.end(), ICUNStatus) != ICUNlevels.end()) {
              data(i, 7) = ICUNlevels[max_value];
            }
          }
          return data;
        }
      ')
      return final_status(data, NatureServe_statelevels, NatureServe_globallevels, ICUNlevels)
    }
  }
  
  # for 3 states
  if (!is.null(state2) && !is.null(state3) && is.null(state4)) {
    if (comparison == "most concern") {
      # Initialize the Final_status column
      data$Final_status <- NA
      
      # Loop through each row
      cppFunction('
        NumericVector final_status(Dataframe data, CharacterVector NatureServe_statelevels, CharacterVector NatureServe_globallevels, CharacterVector ICUNlevels) {
          for (int i = 0; i < data.nrow(); i++) {
            // Extract the statuses for the current row
            std::string state1NatureServeStatus = as<std::string>(data(i, 3));
            std::string state2NatureServeStatus = as<std::string>(data(i, 4));
            std::string state3NatureServeStatus = as<std::string>(data(i, 5));
            std::string GlobalNatureServeStatus = as<std::string>(data(i, 6));
            std::string ICUNStatus = as<std::string>(data(i, 7));
            
            // Map statuses to numeric values using their respective level systems
            std::vector<int> numeric_values = {
              state1NatureServeStatus.empty() ? INT_MAX : std::distance(NatureServe_statelevels.begin(), std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state1NatureServeStatus)),
              state2NatureServeStatus.empty() ? INT_MAX : std::distance(NatureServe_statelevels.begin(), std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state2NatureServeStatus)),
              state3NatureServeStatus.empty() ? INT_MAX : std::distance(NatureServe_statelevels.begin(), std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state3NatureServeStatus)),
              GlobalNatureServeStatus.empty() ? INT_MAX : std::distance(NatureServe_globallevels.begin(), std::find(NatureServe_globallevels.begin(), NatureServe_globallevels.end(), GlobalNatureServeStatus)),
              ICUNStatus.empty() ? INT_MAX : std::distance(ICUNlevels.begin(), std::find(ICUNlevels.begin(), ICUNlevels.end(), ICUNStatus))
            };
            
            // Find the minimum numeric value
            int min_value = *std::min_element(numeric_values.begin(), numeric_values.end());
            
            // Assign the corresponding status or NA if all are Inf
            if (min_value == INT_MAX) {
              data(i, 8) = NA_STRING;
            } else {
              // Determine which level system the minimum value belongs to
              if (std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state1NatureServeStatus) != NatureServe_statelevels.end()) {
                data(i, 8) = NatureServe_statelevels[min_value];
              } else if (std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state2NatureServeStatus) != NatureServe_statelevels.end()) {
                data(i, 8) = NatureServe_statelevels[min_value];
              } else if (std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state3NatureServeStatus) != NatureServe_statelevels.end()) {
                data(i, 8) = NatureServe_statelevels[min_value];
              } else if (std::find(NatureServe_globallevels.begin(), NatureServe_globallevels.end(), GlobalNatureServeStatus) != NatureServe_globallevels.end()) {
                data(i, 8) = NatureServe_globallevels[min_value];
              } else if (std::find(ICUNlevels.begin(), ICUNlevels.end(), ICUNStatus) != ICUNlevels.end()) {
                data(i, 8) = ICUNlevels[min_value];
              }
            } 
          }
          return data;
        }
      ')
      return final_status(data, NatureServe_statelevels, NatureServe_globallevels, ICUNlevels)
    }
    if (comparison == "least concern") {
      data$Final_status <- NA
      
      # Loop through each row
      cppFunction('
        NumericVector final_status(Dataframe data, CharacterVector NatureServe_statelevels, CharacterVector NatureServe_globallevels, CharacterVector ICUNlevels) {
          for (int i = 0; i < data.nrow(); i++) {
            // Extract the statuses for the current row
            std::string state1NatureServeStatus = as<std::string>(data(i, 3));
            std::string state2NatureServeStatus = as<std::string>(data(i, 4));
            std::string state3NatureServeStatus = as<std::string>(data(i, 5));
            std::string GlobalNatureServeStatus = as<std::string>(data(i, 6));
            std::string ICUNStatus = as<std::string>(data(i, 7));
            
            int state1NatureServe_value = std::distance(NatureServe_statelevels.begin(), std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state1NatureServeStatus));
            if (state1NatureServe_value != NA_INTEGER && state1NatureServe_value > 309) {
              state1NatureServe_value = 0;
            }
            
            int state2NatureServe_value = std::distance(NatureServe_statelevels.begin(), std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state2NatureServeStatus));
            if (state2NatureServe_value != NA_INTEGER && state2NatureServe_value > 309) {
              state2NatureServe_value = 0;
            }
            
            int state3NatureServe_value = std::distance(NatureServe_statelevels.begin(), std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state3NatureServeStatus));
            if (state3NatureServe_value != NA_INTEGER && state3NatureServe_value > 309) {
              state3NatureServe_value = 0;
            }
            
            int GlobalNatureServe_value = std::distance(NatureServe_globallevels.begin(), std::find(NatureServe_globallevels.begin(), NatureServe_globallevels.end(), GlobalNatureServeStatus));
            if (GlobalNatureServe_value != NA_INTEGER && GlobalNatureServe_value > 309) {
              GlobalNatureServe_value = 0;
            }
            
            // Map statuses to numeric values using their respective level systems
            std::vector<int> numeric_values = {
              state1NatureServe_value,
              state2NatureServe_value,
              state3NatureServe_value,
              GlobalNatureServe_value,
              ICUNStatus.empty() ? 0 : std::distance(ICUNlevels.begin(), std::find(ICUNlevels.begin(), ICUNlevels.end(), ICUNStatus))
            };
            max_value = *std::max_element(numeric_values.begin(), numeric_values.end());
            if (max_value == INT_MAX) {
              data(i, 8) = NA_STRING;
            } else {
              if (std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state1NatureServeStatus) != NatureServe_statelevels.end()) {
                data(i, 8) = NatureServe_statelevels[max_value];
              } else if (std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state2NatureServeStatus) != NatureServe_statelevels.end()) {
                data(i, 8) = NatureServe_statelevels[max_value];
              } else if (std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state3NatureServeStatus) != NatureServe_statelevels.end()) {
                data(i, 8) = NatureServe_statelevels[max_value];
              } else if (std::find(NatureServe_globallevels.begin(), NatureServe_globallevels.end(), GlobalNatureServeStatus) != NatureServe_globallevels.end()) {
                data(i, 8) = NatureServe_globallevels[max_value];
              } else if (std::find(ICUNlevels.begin(), ICUNlevels.end(), ICUNStatus) != ICUNlevels.end()) {
                data(i, 8) = ICUNlevels[max_value];
              }
            }
          }
          return data;
        }
      ')
      return final_status(data, NatureServe_statelevels, NatureServe_globallevels, ICUNlevels)
    }
  }
  
  # for 4 states
  if (!is.null(state2) && !is.null(state3) && !is.null(state4)) {
    if (comparison == "most concern") {
      # Initialize the Final_status column
      data$Final_status <- NA
      
      # Loop through each row
      cppFunction('
        NumericVector final_status(Dataframe data, CharacterVector NatureServe_statelevels, CharacterVector NatureServe_globallevels, CharacterVector ICUNlevels) {
          for (int i = 0; i < data.nrow(); i++) {
            // Extract the statuses for the current row
            std::string state1NatureServeStatus = as<std::string>(data(i, 3));
            std::string state2NatureServeStatus = as<std::string>(data(i, 4));
            std::string state3NatureServeStatus = as<std::string>(data(i, 5));
            std::string state4NatureServeStatus = as<std::string>(data(i, 6));
            std::string GlobalNatureServeStatus = as<std::string>(data(i, 7));
            std::string ICUNStatus = as<std::string>(data(i, 8));
            
            // Map statuses to numeric values using their respective level systems
            std::vector<int> numeric_values = {
              state1NatureServeStatus.empty() ? INT_MAX : std::distance(NatureServe_statelevels.begin(), std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state1NatureServeStatus)),
              state2NatureServeStatus.empty() ? INT_MAX : std::distance(NatureServe_statelevels.begin(), std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state2NatureServeStatus)),
              state3NatureServeStatus.empty() ? INT_MAX : std::distance(NatureServe_statelevels.begin(), std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state3NatureServeStatus)),
              state4NatureServeStatus.empty() ? INT_MAX : std::distance(NatureServe_statelevels.begin(), std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state4NatureServeStatus)),
              GlobalNatureServeStatus.empty() ? INT_MAX : std::distance(NatureServe_globallevels.begin(), std::find(NatureServe_globallevels.begin(), NatureServe_globallevels.end(), GlobalNatureServeStatus)),
              ICUNStatus.empty() ? INT_MAX : std::distance(ICUNlevels.begin(), std::find(ICUNlevels.begin(), ICUNlevels.end(), ICUNStatus))
            };
            
            // Find the minimum numeric value
            int min_value = *std::min_element(numeric_values.begin(), numeric_values.end());
            
            // Assign the corresponding status or NA if all are Inf
            if (min_value == INT_MAX) {
              data(i, 9) = NA_STRING;
            } else {
              // Determine which level system the minimum value belongs to
              if (std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state1NatureServeStatus) != NatureServe_statelevels.end()) {
                data(i, 9) = NatureServe_statelevels[min_value];
              } else if (std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state2NatureServeStatus) != NatureServe_statelevels.end()) {
                data(i, 9) = NatureServe_statelevels[min_value];
              } else if (std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state3NatureServeStatus) != NatureServe_statelevels.end()) {
                data(i, 9) = NatureServe_statelevels[min_value];
              } else if (std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state4NatureServeStatus) != NatureServe_statelevels.end()) {
                data(i, 9) = NatureServe_statelevels[min_value];
              } else if (std::find(NatureServe_globallevels.begin(), NatureServe_globallevels.end(), GlobalNatureServeStatus) != NatureServe_globallevels.end()) {
                data(i, 9) = NatureServe_globallevels[min_value];
              } else if (std::find(ICUNlevels.begin(), ICUNlevels.end(), ICUNStatus) != ICUNlevels.end()) {
                data(i, 9) = ICUNlevels[min_value];
              }
            }
          }
          return data;
        }
      ')
      return final_status(data, NatureServe_statelevels, NatureServe_globallevels, ICUNlevels)
    }  
    if (comparison == "least concern") {
      data$Final_status <- NA
      
      # Loop through each row
      cppFunction('
        NumericVector final_status(Dataframe data, CharacterVector NatureServe_statelevels, CharacterVector NatureServe_globallevels, CharacterVector ICUNlevels) {
          for (int i = 0; i < data.nrow(); i++) {
            // Extract the statuses for the current row
            std::string state1NatureServeStatus = as<std::string>(data(i, 3));
            std::string state2NatureServeStatus = as<std::string>(data(i, 4));
            std::string state3NatureServeStatus = as<std::string>(data(i, 5));
            std::string state4NatureServeStatus = as<std::string>(data(i, 6));
            std::string GlobalNatureServeStatus = as<std::string>(data(i, 7));
            std::string ICUNStatus = as<std::string>(data(i, 8));
            
            int state1NatureServe_value = std::distance(NatureServe_statelevels.begin(), std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state1NatureServeStatus));
            if (state1NatureServe_value != NA_INTEGER && state1NatureServe_value > 309) {
              state1NatureServe_value = 0;
            }
            
            int state2NatureServe_value = std::distance(NatureServe_statelevels.begin(), std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state2NatureServeStatus));
            if (state2NatureServe_value != NA_INTEGER && state2NatureServe_value > 309) {
              state2NatureServe_value = 0;
            }
            
            int state3NatureServe_value = std::distance(NatureServe_statelevels.begin(), std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state3NatureServeStatus));
            if (state3NatureServe_value != NA_INTEGER && state3NatureServe_value > 309) {
              state3NatureServe_value = 0;
            }
            
            int state4NatureServe_value = std::distance(NatureServe_statelevels.begin(), std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state4NatureServeStatus));
            if (state4NatureServe_value != NA_INTEGER && state4NatureServe_value > 309) {
              state4NatureServe_value = 0;
            }
            
            int GlobalNatureServe_value = std::distance(NatureServe_globallevels.begin(), std::find(NatureServe_globallevels.begin(), NatureServe_globallevels.end(), GlobalNatureServeStatus));
            if (GlobalNatureServe_value != NA_INTEGER && GlobalNatureServe_value > 309) {
              GlobalNatureServe_value = 0;
            }
            
            // Map statuses to numeric values using their respective level systems
            std::vector<int> numeric_values = {
              state1NatureServe_value,
              state2NatureServe_value,
              state3NatureServe_value,
              state4NatureServe_value,
              GlobalNatureServe_value,
              ICUNStatus.empty() ? 0 : std::distance(ICUNlevels.begin(), std::find(ICUNlevels.begin(), ICUNlevels.end(), ICUNStatus))
            };

            // Find the maximum numeric value
            int max_value = *std::max_element(numeric_values.begin(), numeric_values.end());

            // Determine which level system the maximum value belongs to
            if (std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state1NatureServeStatus) != NatureServe_statelevels.end()) {
              data(i, 9) = NatureServe_statelevels[max_value];
            } else if (std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state2NatureServeStatus) != NatureServe_statelevels.end()) {
              data(i, 9) = NatureServe_statelevels[max_value];
            } else if (std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state3NatureServeStatus) != NatureServe_statelevels.end()) {
              data(i, 9) = NatureServe_statelevels[max_value];
            } else if (std::find(NatureServe_statelevels.begin(), NatureServe_statelevels.end(), state4NatureServeStatus) != NatureServe_statelevels.end()) {
              data(i, 9) = NatureServe_statelevels[max_value];
            } else if (std::find(NatureServe_globallevels.begin(), NatureServe_globallevels.end(), GlobalNatureServeStatus) != NatureServe_globallevels.end()) {
              data(i, 9) = NatureServe_globallevels[max_value];
            } else if (std::find(ICUNlevels.begin(), ICUNlevels.end(), ICUNStatus) != ICUNlevels.end()) {
              data(i, 9) = ICUNlevels[max_value];
            }
          }
          return data;
        }
      ')
      return final_status(data, NatureServe_statelevels, NatureServe_globallevels, ICUNlevels)
    }
  }
}
