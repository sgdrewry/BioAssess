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
      for (i in 1:nrow(data)) {
        # Extract the statuses for the current row
        state1NatureServeStatus <- as.character(data[i, 3])
        GlobalNatureServeStatus <- as.character(data[i, 4])
        ICUNStatus <- as.character(data[i, 5])
        
        # Map statuses to numeric values using their respective level systems
        numeric_values <- c(
          ifelse(is.na(state1NatureServeStatus), Inf, match(state1NatureServeStatus, NatureServe_statelevels)),
          ifelse(is.na(GlobalNatureServeStatus), Inf, match(GlobalNatureServeStatus, NatureServe_globallevels)),
          ifelse(is.na(ICUNStatus), Inf, match(ICUNStatus, ICUNlevels))
        )
        
        # Find the minimum numeric value
        min_value <- min(numeric_values, na.rm = TRUE)
        
        # Assign the corresponding status or NA if all are Inf
        if (min_value == Inf) {
          data$Final_status[i] <- NA
        } else {
          # Determine which level system the minimum value belongs to
          if (min_value %in% match(state1NatureServeStatus, NatureServe_statelevels)) {
            data$Final_status[i] <- NatureServe_statelevels[min_value]
          } else if (min_value %in% match(GlobalNatureServeStatus, NatureServe_globallevels)) {
            data$Final_status[i] <- NatureServe_globallevels[min_value]
          } else if (min_value %in% match(ICUNStatus, ICUNlevels)) {
            data$Final_status[i] <- ICUNlevels[min_value]
          }
        }
      }
      return(data)
    }
    if (comparison == "least concern") {
      # Initialize the Final_status column
      data$Final_status <- NA
      
      # Loop through each row
      for (i in 1:nrow(data)) {
        # Extract the statuses for the current row
        state1NatureServeStatus <- as.character(data[i, 3])
        GlobalNatureServeStatus <- as.character(data[i, 4])
        ICUNStatus <- as.character(data[i, 5])
        
        
        state1NatureServe_value <- match(state1NatureServeStatus, NatureServe_statelevels)
        if (!is.na(state1NatureServe_value) && state1NatureServe_value > 309) {
          state1NatureServe_value <- 0
        }
        
        GlobalNatureServe_value <- match(GlobalNatureServeStatus, NatureServe_globallevels)
        if (!is.na(GlobalNatureServe_value) && GlobalNatureServe_value > 309) {
          GlobalNatureServe_value <- 0
        }
        
        # Map statuses to numeric values using their respective level systems
        numeric_values <- c(
          state1NatureServe_value, 
          GlobalNatureServe_value,
          ifelse(is.na(ICUNStatus), 0, match(ICUNStatus, ICUNlevels))
        )
        
        # Find the maximum numeric value
        max_value <- max(numeric_values, na.rm = TRUE)
        
        
        # Determine which level system the maximum value belongs to
        if (max_value %in% match(state1NatureServeStatus, NatureServe_statelevels)) {
          data$Final_status[i] <- NatureServe_statelevels[max_value]
        } else if (max_value %in% match(GlobalNatureServeStatus, NatureServe_globallevels)) {
          data$Final_status[i] <- NatureServe_globallevels[max_value]
        } else if (max_value %in% match(ICUNStatus, ICUNlevels)) {
          data$Final_status[i] <- ICUNlevels[max_value]
        }
      } 
      return(data)
    }
  }    
  
  # for 2 states
  if (!is.null(state2) && is.null(state3) && is.null(state4)) {
    if (comparison == "most concern") {
      # Initialize the Final_status column
      data$Final_status <- NA
      
      # Loop through each row
      for (i in 1:nrow(data)) {
        # Extract the statuses for the current row
        state1NatureServeStatus <- as.character(data[i, 3])
        state2NatureServeStatus <- as.character(data[i, 4])
        GlobalNatureServeStatus <- as.character(data[i, 5])
        ICUNStatus <- as.character(data[i, 6])
        
        # Map statuses to numeric values using their respective level systems
        numeric_values <- c(
          ifelse(is.na(state1NatureServeStatus), Inf, match(state1NatureServeStatus, NatureServe_statelevels)),
          ifelse(is.na(state2NatureServeStatus), Inf, match(state2NatureServeStatus, NatureServe_statelevels)),
          ifelse(is.na(GlobalNatureServeStatus), Inf, match(GlobalNatureServeStatus, NatureServe_globallevels)),
          ifelse(is.na(ICUNStatus), Inf, match(ICUNStatus, ICUNlevels))
        )
        
        # Find the minimum numeric value
        min_value <- min(numeric_values, na.rm = TRUE)
        
        # Assign the corresponding status or NA if all are Inf
        if (min_value == Inf) {
          data$Final_status[i] <- NA
        } else {
          # Determine which level system the minimum value belongs to
          if (min_value %in% match(state1NatureServeStatus, NatureServe_statelevels)) {
            data$Final_status[i] <- NatureServe_statelevels[min_value]
          } else if (min_value %in% match(state2NatureServeStatus, NatureServe_statelevels)) {
            data$Final_status[i] <- NatureServe_statelevels[min_value]
          } else if (min_value %in% match(GlobalNatureServeStatus, NatureServe_globallevels)) {
            data$Final_status[i] <- NatureServe_globallevels[min_value]
          } else if (min_value %in% match(ICUNStatus, ICUNlevels)) {
            data$Final_status[i] <- ICUNlevels[min_value]
          }
        }
      } 
      return(data)
    }
    if (comparison == "least concern") {         
      # Initialize the Final_status column
      data$Final_status <- NA
      
      # Loop through each row
      for (i in 1:nrow(data)) {
        # Extract the statuses for the current row
        state1NatureServeStatus <- as.character(data[i, 3])
        state2NatureServeStatus <- as.character(data[i, 4])
        GlobalNatureServeStatus <- as.character(data[i, 5])
        ICUNStatus <- as.character(data[i, 6])
        
        
        state1NatureServe_value <- match(state1NatureServeStatus, NatureServe_statelevels)
        if (!is.na(state1NatureServe_value) && state1NatureServe_value > 309) {
          state1NatureServe_value <- 0
        }
        
        state2NatureServe_value <- match(state2NatureServeStatus, NatureServe_statelevels)
        if (!is.na(state2NatureServe_value) && state2NatureServe_value > 309) {
          state2NatureServe_value <- 0
        }
        
        GlobalNatureServe_value <- match(GlobalNatureServeStatus, NatureServe_globallevels)
        if (!is.na(GlobalNatureServe_value) && GlobalNatureServe_value > 309) {
          GlobalNatureServe_value <- 0
        }
        
        # Map statuses to numeric values using their respective level systems
        numeric_values <- c(
          state1NatureServe_value, 
          state2NatureServe_value,
          GlobalNatureServe_value,
          ifelse(is.na(ICUNStatus), 0, match(ICUNStatus, ICUNlevels))
        )
        
        # Find the maximum numeric value
        max_value <- max(numeric_values, na.rm = TRUE)
        
        
        # Determine which level system the maximum value belongs to
        if (max_value %in% match(state1NatureServeStatus, NatureServe_statelevels)) {
          data$Final_status[i] <- NatureServe_statelevels[max_value]
        } else if (max_value %in% match(state2NatureServeStatus, NatureServe_statelevels)) {
          data$Final_status[i] <- NatureServe_statelevels[max_value]
        } else if (max_value %in% match(GlobalNatureServeStatus, NatureServe_globallevels)) {
          data$Final_status[i] <- NatureServe_globallevels[max_value]
        } else if (max_value %in% match(ICUNStatus, ICUNlevels)) {
          data$Final_status[i] <- ICUNlevels[max_value]
        }
      } 
      return(data)
    }
  }
  
  # for 3 states
  if (!is.null(state2) && !is.null(state3) && is.null(state4)) {
    if (comparison == "most concern") {
      # Initialize the Final_status column
      data$Final_status <- NA
      
      # Loop through each row
      for (i in 1:nrow(data)) {
        # Extract the statuses for the current row
        state1NatureServeStatus <- as.character(data[i, 3])
        state2NatureServeStatus <- as.character(data[i, 4])
        state3NatureServeStatus <- as.character(data[i, 5])
        GlobalNatureServeStatus <- as.character(data[i, 6])
        ICUNStatus <- as.character(data[i, 7])
        
        # Map statuses to numeric values using their respective level systems
        numeric_values <- c(
          ifelse(is.na(state1NatureServeStatus), Inf, match(state1NatureServeStatus, NatureServe_statelevels)),
          ifelse(is.na(state2NatureServeStatus), Inf, match(state2NatureServeStatus, NatureServe_statelevels)),
          ifelse(is.na(state3NatureServeStatus), Inf, match(state3NatureServeStatus, NatureServe_statelevels)),
          ifelse(is.na(GlobalNatureServeStatus), Inf, match(GlobalNatureServeStatus, NatureServe_globallevels)),
          ifelse(is.na(ICUNStatus), Inf, match(ICUNStatus, ICUNlevels))
        )
        
        # Find the minimum numeric value
        min_value <- min(numeric_values, na.rm = TRUE)
        
        # Assign the corresponding status or NA if all are Inf
        if (min_value == Inf) {
          data$Final_status[i] <- NA
        } else {
          # Determine which level system the minimum value belongs to
          if (min_value %in% match(state1NatureServeStatus, NatureServe_statelevels)) {
            data$Final_status[i] <- NatureServe_statelevels[min_value]
          } else if (min_value %in% match(state2NatureServeStatus, NatureServe_statelevels)) {
            data$Final_status[i] <- NatureServe_statelevels[min_value]
          } else if (min_value %in% match(state3NatureServeStatus, NatureServe_statelevels)) {
            data$Final_status[i] <- NatureServe_statelevels[min_value]
          } else if (min_value %in% match(GlobalNatureServeStatus, NatureServe_globallevels)) {
            data$Final_status[i] <- NatureServe_globallevels[min_value]
          } else if (min_value %in% match(ICUNStatus, ICUNlevels)) {
            data$Final_status[i] <- ICUNlevels[min_value]
          }
        }
      }  
      return(data)
    }
    if (comparison == "least concern") {
      data$Final_status <- NA
      
      # Loop through each row
      for (i in 1:nrow(data)) {
        # Extract the statuses for the current row
        state1NatureServeStatus <- as.character(data[i, 3])
        state2NatureServeStatus <- as.character(data[i, 4])
        state3NatureServeStatus <- as.character(data[i, 5])
        GlobalNatureServeStatus <- as.character(data[i, 6])
        ICUNStatus <- as.character(data[i, 7])
        
        
        state1NatureServe_value <- match(state1NatureServeStatus, NatureServe_statelevels)
        if (!is.na(state1NatureServe_value) && state1NatureServe_value > 309) {
          state1NatureServe_value <- 0
        }
        
        state2NatureServe_value <- match(state2NatureServeStatus, NatureServe_statelevels)
        if (!is.na(state2NatureServe_value) && state2NatureServe_value > 309) {
          state2NatureServe_value <- 0
        }
        
        state3NatureServe_value <- match(state3NatureServeStatus, NatureServe_statelevels)
        if (!is.na(state3NatureServe_value) && state3NatureServe_value > 309) {
          state3NatureServe_value <- 0
        }
        
        GlobalNatureServe_value <- match(GlobalNatureServeStatus, NatureServe_globallevels)
        if (!is.na(GlobalNatureServe_value) && GlobalNatureServe_value > 309) {
          GlobalNatureServe_value <- 0
        }
        
        # Map statuses to numeric values using their respective level systems
        numeric_values <- c(
          state1NatureServe_value, 
          state2NatureServe_value,
          state3NatureServe_value,
          GlobalNatureServe_value,
          ifelse(is.na(ICUNStatus), 0, match(ICUNStatus, ICUNlevels))
        )
        
        # Find the maximum numeric value
        max_value <- max(numeric_values, na.rm = TRUE)
        
        
        # Determine which level system the maximum value belongs to
        if (max_value %in% match(state1NatureServeStatus, NatureServe_statelevels)) {
          data$Final_status[i] <- NatureServe_statelevels[max_value]
        } else if (max_value %in% match(state2NatureServeStatus, NatureServe_statelevels)) {
          data$Final_status[i] <- NatureServe_statelevels[max_value]
        } else if (max_value %in% match(state3NatureServeStatus, NatureServe_statelevels)) {
          data$Final_status[i] <- NatureServe_statelevels[max_value]
        } else if (max_value %in% match(GlobalNatureServeStatus, NatureServe_globallevels)) {
          data$Final_status[i] <- NatureServe_globallevels[max_value]
        } else if (max_value %in% match(ICUNStatus, ICUNlevels)) {
          data$Final_status[i] <- ICUNlevels[max_value]
        }
      } 
      return(data)
    }
  }
  
  # for 4 states
  if (!is.null(state2) && !is.null(state3) && !is.null(state4)) {
    if (comparison == "most concern") {
      # Initialize the Final_status column
      data$Final_status <- NA
      
      # Loop through each row
      for (i in 1:nrow(data)) {
        # Extract the statuses for the current row
        state1NatureServeStatus <- as.character(data[i, 3])
        state2NatureServeStatus <- as.character(data[i, 4])
        state3NatureServeStatus <- as.character(data[i, 5])
        state4NatureServeStatus <- as.character(data[i, 6])
        GlobalNatureServeStatus <- as.character(data[i, 7])
        ICUNStatus <- as.character(data[i, 8])
        
        # Map statuses to numeric values using their respective level systems
        numeric_values <- c(
          ifelse(is.na(state1NatureServeStatus), Inf, match(state1NatureServeStatus, NatureServe_statelevels)),
          ifelse(is.na(state2NatureServeStatus), Inf, match(state2NatureServeStatus, NatureServe_statelevels)),
          ifelse(is.na(state3NatureServeStatus), Inf, match(state3NatureServeStatus, NatureServe_statelevels)),
          ifelse(is.na(state4NatureServeStatus), Inf, match(state4NatureServeStatus, NatureServe_statelevels)),
          ifelse(is.na(GlobalNatureServeStatus), Inf, match(GlobalNatureServeStatus, NatureServe_globallevels)),
          ifelse(is.na(ICUNStatus), Inf, match(ICUNStatus, ICUNlevels))
        )
        
        # Find the minimum numeric value
        min_value <- min(numeric_values, na.rm = TRUE)
        
        # Assign the corresponding status or NA if all are Inf
        if (min_value == Inf) {
          data$Final_status[i] <- NA
        } else {
          # Determine which level system the minimum value belongs to
          if (min_value %in% match(state1NatureServeStatus, NatureServe_statelevels)) {
            data$Final_status[i] <- NatureServe_statelevels[min_value]
          } else if (min_value %in% match(state2NatureServeStatus, NatureServe_statelevels)) {
            data$Final_status[i] <- NatureServe_statelevels[min_value]
          } else if (min_value %in% match(state3NatureServeStatus, NatureServe_statelevels)) {
            data$Final_status[i] <- NatureServe_statelevels[min_value]
          } else if (min_value %in% match(state4NatureServeStatus, NatureServe_statelevels)) {
            data$Final_status[i] <- NatureServe_statelevels[min_value]
          } else if (min_value %in% match(GlobalNatureServeStatus, NatureServe_globallevels)) {
            data$Final_status[i] <- NatureServe_globallevels[min_value]
          } else if (min_value %in% match(ICUNStatus, ICUNlevels)) {
            data$Final_status[i] <- ICUNlevels[min_value]
          }
        }
      }  
      return(data)
    }  
    if (comparison == "least concern") {
      data$Final_status <- NA
      
      # Loop through each row
      for (i in 1:nrow(data)) {
        # Extract the statuses for the current row
        state1NatureServeStatus <- as.character(data[i, 3])
        state2NatureServeStatus <- as.character(data[i, 4])
        state3NatureServeStatus <- as.character(data[i, 5])
        state3NatureServeStatus <- as.character(data[i, 6])
        GlobalNatureServeStatus <- as.character(data[i, 7])
        ICUNStatus <- as.character(data[i, 8])
        
        
        state1NatureServe_value <- match(state1NatureServeStatus, NatureServe_statelevels)
        if (!is.na(state1NatureServe_value) && state1NatureServe_value > 309) {
          state1NatureServe_value <- 0
        }
        
        state2NatureServe_value <- match(state2NatureServeStatus, NatureServe_statelevels)
        if (!is.na(state2NatureServe_value) && state2NatureServe_value > 309) {
          state2NatureServe_value <- 0
        }
        
        state3NatureServe_value <- match(state3NatureServeStatus, NatureServe_statelevels)
        if (!is.na(state3NatureServe_value) && state3NatureServe_value > 309) {
          state3NatureServe_value <- 0
        }
        
        state4NatureServe_value <- match(state4NatureServeStatus, NatureServe_statelevels)
        if (!is.na(state4NatureServe_value) && state4NatureServe_value > 309) {
          state4NatureServe_value <- 0
        }
        
        GlobalNatureServe_value <- match(GlobalNatureServeStatus, NatureServe_globallevels)
        if (!is.na(GlobalNatureServe_value) && GlobalNatureServe_value > 309) {
          GlobalNatureServe_value <- 0
        }
        
        # Map statuses to numeric values using their respective level systems
        numeric_values <- c(
          state1NatureServe_value, 
          state2NatureServe_value,
          state3NatureServe_value,
          state4NatureServe_value,
          GlobalNatureServe_value,
          ifelse(is.na(ICUNStatus), 0, match(ICUNStatus, ICUNlevels))
        )
        
        # Find the maximum numeric value
        max_value <- max(numeric_values, na.rm = TRUE)
        
        
        # Determine which level system the maximum value belongs to
        if (max_value %in% match(state1NatureServeStatus, NatureServe_statelevels)) {
          data$Final_status[i] <- NatureServe_statelevels[max_value]
        } else if (max_value %in% match(state2NatureServeStatus, NatureServe_statelevels)) {
          data$Final_status[i] <- NatureServe_statelevels[max_value]
        } else if (max_value %in% match(state3NatureServeStatus, NatureServe_statelevels)) {
          data$Final_status[i] <- NatureServe_statelevels[max_value]
        } else if (max_value %in% match(state4NatureServeStatus, NatureServe_statelevels)) {
          data$Final_status[i] <- NatureServe_statelevels[max_value]
        } else if (max_value %in% match(GlobalNatureServeStatus, NatureServe_globallevels)) {
          data$Final_status[i] <- NatureServe_globallevels[max_value]
        } else if (max_value %in% match(ICUNStatus, ICUNlevels)) {
          data$Final_status[i] <- ICUNlevels[max_value]
        }
      } 
      return(data)
    }
  }
}
