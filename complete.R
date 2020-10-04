complete <- function(directory, id = 1:332) {
  filename <- 0
  complete_observations <- data.frame(
    id = integer(),
    nobs = integer()
  )
  complete_data_cntr <- 1
  index <- 1
  
  for (id_num in id) {
    ## Determine filename
    # ID number is single digit
    if (id_num < 10) {
      filename <- paste("00", id_num, ".csv", sep="")
    }
    # ID number is double digit
    else if (id_num >= 10 & id_num < 100) {
      filename <- paste("0", id_num, ".csv", sep="")
    }
    # ID number is triple digit
    else if (id_num >= 100) {
      filename <- paste(id_num, ".csv", sep="")
    }
    
    filepath <- paste(directory, "\\", filename, sep="")
    complete_data <- read.csv(filepath)
    
    ## Extract the each pollutant and see which values are NA
    nitrate_data <- complete_data[,"nitrate"]
    sulfate_data <- complete_data[,"sulfate"]
    
    # Get the number of complete cases
    complete_cases_vector <- complete.cases(nitrate_data, sulfate_data)
    complete_cases <- length(complete_cases_vector[complete_cases_vector == TRUE])
    
    # Create data frame with newly found data
    complete_cases_row <- data.frame(
      id = id_num,
      nobs = complete_cases
    )
    
    # Append new frame as row to existing data frame
    complete_observations <- rbind(complete_observations, complete_cases_row)
    
    index <- index + 1
    
    # Clear the complete_cases_row data frame
    complete_cases_row <- data.frame(
      id = integer(),
      nobs = integer()
    )
    
  }
  
  complete_observations
}