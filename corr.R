source("complete.R")

corr <- function(directory, threshold = 0) {
  corr_vector <- vector()
  index <- 1
  
  # Get complete cases for all monitors
  complete_cases <- complete("specdata", 1:332)
  
  # Get all monitor IDs that have complete cases greater than the threshold
  desired_monitors <- complete_cases[complete_cases[,"nobs"] > threshold,]
  desired_monitors <- desired_monitors["id"]
  
  if (nrow(desired_monitors) > 0) {
    # Calculate correlation for each monitor
    for (monitor in 1:nrow(desired_monitors)) {
      ## Determine filename
      # ID number is single digit
      if (monitor < 10) {
        filename <- paste("00", monitor, ".csv", sep="")
      }
      # ID number is double digit
      else if (monitor >= 10 & monitor < 100) {
        filename <- paste("0", monitor, ".csv", sep="")
      }
      # ID number is triple digit
      else if (monitor >= 100) {
        filename <- paste(monitor, ".csv", sep="")
      }
      
      # Get data from CSV file
      filepath <- paste(directory, "\\", filename, sep="")
      complete_data <- read.csv(filepath)
      
      # Extract the pollutant columns
      nitrate_data <- complete_data[,"nitrate"]
      sulfate_data <- complete_data[,"sulfate"]
      
      # Get the data which show complete cases
      complete_cases_vector <- complete.cases(nitrate_data, sulfate_data)
      
      if (length(complete_cases_vector[complete_cases_vector == TRUE]) > 0) {
        pollutant_data_frame <- data.frame(nitrate=nitrate_data[complete_cases_vector],
                                           sulfate=sulfate_data[complete_cases_vector])
        
        # Calculate correlation of Nitrate and Sulfate data
        correlation <- cor(pollutant_data_frame, use="complete.obs")
        corr_vector[index] <- correlation["nitrate", "sulfate"]
        index <- index + 1  
      }
  }
    
  }
  
  else {
    corr_vector <- numeric()
  }
  
  corr_vector
}