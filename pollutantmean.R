pollutantmean <- function(directory, pollutant, id = 1:332){
  
  filename <- 0
  total_averages <- list()
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
    
    ## Extract the requested pollutant
    pollutant_data <- complete_data[,pollutant]
    
    ## Clean data by removing any NA values
    na_in_data <- is.na(pollutant_data)
    pollutant_data <- pollutant_data[!na_in_data]
    
    if (length(pollutant_data) > 0) {
      # Calculate of specific monitor
      pollutant_mean <- mean(pollutant_data)
      
      #Append mean to the total averages list
      total_averages[index] <- pollutant_mean
      index <- index + 1 
    }
    
  }
  
  # Compute mean of all desired monitors
  mean <- mean(as.numeric(total_averages), na.rm = TRUE)
  mean
  
}