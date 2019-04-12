#Name: Anbarasan
#StudentID: 1508153

pollutantmean <- function(directory,pollutant,id = 1:332)
{
  ## directory is a character vector  of length 1 indicating
  ## the location of the csv file
  
  ## pollutant is a character vector of length 1 indicating the name of
  ## the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## id is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  
  #Start
  
  if(pollutant != 'sulfate' && pollutant != 'nitrate') {
    print("Error: please user only sulfate or nitrate as pollutant")
    return(NA)
  }
  
  if(1 > min(id) || 332 < max(id)) {
    print(paste("Error: id is out of range."))
    return(NA)
  }
  
  # step 1: to list the files in the direcotry using list.files
  # list.files will create character vector of the names of files.
  # e.g [1] file path/1.csv
  Directory_File <- list.files(directory,full.names = TRUE)
  
  # step 2: for-looping every csv file from Directory_File
  # read.csv to read each file
  # total_value will be a vector to hole the pollutant values/data
  total_value <-c()
   for(i in id){
    pollutant_dataset <- read.csv(Directory_File[i])
    #below method doing adding total_value, with data without NA
    total_value <-c(total_value,pollutant_dataset[!is.na(pollutant_dataset[,pollutant]),pollutant])
  }
  
  # step 3: to do the mean cal
  return(mean(total_value))
  
}