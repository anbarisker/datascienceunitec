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
  
  # step 1: to list the files in the direcotry using list.files
  # e.g [1] file path/1.csv
  Directory_File <- list.files(directory,full.names = TRUE)
  
  # step 2: for-looping every csv file from Directory_File and row bind the data from it
  # to data.frame, dat
  for(i in id){
    pollutant_dataset <- rbind(data.frame(),read.csv(Directory_File[i]))
  }
  
  # step 3: to do the mean cal, with the input type of pollutant and removing
  # the na values
  return(mean(pollutant_dataset[,pollutant],na.rm = TRUE))
  
}