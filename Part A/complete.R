#Name: Anbarasan
#StudentID: 1508153

complete <- function(directory, id=1:332)
{
  ## directory is location of the csv files
  ## id is the montior ID number to be used
  ## Return a data frame of the form:
  ## id nobs
  ## 1 117
  ## 2 1047
  ## .. 
  ## Where 'id' is the monitor ID number and 'nobs' is the no. of complete case
  
  Directory_File <-list.files(directory,full.names = TRUE)
  # step 2 create a empty vector object
  v <-vector()
  # do for loop, to get the data for every csv by reading the file
  # store each records in vector with sum of compltete.case of the records
  # without na values
  for(i in 1:length(id))
  {
    #read.csv -> reads a file in table format and creates a date frame from it.
    records <-c(read.csv(Directory_File[id[i]]))
    # complete.cases -> Return a logical vector indicating which cases are complete, i.e., have no missing values.
    v[i] <-sum(complete.cases(records))
    
  }
  #create data.frame with Id, with the value of nobs
  final_data <-data.frame(id,nobs=v)
  return(final_data)
}