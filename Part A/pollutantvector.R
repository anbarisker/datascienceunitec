#Name: Anbarasan
#StudentID: 1508153

# Write a function named 'pollutantvector' 
# that returns a vector of those pollutants (sulfate or nitrate) 
# whose values are greater than 'p', across a specified list of monitors. 
# The function 'pollutantvector' takes four arguments: 'directory', 'pollutant', 'id' and 'p'. 
# Given a vector monitor ID numbers, 'pollutantvector' reads that
# monitors' particulate matter data from the directory specified in the '
# directory' argument and returns the ones more than a certain value ('p') 
# across all of the monitors, ignoring any missing values coded as NA.

#Start

pollutantvector <-function(directory,pollutant,id=1:332,p)
{
  #Directory_files <-list.files(directory,full.names = TRUE)
  #result will be the dataframe
  #c is combine values in vector/list
  result <-c(Date=character(),sulfate=numeric(),nitrate=numeric(),ID=integer())
  
  
  #Check input pollutant if vaild
  if((pollutant !="sulfate") && (pollutant !="nitrate"))
  {
    print(paste("Error: input pollutant",pollutant,"is invaild. Should be sulfate or nitrate"))
    return()
  }
  #Check input id is between the range
  if(1>min(id) | 332<max(id))
  {
    print("Error: id value should between 1-332")
    return()
  }
  
  Directory_File <-list.files(directory,full.names = TRUE)
  for(i in id)
  {
    records <-read.csv(Directory_File[i])
    #apply : Returns a vector or array or list of values obtained by applying a function to margins of an array or matrix.
    #so array will records[pollutant],margin 1 then where function(x) takes each and check if its not NA and more than p value
    vaildlistdata <-apply(records[pollutant], 1, function(x) (!is.na(x)&&(x > p)))
    data <-records[vaildlistdata,]
    result<-rbind(result,data)
  }
 
  return(result)
}