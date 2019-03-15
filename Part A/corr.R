#Name: Anbarasan
#StudentID: 1508153

corr <- function(directory, threshold=0)
{
  # 'directory' is a cjaracter vector of length 1 indicating the
  # location of the csv files
  # 'threshold' is a numeric vector of len 1 indicating the
  # number of completely observed observations (on all variables)
  # nitrate and sulfate; the default is 0
  
  # Return a numeric vector of correlations
  # Note: do not round the results!
  
  Directory_File <-list.files(directory,full.names = TRUE)
  rawdatas <- complete(directory)
  # subset: Return subsets of vectors, matrices or data frames which meet conditions.
  subract_data <-subset(rawdatas,nobs>threshold)
  result <-integer(0)
 
   for(i in subract_data$id){
    data <- read.csv(Directory_File[i])
    result <- c(result,cor(data$nitrate,data$sulfate,use="complete.obs"))
  }
  result
  

  #method 2 can get same values but this is not using the complete.R
  #result <-vector(mode="numeric",length = 0)
  # for(i in 1:length(Directory_File))
  # {
  #   records <-read.csv(Directory_File[i])
  #   tsum <-sum((!is.na(records$sulfate)) & (!is.na(records$nitrate)))
  #   if(tsum > threshold)
  #  {
  #    temp <-records[which(!is.na(records$sulfate)),]
  #    get_data <-temp[which(!is.na(temp$nitrate)),]
  #    result <-c(result,cor(get_data$sulfate,get_data$nitrate))
  #  }
  #}
  #result
  
}