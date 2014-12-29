corr <- function(directory, threshold = 0){

##assign the filename
##determine the number of completed records for the file
##determine if this meets the threshold
##if so, determine the correlation between nitrate and sulfate
##if not, return a vector with length of 0
##answer should be in vector form
##


}
corr <- function(directory, threshold=0,ids = 1:332){
  
  # calls the complete function to complete the outputdata data.frame
  # outputdata shows the id and the nobs
  complete(directory,ids)
  
  load(file = "outputdata.RData", envir = .GlobalEnv)
  
  corrids <- NA
  
  for(i in 1:nrow(outputdata)){
    
    if(outputdata[i,"nobs"] >= threshold){
     
      corrids <- append(corrids,outputdata[i,"id"])
    }
    
  }
  
print(corrids)

  
}

complete <- function(directory, id = 1:332){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
    
  newid <- formatC(id, width = 3, format = "d", flag = "0")
  
  filename <- paste(directory, "/", newid, ".csv", sep="")
  
  outputdata <- data.frame(id = 0, nobs = 0)
  
  #set for loop for number of monitors to read
  for(i in 1:length(id)){
    
    #persist file data to temporary dataframe
    tempdata <- read.csv(filename[i])
    
    #reset counter
    count <- 0
    
    #set for loop to review all rows in tempdata
    for(j in 1:nrow(tempdata)){
      
      #save a row of tempdata to a temporary vector to review
      tempvector <- as.vector(tempdata[j,])
      
      #if there is no NA value, iterate count by 1
      if((!is.na(tempvector[2]))&(!is.na(tempvector[3]))){
        count <- count + 1
      }
    }
    
    outputdata[i,"id"] <- id[i]
    outputdata[i,"nobs"] <- count
    
  }
  
#print(outputdata)
# removed print to work with corr function
save(outputdata, file = "outputdata.RData")
print(outputdata)
}
