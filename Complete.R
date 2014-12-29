complete <- function(directory, id = 1:332)

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
  
print(outputdata)

}
