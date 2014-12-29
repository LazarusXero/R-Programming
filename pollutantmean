## PollutantMean Function for R Programming Course

pollutantmean <- function(directory, pollutant, id = 1:332){
        ## information below taken directly from project summary:
        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)

##reformat the ID vector to add leading zeros when the ID is less then 3 digits        
newid <- formatC(id, width = 3, format = "d", flag = "0")        

##create a filename vector for all of the IDs provided in argument
filename <- paste(directory,"/",newid,".csv",sep="")

tempvector <- vector()

##for loop from 1 to the number of stations to check
for(i in seq_along(newid)){
        
        ##persist file contents to dataset
        data <- read.csv(filename[i])
        
        ##append new data to existing data
        tempvector <- append(tempvector,data[,pollutant])

}

##calculate and print out final mean of all values in matrix
print(formatC(mean(tempvector, na.rm = TRUE),digits = 3, format = "f"))
        
}
