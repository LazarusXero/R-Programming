rankhospital <- function(state, outcome, num = "best"){
  
  #check the state argument to see if it is valid
  statelist <- as.vector(c(state.abb,"DC", "PR", "VI", "GU"))
  
  validstate <- match(state,statelist,nomatch = 0)
  
  if(validstate == 0) stop("invalid state")
  
  
  #check the outcome argument to see if it is valid
  validoutcomematch <- c("heart attack", "heart failure", "pneumonia")
  
  validoutcome <- match(outcome, validoutcomematch, nomatch = 0)
  
  if(validoutcome == 0) stop("invalid outcome")
  
  
  #setting a code for the outcome value
  outcomelist <- c(11,17,23)
  outcomecode <- outcomelist[validoutcome]
  
  
  #read the data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  statedata <- data[data$State == state,]
  
  #sort the data by outcome (coerced to numeric) and then by Hospital Name - warnings suppressed
  statedata <- suppressWarnings(statedata[order(as.numeric(statedata[,outcomecode]), statedata[,2]),])
  
  a <- statedata[,2]                    #save vector of Hospital names to a
  b <- suppressWarnings(as.numeric(as.character(statedata[,outcomecode])))  #save vector of outcomes to b
  temp <- as.data.frame(cbind(a,b), stringsAsFactors = FALSE)     #bind columns to temp dataframe
  temp <- temp[complete.cases(temp),]   #remove incomplete cases
  c <- as.character(c(1:nrow(temp)))    #save vector of ranks to c equal to number of rows in temp
  temp <- as.data.frame(cbind(temp,c), stringsAsFactors = FALSE)  #bind rank vector to temp
  
  if(num == "best") rank <- 1
  
  else if(num == "worst") rank <- nrow(temp)
  
  else rank <- num
    
  return(temp[rank,1])
  
}
