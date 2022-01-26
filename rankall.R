rankall <- function(outcome, num = "best") {
  ## Read data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings="Not Available")
  
  ## Check that inputs are valid
  validOutcome = c("heart attack","heart failure","pneumonia")
  if (!outcome %in% validOutcome) { stop("invalid outcome")}
  
  validState = sort(unique(data[,7]))
  ##if (!state %in% validState) stop("invalid state")
  
  ## convert outcome
  fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- fullColName[match(outcome,validOutcome)]
  
  ## Find rank for hospital by state
  hospital<-character(0)
  
  for (i in seq_along(validState)) {
    ## Return hospital name in that state with the given rank 30-day death rate
    data.state <- data[data$State==validState[i],]
    
    # order data
    sorted.data.state <- data.state[order(as.numeric(data.state[[colName]]),data.state[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
    
    this.num = num
    if (this.num=="best") this.num = 1
    if (this.num=='worst') this.num = nrow(sorted.data.state)
    
    hospital[i] <- sorted.data.state[this.num,"Hospital.Name"]
  }
  
  ## Return a data frame with the hospital names and the (abbreviated) state name
  data.frame(hospital=hospital,state=validState,row.names=validState)
}
