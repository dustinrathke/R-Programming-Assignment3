rankhospital <- function(state, outcome, num = "best") {
  ## Read data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings="Not Available")
  
  ## Checking that inputs are valid
  validOutcome = c("heart attack","heart failure","pneumonia")
  if (!outcome %in% validOutcome) { stop("invalid outcome")}
  
  validState = unique(data[,7])
  if (!state %in% validState) stop("invalid state")
  
  ## convert outcome name
  fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- fullColName[match(outcome,validOutcome)]
  
  ## Gives hospital name with rank
  data.state <- data[data$State==state,]
  
  # Order the data
  sorted.data.state <- data.state[order(as.numeric(data.state[[colName]]),data.state[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
  
  if (num=="best") num = 1
  if (num=='worst') num = nrow(sorted.data.state)
  
  sorted.data.state[num,"Hospital.Name"]
}