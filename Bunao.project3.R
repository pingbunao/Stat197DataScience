setwd("C:/Users/Ma.Ofil/Desktop/Project3")

library(readr)


#PART 1 (Plot the 30-day mortality rates for Heart Attack) -----
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)


#making a histogram
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11], xlab = "30-day Death Rate", main = "30-day Death Rate for 
     Heart Attacks")


#PART 2 (Finding the Best Hospital in a State) -----
best <- function(state, outcome){
  library(dplyr)
  
  outcomeData <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  bestHospital <- NULL
  rate <- NULL
  
  #Function for Heart Attack ----
  inHeartAttack <- function(s, x){
    x <- select(x, State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    filtered <- x[x$State==s & 
          x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != 'Not Available' ,
          c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
    
    #Sort by Hospital.Name
    sortedData <- arrange(filtered, Hospital.Name)
    
    #Sort by Rate
    sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    sortedData <- arrange(sortedData, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    
    #Return
    bestHosp <- sortedData
    
  }
  
  #Function for Heart Failure ----
  inHeartFailure <- function(s, x){
    x <- select(x, State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    filtered <- x[x$State==s & 
          x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != 'Not Available' ,
          c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
    
    #Sort by Hospital.Name
    sortedData <- arrange(filtered, Hospital.Name)
    
    #Sort by Rate
    sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    sortedData <- arrange(sortedData, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    
    #Return
    bestHosp <- sortedData
    
  }
  
  
  
  
  #Function for Pneumonia ----
  inPneumonia <- function(s, x){
    x <- select(x, State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    filtered <- x[x$State==s & 
        x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != 'Not Available' ,
        c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
    
    #Sort by Hospital.Name
    sortedData <- arrange(filtered, Hospital.Name)
    
    #Sort by Rate
    sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    sortedData <- arrange(sortedData, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    
    #Return
    bestHosp <- sortedData
    
  }
  

  #Checking if the outcome and state are valid -----
  if (outcome == "heart attack"){
    if (length(outcomeData[outcomeData$State == state,c("State")])>0){
      bh <- inHeartAttack(state, outcomeData)
      bestHospital <- bh[1,c("Hospital.Name")]
      rate <- bh[1,2]
    } else{
      print(paste("Error in best(", state, ", ", outcome,") : invalid state", sep=""))
    }
  } else if (outcome == "heart failure"){
    if (length(outcomeData[outcomeData$State == state,c("State")])>0){
      bh <- inHeartFailure(state, outcomeData)
      bestHospital <- bh[1,c("Hospital.Name")]
      rate <- bh[1,2]
      
    } else{
      print(paste("Error in best(", state, ", ", outcome,") : invalid state", sep=""))
    }
  } else if (outcome == "pneumonia"){
    if (length(outcomeData[outcomeData$State == state,c("State")])>0){
      bh <- inPneumonia(state, outcomeData)
      bestHospital <- bh[1,c("Hospital.Name")]
      rate <- bh[1,2]
    } else{
      print(paste("Error in best(", state, ", ", outcome,") : invalid state", sep=""))
    }
  } else{
    print(paste("Error in best(", state, ", ", outcome,") : invalid outcome", sep=""))
  }
  
  bestHospital
}
    
  


#PART 3 (Ranking Hospitals) -----
rankhospital <- function(state, outcome, num = "best"){
  library(dplyr)

  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  hospital <- NULL
  rate <- NULL
  range <- NULL
  
  #Function for Heart Attack
  inHeartAttack <- function(s, x){
    x <- select(x, State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    filtered <- x[x$State==s & x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != 'Not Available' ,c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
    
    #Sort by Hospital.Name
    sortedData <- arrange(filtered, Hospital.Name)
    
    #Sort by Rate
    sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    sortedData <- arrange(sortedData, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    
    #Return
    bestHosp <- sortedData
    
  }
  
  #Function for Heart Failure
  inHeartFailure <- function(s, x){
    x <- select(x, State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    filtered <- x[x$State==s & x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != 'Not Available' ,c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
    
    #Sort by Hospital.Name
    sortedData <- arrange(filtered, Hospital.Name)
    
    #Sort by Rate
    sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    sortedData <- arrange(sortedData, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    
    #Return
    bestHosp <- sortedData
    
  }
  
  #Function for Pneumonia
  inPneumonia <- function(s, x){
    x <- select(x, State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    filtered <- x[x$State==s & x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != 'Not Available' ,c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
    
    #Sort by Hospital.Name
    sortedData <- arrange(filtered, Hospital.Name)
    
    #Sort by Rate
    sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    sortedData <- arrange(sortedData, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    
    #Return
    bestHosp <- sortedData
    
  }
  

  getHospital <- function(ds, range){
    if (range == "best")
      ds[1, ]
    else if (range == "worst")
      ds[nrow(ds),]
    else if (is.numeric(range))
      ds[range,]
  }
  

  #Checking if the outcome and state are valid
  if (outcome == "heart attack"){
    if (length(outcomeData[outcomeData$State == state,c("State")])>0){
      bh <- inHeartAttack(state, outcomeData)
      bh <- getHospital(bh, num)
      hospital <- bh[,c("Hospital.Name")]
      rate <- bh[,2]
    } else{
      hospital <- paste("Error in best(", state, ", ", outcome,") : invalid state", sep="")
    }
  } else if (outcome == "heart failure"){
    if (length(outcomeData[outcomeData$State == state,c("State")])>0){
      bh <- inHeartFailure(state, outcomeData)
      bh <- getHospital(bh, num)
      hospital <- bh[,c("Hospital.Name")]
      rate <- bh[,2]
    } else{
      hospital <- paste("Error in best(", state, ", ", outcome,") : invalid state", sep="")
    }
  } else if (outcome == "pneumonia"){

    if (length(outcomeData[outcomeData$State == state,c("State")])>0){
      bh <- inPneumonia(state, outcomeData)
      bh <- getHospital(bh, num)
      hospital <- bh[,c("Hospital.Name")]
      rate <- bh[,2]
    } else{
      hospital <- paste("Error in best(", state, ", ", outcome,") : invalid state", sep="")
    }
  } else{
    hospital <- paste("Error in best(", state, ", ", outcome,") : invalid outcome", sep="")
  }
  hospital
}
