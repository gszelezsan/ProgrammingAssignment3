# --------------------------------------------------------------------------------------------------------------------
# Coursera: R programming → Week 4
# Peer-graded Assignment: Programming Assignment 3: Hospital Quality (best.R)
# --------------------------------------------------------------------------------------------------------------------
# 
# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
# in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of “heart attack”, “heart failure”, or “pneumonia”. 
# Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.
# Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
# be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals “b”, “c”,
# and “f” are tied for best, then hospital “b” should be returned).

# The function should check the validity of its arguments. If an invalid state value is passed to best, the
# function should throw an error via the stop function with the exact message “invalid state”. If an invalid
# outcome value is passed to best, the function should throw an error via the stop function with the exact
# message “invalid outcome”.

setwd("C:/Users/Gyorgy Szelezsan/Documents/GitHub/ProgrammingAssignment3")

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

best <- function(state, outcome) {
      options(warn=-1)                    # turning OFF warning messages (because they annoy me)
      
      ## Read outcome data
      oocm <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      # read table, and ensure the Provider.Number remains with leading zeros
      oocm[, c(11,17,23)] <- sapply(oocm[, c(11,17,23)], as.numeric) 
      # convert the values in the outcome columns into numeric values
      
      names(oocm)[11] <- "heart attack"   #renaming columns for easier identification
      names(oocm)[17] <- "heart failure"  #renaming columns for easier identification
      names(oocm)[23] <- "pneumonia"      #renaming columns for easier identification
      
      ## Check that state and outcome are valid
      
      if (!(state %in% unique(oocm$State))) stop("invalid state")
      
      outcomes <- c("heart attack", "heart failure", "pneumonia")
      if (!(outcome %in% outcomes)) stop("invalid outcome")
      
      ## Return hospital name in that state with lowest 30-day death
      oocm_st <- subset(oocm, oocm[,7] == state)
      #subset data based on "state" value
      oocm_oc <- oocm_st[ ,which(names(oocm_st) %in% c("Hospital.Name","State", outcome))]
      #subset data based on "outcome" value
      oocm_oc <- na.omit(oocm_oc)         # remove NA values
      oocm_oc <- oocm_oc[order(oocm_oc[3],oocm_oc[1]),] 
      # order data by lowest mortality rate, then alphabetical of Hospital name
      
      ## rate
      oocm_oc$rank <- NA                  # create new column
      oocm_oc$rank <- 1:nrow(oocm_oc)     # rank create the ranking based on already sorted data (by mortality rate, by name)
      return(oocm_oc$Hospital.Name[1])    # return the "top" of the list
      
      options(warn=0)                     # turning ON warning messages (because I still need them outside this script)
}


best("TX", "pneumonia")
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")
