# --------------------------------------------------------------------------------------------------------------------
# Coursera: R programming → Week 4
# Peer-graded Assignment: Programming Assignment 3: Hospital Quality (rankhospital.R)
# --------------------------------------------------------------------------------------------------------------------

# Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
# state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
# 
# The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
# of the hospital that has the ranking specified by the num argument. For example, the call
# rankhospital("MD", "heart failure", 5)
# would return a character vector containing the name of the hospital with the 5th lowest 30-day death rate
# for heart failure. The num argument can take values “best”, “worst”, or an integer indicating the ranking
# (smaller numbers are better). If the number given by num is larger than the number of hospitals in that
# state, then the function should return NA. Hospitals that do not have data on a particular outcome should
# be excluded from the set of hospitals when deciding the rankings.
# 
# Handling ties. It may occur that multiple hospitals have the same 30-day mortality rate for a given cause
# of death. In those cases ties should be broken by using the hospital name. For example, in Texas (“TX”),
# the hospitals with lowest 30-day mortality rate for heart failure are shown here.



setwd("C:/Users/Gyorgy Szelezsan/Documents/GitHub/ProgrammingAssignment3")

rankhospital <- function(state, outcome, num = "best") {
      
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
      
      ## Best = 1, Worst = highest number, else rank = rank
      if (num == "best") {
            num <- 1
      } else if (num == "worst"){
            num <- nrow(oocm_oc)
      } else {
            num <- num
      }
      ## Return hospital name in that state with the given rank      
     return(oocm_oc$Hospital.Name[num])    # return the "num" value of the list
      options(warn=0)                # turning ON warning messages (because I still need them outside this script)
}

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)

rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
