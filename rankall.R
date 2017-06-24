# --------------------------------------------------------------------------------------------------------------------
# Coursera: R programming → Week 4
# Peer-graded Assignment: Programming Assignment 3: Hospital Quality (rankall.R)
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

rankall <- function(outcome, num = "best") {
      require(dplyr)
      options(warn=-1)                    # turning OFF warning messages (because they annoy me)
      
      ## Read outcome data
      oocm <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      
      outcomes <- c("heart attack", "heart failure", "pneumonia")
      if (!(outcome %in% outcomes)) stop("invalid outcome")
      
      #simplify the data table
      
      df <- oocm[c(2,7,
                   if (
                         outcome == "heart attack"
                   ){11} else if (
                         outcome == "heart failure"
                   ) {17} else if (
                         outcome == "pneumonia"
                   ) {23}
                         )]
      names(df)[3] <- "mortality"   #renaming columns for easier identification

      # convert the values in the outcome columns into numeric values
      df[,3] <- sapply(df[,3], as.numeric)
      
      # rank hospitals by state / mortality rate
      
      df <- df %>% 
            arrange(State, mortality, Hospital.Name) %>%
            group_by(State) %>%
            mutate(ranking=row_number())
      
      # Create the return table's structure - we need data for each state
      df_state <- df$State %>%
            unique %>%
            as.data.frame
      
      names(df_state)[1] <- "State"   #renaming columns for easier identification
      
      # Return df2 table, based on num's value
      
      if(num == "best") {
            df2 <- top_n(df ,1 ,-ranking) # if BEST (since top_n returns the max value, have to reverse the ranking)     
            df2 <- df2[c(1,2)]      
      } else if (num == "worst"){
            df <- na.omit(df) # has to remove NA, otherwise for WORST we'll mostly have NA rated hospitals
            df2 <- top_n(df ,1 ,ranking) # if WORST      
            df2 <- df2[c(1,2)]
      } else {
            df_f <- filter(df ,ranking == num) 
            df2 <- merge(df_state, df_f, all.x = TRUE)
            df2 <- df2[c(2,1)]      
      }
      options(warn=0)                # turning ON warning messages (because I still need them outside this script)      

      df2 # return the table I need
}

head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
