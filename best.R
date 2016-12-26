
##
# The function best takes two arguments: the 2-character abbreviated name of 
# a state(state) and an outcome(outcome). The function reads the outcome-of-
# care-measures.csv file and returns a character vector with the name of
# the hospital that has the lowest/best 30-day mortality for the specified
# outcome in that state. For example, the call best("TX", "pneumonia") 
# would return a character vector containing the name of the hospital with 
# the lowest 30-day death rate for pneumonia.
##
best <- function(state, outcome){
  
  ##
  # Using read.csv to read the csv file. The property 'colClasses = "character"
  # implies that all columns will be treated as characters or strings.
  ##
  outcome_data <- read.csv("./Data/outcome-of-care-measures.csv",
                           colClasses = "character")
  
  ##
  # data.frame is used to convert 'outcome_data' into a data frame that
  # contains only those columns that are specific to the problem being
  # solved. For example column 2 from the main data set corresponds to 
  # the hospital name, 7 is state name, 11 is the mortality rate for heart 
  # attack, 17 is the mortality rate for heart failure and 23 is the
  # is the mortality rate for pneumonia. They are given column names such 
  # as hospital, state, heart attack, etc. for easy manipulation.
  ##
  outcome_data <- data.frame(cbind(outcome_data[, 2], 
                             outcome_data[, 7], 
                             outcome_data[, 11], 
                             outcome_data[, 17], 
                             outcome_data[, 23]), stringsAsFactors = FALSE)
  colnames(outcome_data) <- c("hospital", "state", "heart attack", 
                              "heart failure", "pneumonia")
  minOutcomeValue <- NULL
  hospitalDetails <- NULL
  errorMessage <- NULL
  
  ##
  # This checks whether the state entered by the user is valid.
  ##
  if(!(state %in% outcome_data$state)){
    errorMessage = paste("Error in best (",state, " , ", outcome, ")",
                        " : invalid state", sep = "")
    return (errorMessage)
  }else{
    
    ##
    # Getting data for state requested by the user.
    ##
    state_data <- outcome_data[which(outcome_data$state == state),]
    
    ##
    # This checks whether the outcome falls under one of the three
    # categories (heart attack, heart failure, pneumonia)
    ##
    if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))){
      errorMessage = paste("Error in best (",state, " , ", outcome, ")",
                           " : invalid outcome", sep = "")
      return (errorMessage)
    }else{
      
      ##
      # Converting the outcome value into numeric since this was 
      # converted into character before.
      ##
      state_data[, eval(outcome)] <- suppressWarnings(as.numeric(state_data[, eval(outcome)]))
      
      ##
      # Checking for the hospital that has the minimum value for the 
      # outcome variable.
      ##
      minOutcomeValue <- min(state_data[outcome],na.rm = TRUE)
      hospitalDetails <- state_data[which(state_data[outcome] == minOutcomeValue), ]
      
      ##
      # In case of a tie (two or more hospitals that have the same value
      # for a particular outcome), the hospital names are alphabetically ordered
      # and the first hospital will be returned.
      ##
      if(nrow(hospitalDetails) > 1){
        return (min(hospitalDetails$hospital, na.rm = TRUE)) 
      }
      return (hospitalDetails$hospital)
    }
  }
}