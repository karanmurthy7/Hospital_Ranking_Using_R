
##
# rankhospital takes three arguments: the 2-character abbreviated name of 
# a state(state), an outcome(outcome), and the ranking of the hospital 
# for that particular outcome(num). The function reads the outcome-of-
# care-measures.csv file and returns a character vector with the name of
# the hospital that has the ranking specified in the num argument. For 
# example, the call rankhospital("MD", "heart attack", 5) would return 
# a character vector containing the name of the hospital with the 5th
# lowest 30-day death rate for heart attack.
##
rankhospital <- function(state, outcome, num="best"){
  
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
    errorMessage = paste("Error in rankhospital (",state, " , ", outcome, ")",
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
      errorMessage = paste("Error in rankhospital (",state, " , ", outcome, ")",
                           " : invalid outcome", sep = "")
      return (errorMessage)
      
    }else{
      
      ##
      # This returns the best or worst hospital for the state
      # and outcome requested by the user. Additionally, the user can
      # also search for a hospital with a specific rank in a particular
      # state. For example, find the hospital that ranks second in WA 
      # in treating patients for pnemonia.
      ##
      if(is.numeric(num)){
        
        ##
        # Converting the outcome value into numeric since this was 
        # converted into character before.
        ##
        state_data[, eval(outcome)] <- suppressWarnings(as.numeric(state_data[, eval(outcome)]))
        
        ##
        # Orders by outcome value (the smaller outcome
        # comes before the larger outcome value. For example, 9.8 will
        # come before 10. Additionally in case there is a tie, it will
        # further order alphabetically by the name of the hospital. 
        # na.last = TRUE implies that lapply will put group all 
        # 'Not Available' data at the end.s
        ##
        state_data <- state_data[order(state_data[outcome], state_data["hospital"]
                                       , na.last = TRUE), ]
        return(state_data[,"hospital"][num])
      }else if(num == "best"){
        return (best(state, outcome))
      }else if(num == "worst"){
        state_data[, eval(outcome)] <- suppressWarnings(as.numeric(state_data[, eval(outcome)]))
        state_data <- state_data[order(state_data[outcome], state_data["hospital"]
                                       , na.last = TRUE, decreasing = TRUE), ]
        return(state_data[,"hospital"][1])
      }else{
          stop("invalid outcome")
      }
    }
  }
  
}