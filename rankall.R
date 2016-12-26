##
# The function rankall takes two arguments: an outcome name (outcome) and a 
# hospital rank (num). The function will return a two-column data frame
# containing the hospital in each state that has the ranking specified in
# num. For example, the function call rankall("heart attack", "best") would
# return a data frame containing the names of hospitals that are the best
# in their respective states for 30-day heart attack death rates.
##

rankall <- function(outcome, num = "best"){
  
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
  
  ##
  # This checks whether the outcome falls under one of the three
  # categories (heart attack, heart failure, pneumonia)
  ##
  if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))){
    stop("invalid outcome")
  }else{
    ##
    # Converting the outcome value into numeric since this was 
    # converted into character before.
    ##
    outcome_data[, eval(outcome)] <- suppressWarnings(as.numeric(outcome_data[, eval(outcome)]))
    
    ##
    # This splits the data by state.
    ##
    split_by_state <- split(outcome_data, outcome_data$state, drop = FALSE)
    output <- NULL
    
    ##
    # The user can search for the best or worst hospital per state
    # corresponding to a specific outcome. Additionally, the user can
    # also search for a hospital with a specific rank. For example, 
    # find the hospital that ranks second per state in treating
    # patients for pnemonia.
    ##
    
    if(is.numeric(num)){
      ##
      # lapply first orders by outcome value (the smaller outcome
      # comes before the larger outcome value. For example, 9.8 will
      # come before 10. Additionally in case there is a tie, it will
      # further order alphabetically by the name of the hospital.
      # na.last = TRUE implies that lapply will put group all 
      # 'Not Available' data at the end. lapply will compute the logic
      # mentioned above for every state and store the ordered list  
      # of hospitals in the 'best_hospitals_data' data frame.
      # Then it will use combine in R (c()) to combine the rows 
      # corrsponding to num value and return it to the output variable.
      # The output variable will contain one hospital per state 
      # that will be the best or worst hospital corresponding to an 
      # outcome.
      ##
      output <- lapply(split_by_state, function(x) {
        best_hospitals_data <- x[order(x[outcome], x["hospital"], na.last = TRUE), ]
        c(best_hospitals_data[num,"hospital"], best_hospitals_data[num,"state"])
      })
      
    }else if(num == "best"){
      output <- lapply(split_by_state, function(x) {
        best_hospitals_data <- x[order(x[outcome], x["hospital"], na.last = TRUE), ]
        c(best_hospitals_data[1,"hospital"], best_hospitals_data[1,"state"])
      })
      
    }else if(num == "worst"){
      output <- lapply(split_by_state, function(x) {
        worst_hospitals_data <- x[order(x[outcome], x["hospital"], na.last = TRUE,
                                        decreasing = TRUE), ]
        c(worst_hospitals_data[1,"hospital"], worst_hospitals_data[1,"state"])
      })
      
    }else{
      stop("invalid rank")
    }
    
    ##
    # do.call() to merge the output that was intially
    # split by state.
    ##
    output <- do.call(rbind, output)
    
    ##
    # as.data.frame to create a data-frame using output variable.
    ##
    output <- as.data.frame(output, stringsAsFactors = FALSE)
    colnames(output) <- c("hospital", "state")
    return(output)
  }
  
}
