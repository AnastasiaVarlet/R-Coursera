best <- function(state, outcome) {
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = c("NA", "Not Available"))
    
    ## Check that state and outcome are valid
    ## Check state
    if (!any(state == data$State)) {
        stop('invalid state')
    }
    
    ## Check outcome
    if (outcome == 'heart attack') {
        my_ind <- 11
    }
    else if (outcome == 'heart failure'){
        my_ind <- 17
    }
    else if (outcome == 'pneumonia'){
        my_ind <- 23
    }
    else stop('invalid outcome')
    
    ## Coerce data + convert to numeric
    data.state <- data[data$State == state, ]
    data.state[, my_ind] <- as.numeric(x = data.state[, my_ind])
    
    ## Find hospital nameSSS in that state with lowest 30-day death rate
    bestHosp <- data.state[ (data.state[, my_ind] == min(data.state[, my_ind],na.rm = TRUE) ), ]$Hospital.Name
    
    ## Handling ties:
    ## If there is a tie for the best hospital for a given outcome, then the hospital names should
    ## be sorted in alphabetical order and the first hospital in that set should be chosen    
    the_best <- sort(bestHosp)[1]
    
    ## Return hospital name in that state with lowest 30-day death rate
    return(the_best)
}