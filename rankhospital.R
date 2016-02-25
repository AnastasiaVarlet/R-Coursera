rankhospital <- function(state, outcome, num = "best") {

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
    data.state <- na.omit(data.state)

    ## Translate the value of num
    if (num == "best"){
        my_num <- 1
    }
    else if (num == "worst"){
        my_num <- nrow(data.state)
    }
    else if (is.numeric(num)){
        if (num<0 || num>nrow(data.state)){
            return(NA)
        }
        else my_num <- num
    }
    else stop('invalid num')
    
    ## Order the hospitals in that state for the given outcome my_ind
    ranked.data.state <- data.state[order(data.state[, my_ind],data.state$Hospital.Name, na.last=NA), ]
    
    ## Return hospital name in that state with the given rank 30-day death rate
    return(ranked.data.state[my_num, ]$Hospital.Name)
}