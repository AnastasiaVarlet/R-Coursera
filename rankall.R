rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = c("NA", "Not Available"))
    
    ## Check that outcome is valid
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
    
    ## Identifying the different states and ranking them with alphabetic order
    listofstates <- sort(unique(data$State))
    
    ## Create the list to store the result
    result <- list()
    
    ## Coerce data + convert to numeric
    for (my_state in listofstates){
        data.state <- data[data$State == my_state, ]
        data.state[, my_ind] <- as.numeric(x = data.state[, my_ind])

        
        ## Order the hospitals in that state for the given outcome my_ind
        if (num == "worst") {
            ranked.data.state <- data.state[order(-data.state[, my_ind], data.state$Hospital.Name, na.last=NA), ]
        } 
        else {
            ranked.data.state <- data.state[order(data.state[, my_ind], data.state$Hospital.Name, na.last=NA), ]
        }
        
        ## Check & Translate the value of num into a number
        if (num == "best"){
            my_num <- 1
        }
        else if (num == "worst"){
            my_num <- 1
        }
        else if (is.numeric(num)){
            if (num<0 || num>nrow(ranked.data.state)){
                result <- rbind(result, list("<NA>", my_state))
                next
            }
            else my_num <- num
        }
        else stop('invalid num')
        
        ## Return hospital name in that state with the given rank 30-day death rate
        result.state <- ranked.data.state[my_num, ]$Hospital.Name
        result <- rbind(result, list(result.state,my_state))
    }
    
    result <- as.data.frame(result)
    colnames(result) <- c('hospital', 'state')
    result  
}