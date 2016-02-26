best <- function(state,outcome) {
        ## 1. Read outcome data
        mycols <- rep("NULL", 46) 
        mycols[c(2,7,11,17,23)] <- NA   ## Select useful Columns
        my_data <- read.csv("outcome-of-care-measures.csv", colClasses=mycols, nrows=999999)
        names(my_data) <- c("Hospital.Name","State","Heart Attack","Heart Failure","Pneumonia") 
        
        ## Force Factor Data to numeric (ignoring NA creation error) for columns 3:5
        
        for(i in 3:5) {
        my_data[,i]<-suppressWarnings(as.numeric(levels(my_data[[i]]))[my_data[[i]]])
        }
        
        ## 2. Check that the state and outcome are valid
        
        state_rng <- levels(my_data$State)
        
        out_rng <- c("heart attack","heart failure","pneumonia","Heart Attack","Heart Failure","Pneumonia")
        
        if(!(state %in% state_rng)) {
                stop("invalid state")
                geterrmessage()
        }
        
        if(!(outcome %in% out_rng)) {
                stop("invalid outcome")
                geterrmessage()
        }
        
        ## 3. Return hospital name in that state with lowest 30-day death rate
        
        state_data <- my_data[grep(state,my_data$State),]       ## Select State
        orderdata <- state_data[order(state_data[,outcome]),]   ## Order Hospitals
        levels(orderdata[1,1])[orderdata[1,1]]
        
        
        ## message("Success!")
        
        
}