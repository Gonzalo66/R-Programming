rankhospital <- function(state, outcome, num = "best") {
        vect <- vector()
        num_ord <- 0
        
        table <- read.csv("outcome-of-care-measures.csv", colClasses = "character",header=TRUE,as.is=TRUE)
        
        ## Check that state and outcome are valid
        sta <- c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","GU","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","PR","RI","SC","SD","TN","TX","UT","VA","WA","WV","WI")
        if(match(state,sta, nomatch = -1) == -1 ) stop("Invalid state.")
        
        out <- c("heart attack","heart failure","pneumonia")
        if(match(outcome,out, nomatch = -1) == -1 ) stop("Invalid outcome.")
        
        ## Return hospital name in that state with the given rank 30-day death rate
        tab1 <- subset(table, State == state)
        if (outcome == "heart attack"){
                tab1[,11]<-as.numeric(tab1[,11])
                tab2 <- tab1[,c(2,11),]
                tab3 <- tab2[order(tab2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, tab2$Hospital.Name),] #
                
        }
        else {
                if (outcome == "heart failure"){
                        tab1[,17]<-as.numeric(tab1[,17])
                        tab2 <- tab1[,c(2,17),]
                        tab3 <- tab2[order(tab2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, tab2$Hospital.Name),]
                }
                else {
                        tab1[,23]<-as.numeric(tab1[,23])
                        tab2 <- tab1[,c(2,23),]
                        tab3 <- tab2[order(tab2$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, tab2$Hospital.Name),]
                }
        
        }
        tab3 <- na.omit(tab3)
        num_dat <- length(tab3$Hospital.Name)
        new_col <- seq(1:num_dat)
        tab3$Rank <- new_col
        
        if(num == "best"){
                num_ord <- 1
        }
        
        if(num == "worst"){
                num_ord <- num_dat
        }
        
        if(is.numeric(num)){
                if (num > num_dat) {
                        vect <- NA
                        return(vect)
                }
                else{
                       num_ord <- num 
                }
        }
        tab4 <- subset(tab3, tab3$Rank == num_ord)
        vect <- tab4[,1]
        return(vect)
}