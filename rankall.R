rankall <- function(outcome, num = "best") {
        
        ## Read outcome data
        
        table <- read.csv("outcome-of-care-measures.csv", colClasses = "character",header=TRUE,as.is=TRUE)
        
        #Check that outcome and num are valid
        outcome <- tolower(outcome)
        out <- c("heart attack","heart failure","pneumonia")
        if(match(outcome,out, nomatch = -1) == -1 ) stop("Invalid outcome.")
        
        if (!is.numeric(num)){
               if (match(num,c("best","worst"), nomatch = -1) == -1) stop("Invalid number.")
        }
        
        ## For each state, find the hospital of the given rank
        
        
        if (outcome == "heart attack"){
                suppressWarnings(table[,11]<-as.numeric(table[,11]))
                tab1 <- table[,c(2,7,11),]
                tab2 <- tab1[order(tab1$State,tab1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, tab1$Hospital.Name),] #
                
        }
        else {
                if (outcome == "heart failure"){
                        suppressWarnings(table[,17]<-as.numeric(table[,17]))
                        tab1 <- table[,c(2,7,17),]
                        tab2 <- tab1[order(tab1$State,tab1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, tab1$Hospital.Name),]
                }
                else {
                        suppressWarnings(table[,23]<-as.numeric(table[,23]))
                        tab1 <- table[,c(2,7,23),]
                        tab2 <- tab1[order(tab1$State,tab1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, tab1$Hospital.Name),]
                }
        }
        tab2 <- na.omit(tab2)
        tab3 <- split(tab2,tab2$State)        
        
        ## Return a data frame with the hospital names and the (abbreviated) state name
        state <- lapply(tab3, function(tab){
                tab_temp <- as.data.frame(tab)
                tab_temp[1,][,2]})
        
        hospital <- lapply(tab3, function(tab){
                tab_temp <- as.data.frame(tab)
                if (num == "best") num <- 1
                if (num == "worst") num <- nrow(tab_temp)
                tab_temp[num,][,1]})
        
        result<- as.data.frame(cbind(hospital,state))
        
}