best <- function(state, outcome) {
        
        vect <- vector()
        min_val <- 0
        
        ## Read outcome data
        table <- read.csv("outcome-of-care-measures.csv", colClasses = "character",header=TRUE,as.is=TRUE)
        
        ## Check that state and outcome are valid
        sta <- c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","GU","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","PR","RI","SC","SD","TN","TX","UT","VA","WA","WV","WI")
        if(match(state,sta, nomatch = -1) == -1 ) stop("Invalid state.")
  
        out <- c("heart attack","heart failure","pneumonia")
        if(match(outcome,out, nomatch = -1) == -1 ) stop("Invalid outcome.")
        
        ## Return hospital name in that state with lowest 30-day death rate
        tab1 <- subset(table, State == state)
        if (outcome == "heart attack"){
                tab1[,11]<-as.numeric(tab1[,11])
                min_val <- min(tab1[,11], na.rm = TRUE)
                tab2 <- subset(tab1,tab1[,11]==min_val)
                vect <- tab2[,2,]
                }
        else {
                if (outcome == "heart failure"){
                        tab1[,17]<-as.numeric(tab1[,17])
                        min_val<-min(tab1[,17], na.rm = TRUE)
                        tab2 <- subset(tab1,tab1[,17]==min_val)
                        vect <- tab2[,2,]
                        }
                else {
                        tab1[,23]<-as.numeric(tab1[,23])
                        min_val<-min(tab1[,23], na.rm = TRUE)
                        tab2 <- subset(tab1,tab1[,23]==min_val)
                        vect <- tab2[,2,]   
                }
        }
        vect<- sort(vect)
        return(vect[1])
}