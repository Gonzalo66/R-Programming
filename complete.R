complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        FilesList <- dir(directory, full.names=TRUE)
        CompleteCases <- data.frame()
        for (i in id) {
                Dat <- read.csv(FilesList[i])
                Nobs <- sum(complete.cases(Dat))
                CompleteCasesTemp <- cbind(i,Nobs)
                CompleteCases <- rbind(CompleteCases,CompleteCasesTemp)
        }
        colnames(CompleteCases) <- c("id","nobs")
        CompleteCases
}
