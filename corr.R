corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        dat <- data.frame()
        FilesList <- dir(directory, full.names=TRUE)
        CompleteCases <- complete(directory)
        vect <- numeric()        
        for (i in 1:nrow(CompleteCases)) {
                if (CompleteCases[i,2] > threshold) {
                        dat <- read.csv(FilesList[i])
                        vect <- c(vect, cor(dat[,2],dat[,3], use = "pairwise.complete.obs"))
                }
        }
        vect
}
