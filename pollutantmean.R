pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
        files_list <- list.files(directory, full.names=TRUE)   #creates a list of files
        dat <- data.frame()                             #creates an empty data frame
        for (i in id) {                                 
                dat <- rbind(dat, read.csv(files_list[i])) #loops through the files, rbinding them together
        }
        dat_subset <- dat[, pollutant,]         #subsets the rows that match the 'pollutant' argument
        mean(dat_subset, na.rm=TRUE)           #identifies the mean of the subset while stripping out the NAs                                        
}
