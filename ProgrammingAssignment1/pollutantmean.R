## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".
  
## 'id' is an integer vector indicating the monitor ID numbers
## to be used
  
## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)
pollutantmean <- function(directory, pollutant, id = 1:332) {
    fileNames <- formatC(id, width=3, format="d", flag="0")
    allData = data.frame()
  
    for(fName in fileNames){
        data <- read.csv(paste(directory, paste(fName, "csv", sep="."), sep="/"))
        allData <- rbind(allData, data)                  
    }
  
    polData = allData[pollutant]
    mean(polData[!is.na(polData)])
}
