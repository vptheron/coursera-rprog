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
complete <- function(directory, id = 1:332) {

   fileNames <- formatC(id, width=3, format="d", flag="0")
   result = data.frame(id=numeric(),
                       nobs=numeric())
  
   for(fName in fileNames){
        data <- read.csv(paste(directory, paste(fName, "csv", sep="."), sep="/"))
        filtered = data[complete.cases(data), "ID"]
        localData <- data.frame(id = c(max(filtered)),
                                nobs = length(filtered))
        result <- rbind(result, localData)                  
    }
  
    result
}
