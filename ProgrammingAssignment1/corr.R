## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlations
corr <- function(directory, threshold = 0) {
    completePerMonitor = complete(directory)
    aboveThreshold = completePerMonitor[completePerMonitor$nobs > threshold,]
    
    fileNames <- formatC(aboveThreshold$id, width=3, format="d", flag="0")
    allData = numeric()

    for(fName in fileNames){
        if(fName != ""){
        data <- read.csv(paste(directory, paste(fName, "csv", sep="."), sep="/"))
        filtered = data[complete.cases(data),]
        allData <- c(allData, cor(filtered$sulfate, filtered$nitrate))                  
    }
}

    allData
}
