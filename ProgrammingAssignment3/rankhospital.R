
rankhospital <- function(state, outcome, num = "best"){
	
	# Read outcome data
	data <- read.csv("outcome-of-care-measures.csv")

	# Check that state and outcome are valid
	if( !(state %in% levels(data[,7])) ){
		stop("invalid state")
	}

        outcomeIndex <- switch(outcome, `heart attack` = 11, `heart failure` = 17, `pneumonia` = 23)
	if( is.null(outcomeIndex ) ){
		stop("invalid outcome")
	}

	# Return hospital name in the state with the given rank 30-day death rate
	hospitalNameAndRate <- data[data$State == state, c(2, outcomeIndex)]
	hospitalNameAndRate[,2] <- as.numeric(as.character(hospitalNameAndRate[,2]))
	hospitalNameAndRate <- hospitalNameAndRate[!is.na(hospitalNameAndRate[,2]),]
	
	sortedNameAndRate <- hospitalNameAndRate[ order(hospitalNameAndRate[,2], hospitalNameAndRate[,1]), ]

	result <-
	if(num == "best")
		sortedNameAndRate[1,1]
	else if(num == "worst")
		sortedNameAndRate[nrow(sortedNameAndRate),1]
	else if(as.numeric(num) <= nrow(sortedNameAndRate))
		sortedNameAndRate[as.numeric(num),1]
	else
		NA

	as.vector(result)
}
