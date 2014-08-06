
rankall <- function(outcome, num = "best"){

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

	# For each state, find the hospital of the given rank
	nameStateAndRate <- data[, c(2,7,outcomeIndex)]
	nameStateAndRate[,3] <- as.numeric(as.character(nameStateAndRate[,3]))
	
	dataF = data.frame(hospital = character(0), state = character(0))
	for(state in levels(nameStateAndRate$State)){
		inState <- nameStateAndRate[nameStateAndRate$State == state, c(1, 3)]
		inState <- inState[!is.na(inState[,2]),]
		sortedInState <- inState[ order(inState[,2], inState[,1]), ]

		result <-
			if(num == "best")
				sortedInState[1,1]
			else if(num == "worst")
				sortedInState[nrow(sortedInState),1]
			else if(as.numeric(num) <= nrow(sortedInState))
				sortedInState[as.numeric(num),1]
			else
				NA
		dataF <- rbind(dataF, data.frame(hospital = as.vector(result), state = state))
	}

	dataF
}
