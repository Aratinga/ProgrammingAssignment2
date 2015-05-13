best <- function(state, outcome) {
state <- "TX"
outcome <- "heart attack"
	# get input file
	outcomefile <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
	## check state param against list of states
	stateslist <- unique(outcomefile[, 7])
	if (state %in% stateslist == FALSE) 
		stop("invalid state")
	## if not found then STOP with error message
	outcomelist <- c("heart attack", "heart failure", "pneumonia")
	if (outcome %in% outcomelist == FALSE) 
		stop("invalid outcome")
	## check outcome string parameter against outcomelist
	## if not found then STOP with error message
## The column to be examined depends on the outcome param
## heart attack : 11, heart failure : 17, pneumonia : 23

	if (outcome == "heart attack") 
		colim <- 11
	if (outcome == "heart failure") 
		colim <- 17
	if (outcome == "pneumonia") 
		colim <- 23
	## name of hospital is col 2
	
	statesub <- subset(outcomefile, State == state, select = c(2, colim))
	statesub[, 2] <- suppressWarnings(as.numeric(gsub("Not Available","NA",statesub[, 2],fixed=TRUE)))
	statesub <- subset(statesub, !is.na(statesub[, 2]))
	sorted <- statesub[order(statesub[, 2], statesub[, 1]), ]

	return(sorted[1, 1])
}