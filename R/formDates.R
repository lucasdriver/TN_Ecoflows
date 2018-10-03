
# Function to format continuous start and end dates to line up with start and end of water year
# function argument calls for a dataframe with first column containing site_no,
# second column with "startDate", and third column with "endDate"

formDates <- function(dateDF) {
	
	for (i in 1:nrow(dateDF)){
		
		# format start dates to "yyyy-10-01"
		startDtNew <- as.POSIXlt(dateDF[i,"startDate"])
		startYr <- as.integer(format(startDtNew, "%Y"))
		startMn <- as.integer(format(startDtNew, "%m"))
		startDy <- as.integer(format(startDtNew, "%d"))
		
		if(startMn == 10 & startDy == 1L) {
			startYrNew <- startYr
		}
		
		else if(startMn < 10 ) {
			startYrNew <- startYr
		}
		
		else if(startMn > 10) {
			startYrNew <- startYr + 1L
		}
		
		else if(startMn == 10 & startDy != 1L) {
			startYrNew <- startYr + 1L
		}
		
		dateDF$startDateNew[i] <- paste0(startYrNew, "-10-01")
		rm(startDtNew, startDy, startMn, startYr, startYrNew)
		
		# format endDate to "yyyy-09-31"
		endDtNew <- as.POSIXlt(dateDF[i,"endDate"])
		endYr <- as.integer(format(endDtNew, "%Y"))
		endMn <- as.integer(format(endDtNew, "%m"))
		endDy <- as.integer(format(endDtNew, "%d"))
		
		if(endMn == 9L & endDy == 30L) {
			endYrNew <- endYr 
		} 
		
		else if(endMn > 9L) {
			endYrNew <- endYr
		}
		
		else if(endMn < 9L) {
			endYrNew <- endYr - 1L
		} 
		
		else if(endMn == 9L & endDy != 30L) {
			endYrNew <- endYr - 1L
		}
		
		dateDF$endDateNew[i] <- paste0(endYrNew, "-09-30")
		rm(endDtNew, endDy, endMn, endYr, endYrNew)
	}
	
	return(dateDF)
}


