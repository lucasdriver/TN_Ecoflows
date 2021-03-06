
# function to compute the moving average 
# of the specified series of daily values (DVs) by the specified 
# number of days (numDays)
# From BBreaker

moveAve <- function(DVs, numDays) {
		stats::filter(DVs,rep(1/numDays,numDays), sides=2)
}

# 7Q10 statistics
# From BBreaker
sevQten <- function(date, flow, yearType = NULL) {
	
	if(is.null(yearType)) {
		stop("must enter watYear, calYear, or climYear")
	}
	
	else if(length(date) != length(flow)) {
		stop("length of dates and flows does not match")
	}
	
	newDF <- data.frame(date = date, flow = flow)
	
	if(yearType == "calYear") {
		newDFAve <- newDF %>%
			mutate(yr = format(date, "%Y"),
						 mn = format(date, "%m"),
						 dvMove = moveAve(flow, 7)) %>%
			na.omit() %>%
			group_by(yr) %>%
			summarize(yearMin = min(dvMove)) %>%
			ungroup() %>%
			data.frame()
	}
	
	else if(yearType == "watYear") {
		newDFAve <- newDF %>%
			mutate(yr = format(date, "%Y"),
						 mn = format(date, "%m"),
						 dvMove = moveAve(flow, 7)) %>%
			mutate(watYr = as.character(as.numeric(yr) + if_else(as.numeric(mn) < 10, 0, 1))) %>%
			na.omit() %>%
			group_by(watYr) %>%
			summarize(yearMin = min(dvMove)) %>%
			ungroup() %>%
			data.frame()
	}
	
	else if(yearType == "climYear") {
		newDFAve <- newDF %>%
			mutate(yr = format(date, "%Y"),
						 mn = format(date, "%m"),
						 dvMove = moveAve(flow, 7)) %>%
			mutate(climYr = as.character(as.numeric(yr) + if_else(as.numeric(mn) < 4, 0, 1))) %>%
			na.omit() %>%
			group_by(climYr) %>%
			summarize(yearMin = min(dvMove)) %>%
			ungroup() %>%
			data.frame()
	}
	
	if(any(newDFAve$yearMin == 0)) {
		val <- 0
	}
	
	else if(any(newDFAve$yearMin != 0)) {
		compLmoms <- tryCatch({
			lmomco::lmoms(log10(newDFAve$yearMin))
		},
		error = function(cond) {
			NA
		})
		
		distPar <- tryCatch({
			lmomco::lmom2par(compLmoms, type = "pe3")
		},
		error = function(cond) {
			NA
		})
		
		val <- tryCatch({
			signif(10^(lmomco::par2qua(f = 0.1, para = distPar)), 3)
		},
		error = function(cond) {
			NA
		})
	}
	return(val)
}


####################
# function to calculate 7Q10 values 
cal7Q10 <- function (siteIDs) {
	
	sevQ10_vals <- data.frame()
	
	for (i in 1:length(siteIDs)) {
		
		dvs <- readNWISdv(siteNumbers = siteIDs[i], parameterCd = "00060", statCd = "00003")
		
		dvs <- dvs[,c(3:4)]
	
		colnames(dvs) <- c("Date", "Flow")
		
		newDF <- data.frame(site_no = as.character(siteIDs[i]), 
												startDate = dvs$Date[1], 
												endDate = dvs$Date[nrow(dvs)],
												val = sevQten(dvs$Date, dvs$Flow, yearType = "climYear"))
		
		sevQ10_vals <- rbind(sevQ10_vals, newDF); rm(newDF)
		
	}
	
	return(sevQ10_vals)

}



