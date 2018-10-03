
####################
# function to calculate 7Q10 values 
get7Q10 <- function (siteIDs) {
	
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



