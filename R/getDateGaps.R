# The new EflowStats package will not calculate streamflow characteristics for datasets with 
# incomplete water years (i.e. missing daily values)

# The getDateGaps() function below evaluates the period of record for daily flow values for each
# site to identify continuous chunks of flow data without any gaps in the record

getDateGaps <- function(site) {
	
	newDvs <- tryCatch({
		dataRetrieval::readNWISdv(site, parameterCd = "00060", statCd = "00003", 
															startDate = "1974-10-01",endDate = "2016-09-30")
	},
	
	error = function(cond) {
		data.frame(site_no = site, comment = "NWIS retrieval failure")
	})
	
	if(nrow(newDvs) == 0) {
		newDF <- data.frame(site_no = as.character(site), comment = "out of date range",
												stringsAsFactors = FALSE)
		return(newDF)
	}
	
	else if(nrow(newDvs) == 1) {
		newDF <- data.frame(site_no = as.character(site), comment = "insufficient data",
												stringsAsFactors = FALSE)
		return(newDF)
	}
	
	else if(nrow(newDvs) > 1) {
		colnames(newDvs)[4] <- "Flow"
		siteInfo <- readNWISsite(site)
		
		if(is.na(siteInfo$drain_area_va)) {
			newDF <- data.frame(site_no = as.character(site), comment = "has no DA in site file", 
													stringsAsFactors = FALSE)
			return(newDF)
		}
		
		newDate <- data.frame(Date = seq(min(newDvs$Date), max(newDvs$Date), by = "1 days"))
		newDvs <- merge(x = newDate, y = newDvs, by = "Date", all.x = TRUE)
		refDvs <- mutate(newDvs, chunk = if_else(is.na(Flow), 0, 1))
		reflengths <- rle(refDvs$chunk)$lengths; refVals <- rle(refDvs$chunk)$values
		refDF <- data.frame(reflengths, refVals)
		refDF$totals <- cumsum(refDF[,1])
		
		if(min(refVals) == 1) {
			newDF <- data.frame(site_no = as.character(site), startDate = newDvs[1,1], 
													endDate = newDvs[nrow(newDvs),1],comment = "1", stringsAsFactors = FALSE)
			return(newDF)
		}
		
		else if(min(refVals) == 0) {
			newDF <- data.frame(site_no = as.character(site), startDate = newDvs[1,1], 
													endDate = newDvs[refDF[1,3],1],comment = as.character(refDF[1,2]), 
													stringsAsFactors = FALSE)
			
			for (i in seq(2, nrow(refDF), 1)) {
				newerDF <- data.frame(site_no = as.character(site), startDate = newDvs[refDF[(i-1),3] + 1,1], 
															endDate = newDvs[refDF[i,3],1],comment = as.character(refDF[i,2]), 
															stringsAsFactors = FALSE)
				newDF <- bind_rows(newDF, newerDF); rm(newerDF)
			}
			
			return(newDF)
		}
	}
}

