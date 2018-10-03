### Calculate weighted means for each SFC

# For sites with multiple date chunks of data, the following function calculates weighted means 
# (by length of record in years) for each SFC for each site

# This function is designed to run on a single site, and also designed to run in parallel for 
# mulitple sites


getWeightedMeans <- function(siteID, EflowDF){
	
	newDF <- EflowDF[0,]
	subDF <- dplyr::filter(EflowDF, site_no == siteID)
	
	if(length(unique(subDF$start_date)) == 1) {
		newDF <- bind_rows(newDF, subDF)
	}
	
	else if(length(unique(subDF$start_date)) > 1) {
		
		for (j in seq(1, length(flowNames), 1)) {
			subDFnew <- filter(subDF, comment == flowNames[j])
			
			subDFnew$nYears <- as.numeric(stringr::str_sub(subDFnew$end_date, 1, 4)) - 
				as.numeric(stringr::str_sub(subDFnew$start_date, 1, 4))
			
			subDFwtd <- subDFnew %>%
				summarize_at(c(4:181), funs(weighted.mean(., nYears, na.rm = TRUE))) %>%
				summarize_at(c(1:178), funs(round(., digits = 3)))
			
			info <- data.frame(site_no = siteID, start_date = subDFnew[1,2],
												 end_date = subDFnew[nrow(subDFnew),3])
			
			subDFwtd <- dplyr::bind_cols(info, subDFwtd)
			
			subDFwtd$comment <- paste0(flowNames[j], "_W")
			
			newDF <- bind_rows(newDF, subDFwtd); rm(subDFnew, subDFwtd, info)
			
		}
	}
	return(newDF)
}
