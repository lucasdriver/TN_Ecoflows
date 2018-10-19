
### Calculate EflowStats - Constant cfs Rate using incremental CFS (NOT EXCEEDENCE/QUANTILES)
# The following function runs the new EflowStats package and calculates Stream Flow Characterists for each Constant cfs rate withdrawal scenario
# 
# For each date chunk of continuous data for each site:
# (1) pulls daily values for each site
# (2) calculates peakflow data and floodThresholds
# (3) subtracts constant cfs to generate withdrawal scenario and SETS MINIMUM FLOW VALUE XX? cfs
# (4) calculates SFCs for each constant cfs withdrawal scenario
# 
# Notes: 
#	(1) the new EflowStats package does NOT include built-in functions for calculating e85
# (2) "pref = median" should be included as an argument for calculations for "sep-med" (ma20), but in order to do 
# this I will have to do a work-around for that particular 'stat' group function. 
# HOWEVER, neither e85 nor Sep-med are used in any of the ELFs... so these issues are not critical. 
# (3) argument 'mflNorm' is a scaling factor used to normalize minimum flow level by drainage area
# for example: mfl = 0.05 signifies normalizing the MFL to 5% of the Drainage Area for each site. DA = 100; mflNorm = 0.05; MFL = 5cfs 
# If mflNorm = NULL, no MFL will be applied and flows will be allowed to withdrawal to 0 cfs. 


getFlowStatsConst <- function(siteID, startDtNew, endDtNew, mflNorm = NULL) {
	dvs <- tryCatch({
		readNWISdv(siteID, parameterCd = "00060", startDate = startDtNew, endDate = endDtNew, statCd = "00003")
	},
	
	error = function(cond){
		message("NWIS retrieval failure")
		data.frame(site_no = as.character(siteID), start_date = startDtNew, end_date = endDtNew, 
							 comment = "NWIS retrieval failure", stringsAsFactors = FALSE)
	})
	
	if(nrow(dvs) == 0) {
		EflowDat <- data.frame(site_no = siteID, start_date = startDtNew, end_date = endDtNew, 
													 comment = "NWIS failure", stringsAsFactors = F)
		return(EflowDat)
	}
	
	else if(nrow(dvs) > 0) {
		siteInfo <- readNWISsite(siteID)
		
		if(is.na(siteInfo$drain_area_va)) {
			EflowDat <- data.frame(site_no = as.character(siteID), start_date = startDtNew, 
														 end_date = endDtNew, comment = "has no DA in site file", 
														 stringsAsFactors = FALSE)
			return(EflowDat)
		}
		
		else if(!is.na(siteInfo$drain_area_va)) {
			pkFile <- readNWISpeak(siteID, convertType = FALSE)
			pkFile$peak_dt <- dplyr::if_else(stringr::str_sub(pkFile$peak_dt, start = 9, end = 10) == "00",
																			 paste0(stringr::str_sub(pkFile$peak_dt, 1, 8), "01"), pkFile$peak_dt)
			
			pkFile$peak_dt <- dplyr::if_else(stringr::str_sub(pkFile$peak_dt, start = 6, end = 7) == "00",
																			 paste0(stringr::str_sub(pkFile$peak_dt, 1, 5), "04-01"), pkFile$peak_dt)
			
			pkFile$peak_dt <- as.Date(pkFile$peak_dt, format = "%Y-%m-%d")
			pkFile$peak_va <- as.numeric(pkFile$peak_va)
			pkFile <- pkFile[!is.na(pkFile$peak_va),]
			pkFile <- dplyr::filter(pkFile, !peak_va == 0)
			
			if(nrow(pkFile) < 2) {
				EflowDat <- data.frame(site_no = as.character(siteID),
															 comment = "less than two peak flows", stringsAsFactors = FALSE)
				return(EflowDat)
			}
			
			else if(nrow(pkFile) >= 2) {
				drainageArea <- siteInfo$drain_area_va
				dvs <- dvs[,c(3,4)]
				colnames(dvs) <- c("Date", "Flow")
				dailyQClean <- validate_data(dvs[c("Date", "Flow")], yearType = "water")
				floodThresh <- get_peakThreshold(dailyQClean[c("date","discharge")], pkFile[c("peak_dt","peak_va")])
				
				if (is.nan(floodThresh) == TRUE) {
					floodThresh <- NULL
				}
				
				calc_allHITOut <- tryCatch({
					calc_allHIT(dailyQClean, drainArea = drainageArea, floodThreshold = floodThresh)
				},
				
				error = function(cond) {
					data.frame(comment = "EflowStats error", stringsAsFactors = FALSE)
				})
				
				magnifStatsOut <- tryCatch({
					calc_magnifSeven(dailyQClean, yearType = "water", digits = 3)
				},
				
				error = function(cond) {
					data.frame(comment = "EflowStats error", stringsAsFactors = FALSE)
				})
				
				info <- data.frame(site_no = siteID, start_date = startDtNew, end_date = endDtNew,
													 stringsAsFactors = FALSE)
				
				if(nrow(calc_allHITOut) == 1 & nrow(magnifStatsOut) == 1) {
					EflowDat <- info
					EflowDat$comment <- "EflowStats failure"
					
					return(EflowDat)
				}
				
				else if(nrow(calc_allHITOut) == 1 & nrow(magnifStatsOut) > 1) {
					magnifStatsOutDF <- t(magnifStatsOut$statistic)
					magnifStatsOutDF <- data.frame(magnifStatsOutDF)
					names(magnifStatsOutDF) <- magnifStatsOut$indice
					calc_allHITOutDF <- calc_allHITOut
				}
				
				else if(nrow(calc_allHITOut) > 1 & nrow(magnifStatsOut) == 1) {
					magnifStatsOutDF <- magnifStatsOut
					calc_allHITOutDF <- t(calc_allHITOut$statistic)
					calc_allHITOutDF <- data.frame(calc_allHITOutDF)
					names(calc_allHITOutDF) <- calc_allHITOut$indice
				}
				
				else if(nrow(calc_allHITOut) > 1 & nrow(magnifStatsOut) > 1) {
					magnifStatsOutDF <- t(magnifStatsOut$statistic)
					magnifStatsOutDF <- data.frame(magnifStatsOutDF)
					names(magnifStatsOutDF) <- magnifStatsOut$indice
					calc_allHITOutDF <- t(calc_allHITOut$statistic)
					calc_allHITOutDF <- data.frame(calc_allHITOutDF)
					names(calc_allHITOutDF) <- calc_allHITOut$indice
					calc_allHITOutDF$comment = "raw"
				}
				
				EflowDat <- dplyr::bind_cols(info, magnifStatsOutDF, calc_allHITOutDF)
				
				##################################################
				# calculate SFCs for each withdrawal scenario
				
				# Extract Mean (annual) Flow
				maf <- EflowDat$ma1
				
				withCFS_vec <- c(seq(0.1, .9,.1), seq(1,9,1), seq(10,50,1))
				
				for(i in seq(1, length(withCFS_vec), 1)) {
					
					if (is.null(mflNorm) == TRUE) {
						
						mfl <- 0
					} 
					
					else { 
						
						mfl <- maf * mflNorm
						
					}
					
					newDvs <- data.frame(Date = dvs[1], Flow = NA)
					
					for (j in 1:nrow(newDvs)) {
						
						if(dvs[j,2] - withCFS_vec[i] >= mfl) {
							
							newDvs[j,2] <- dvs[j,2] - withCFS_vec[i]
							
						} 
						
						else if(dvs[j,2] >= mfl & dvs[j,2] - withCFS_vec[i] < mfl) {
							
							newDvs[j,2] <- mfl
						}
						
						else if (dvs[j,2] < mfl) {
							
							newDvs[j,2] <- dvs[j,2]
						}
						
					}
					
					dailyQClean <- validate_data(newDvs[c("Date", "Flow")], yearType = "water")
					
					calc_allHITOut <- tryCatch({
						calc_allHIT(dailyQClean, drainArea = drainageArea, floodThreshold = floodThresh)
					},
					
					error = function(cond) {
						data.frame(comment = as.character(withCFS_vec[i]), stringsAsFactors = FALSE)
					})
					
					magnifStatsOut <- tryCatch({
						calc_magnifSeven(dailyQClean, yearType = "water", digits = 3)
					},
					
					error = function(cond) {
						data.frame(comment = as.character(withCFS_vec[i]), stringsAsFactors = FALSE)
					})
					
					info <- data.frame(site_no = siteID, start_date = startDtNew, end_date = endDtNew,
														 stringsAsFactors = FALSE)
					
					if(nrow(calc_allHITOut) == 1 & nrow(magnifStatsOut) == 1) {
						newEflowDat <- info
						newEflowDat$comment <- "EflowStats failure"
						
						return(newEflowDat)
					}
					
					else if(nrow(calc_allHITOut) == 1 & nrow(magnifStatsOut) > 1) {
						magnifStatsOutDF <- t(magnifStatsOut$statistic)
						magnifStatsOutDF <- data.frame(magnifStatsOutDF)
						names(magnifStatsOutDF) <- magnifStatsOut$indice
						calc_allHITOutDF <- calc_allHITOut
					}
					
					else if(nrow(calc_allHITOut) > 1 & nrow(magnifStatsOut) == 1) {
						magnifStatsOutDF <- magnifStatsOut
						calc_allHITOutDF <- t(calc_allHITOut$statistic)
						calc_allHITOutDF <- data.frame(calc_allHITOutDF)
						names(calc_allHITOutDF) <- calc_allHITOut$indice
					}
					
					else if(nrow(calc_allHITOut) > 1 & nrow(magnifStatsOut) > 1) {
						magnifStatsOutDF <- t(magnifStatsOut$statistic)
						magnifStatsOutDF <- data.frame(magnifStatsOutDF)
						names(magnifStatsOutDF) <- magnifStatsOut$indice
						calc_allHITOutDF <- t(calc_allHITOut$statistic)
						calc_allHITOutDF <- data.frame(calc_allHITOutDF)
						names(calc_allHITOutDF) <- calc_allHITOut$indice
						calc_allHITOutDF$comment = as.character(withCFS_vec[i])
					}
					
					newEflowDat <- dplyr::bind_cols(info, magnifStatsOutDF, calc_allHITOutDF)
					
					EflowDat <- bind_rows(EflowDat, newEflowDat)
					
				}
				
				return(EflowDat)
			}
		}
	}
}
