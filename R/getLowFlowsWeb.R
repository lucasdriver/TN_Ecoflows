

getLowFlow <- function(siteIds) {
  
  if(!require(rvest)){             
    install.packages("rvest")      
  }
  
  if(!require(dplyr)){             
    install.packages("dplyr")      
  }
  
  if(!require(stringr)){             
    install.packages("stringr")      
  }
  
  library(rvest) 
  library(dplyr)    
  library(stringr)
  
  stats <- "7_Day_10_Year_Low_Flow"
  
  results <- data.frame(site_no = as.character(),
                        stats = as.character(),
                        val = as.numeric(),
                        stringsAsFactors = FALSE)
  
  for(i in 1:length(siteIds)) {
    
    N <- length(siteIds)
    station <- siteIds[i]
    url <- paste0("https://streamstatsags.cr.usgs.gov/gagepages/html/", station, ".htm")
    print(paste0(round((i/N)*100,2),"%"," complete"))
    
    hold <- tryCatch(
      url %>%
        read_html() %>%
        html_nodes(xpath = '/html/body/table[3]') %>%
        html_table() %>%
        as.data.frame(),
      error = function(e){"failure"}
      )
    
    if(hold == "failure") {
      
      finDF <- data.frame(site_no = station, 
                          stats = "failed",
                          val = NA,
                          stringsAsFactors = FALSE)
      
    }else if(!(stats %in% hold$X1)[1] == TRUE){
      
      finDF <- data.frame(site_no = station, 
                          stats = "failed",
                          val = NA,
                          stringsAsFactors = FALSE)
      
    }else{
      
      statsNew <- (hold[(hold$X1 %in% stats),])[, 1]
      vals <- dplyr::filter(hold, X1 %in% stats)[, 2]
      finDF <- data.frame(site_no = rep(station, length(vals)), 
                          stats = statsNew, 
                          val = as.numeric(vals),
                          stringsAsFactors = FALSE)
    }
    
    results <- dplyr::bind_rows(results, finDF); rm(finDF)
    
  }
  
  return(results)
}


