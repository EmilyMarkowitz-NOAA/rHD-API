nmfsLandingsData <- function(species = "POLLOCK, WALLEYE", landingsType = "Commercial",
                          fromYr = 2015, toYr = 2019, checkName = TRUE){
# 
  require(httr)     
  require(jsonlite) 
  require(dplyr)    
  
  if(!landingsType%in%c("Commercial","Recreational")){
    stop("landingsType must be 'Commercial' 'Recreational'.")
  }
  
  species <- toupper(species)
  if(checkName){
    source("./speciesNames.R")
    specNmChoices <- agrep(species,.speciesNames, ignore.case=TRUE, value = TRUE)
    if(length(specNmChoices)==0){
      stop("The species was not found in the list of potential types.\n
         See species types at https://foss.nmfs.noaa.gov/apexfoss/f?p=215:200:8868594257893::NO::: ")
    }
    if(length(specNmChoices)>1){
      message("The entry for species matched the following potential species \n")
      cat(specNmChoices, sep=";  ")
      message("\n You can either enter 'Q' and provide a more specific species name, \n
                or enter 'c' to attempt the species name provided (which may pull multiple species).")
      browser()
    }
    if(length(specNmChoices)==1){
      species <- specNmChoices
    }
  }
  
  datList <- list()
  for(yr in fromYr:toYr){
    request <- paste0('{"ts_afs_name":{"$like":"%',species,'%"},"collection":"',landingsType,'","year":"',yr,'"}')
    
    ## send the request to the NMFS server and returns a response.
    response <- GET('https://www.st.nmfs.noaa.gov/ords/foss/landings',
                 query = list(q = request, limit = 10000))
    ## Checks for a successful reponse (any other code beside 200 is a failure). Different codes may indicate how it failed though.
    if(status_code(response)!=200)
      stop(paste("Request to API failed. Status code of response:",status_code(response)))
    ## retrives the content of the request
    df <- content(response, as = "text", encoding = "UTF-8")
    df <- fromJSON(df, flatten = TRUE)
    df$links <- NULL
    df <- df %>% data.frame()
    
    names(df) <- sub("items.","",names(df),fixed=TRUE)
    
    if(any(df$hasMore))
      stop("The request exceeded the 10,000 observation limit and did not retrieve of the data. Try constraining your search.")
    
    df <- df %>% transform(year=as.numeric(year),
                           links = NULL, hasMore = NULL, 
                           limit = NULL, offset = NULL, count = NULL)
    
    datList[[as.character(yr)]] <- df
  }  
  dat.f <- bind_rows(datList)
  dat.f <- data.frame(as.list(dat.f), stringsAsFactors=TRUE)
  return(dat.f)
}
