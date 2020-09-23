nmfsTradeData <- function(prodType = "GROUNDFISH", tradeType = "EXP",
                          fromYr = 2015, toYr = 2019, checkName = TRUE){
# 
  require(httr)     
  require(jsonlite) 
  require(dplyr)    
  
  if(!tradeType%in%c("EXP","IMP","REX")){
    stop("tradeType must be 'EXP' (exports),'IMP' (imports), or 'REX' (re-exports).")
  }
  
  prodType <- toupper(prodType)
  if(checkName){
    source("./PRODUCTNAMES.R")
    if(!prodType%in%.productNames)
    stop("The product type was not found in the list of potential product types.\\
         See product types at https://foss.nmfs.noaa.gov/apexfoss/f?p=215:2:15168872762911::NO::: ")
  }
  
  datList <- list()
  for(yr in fromYr:toYr){
    request <- paste0('{"name":{"$like":"%',prodType,'%"},"source":"',tradeType,'","year":"',yr,'"}')
    
    ## send the request to the NMFS server and returns a response.
    response <- GET('https://www.st.nmfs.noaa.gov/ords/foss/trade_data',
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
    
    df <- df %>% transform(year=as.numeric(year), month = as.numeric(month),
                           links = NULL, hasMore = NULL, 
                           limit = NULL, offset = NULL, count = NULL)
    
    datList[[as.character(yr)]] <- df
  }  
  dat.f <- bind_rows(datList)
  dat.f <- data.frame(as.list(dat.f), stringsAsFactors=TRUE)
  return(dat.f)
}
