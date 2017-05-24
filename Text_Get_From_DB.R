################################################################################
# File-Name: Text_Get_From_DB.R                                                #
# Date: 7 April 2017                                                           #
# Author: Shane A. McGarry                                                     # 
# ORCID:orcid.org/0000-0002-0312-9163                                          #
# Institution: Maynooth University                                             #
# Project: Letters of 1916 - Prof Susan Schreibman                             #
# Purpose: Load data from the letters explore relational database for          #
#          analysis and processing. Removes the need for physical TEI files.   #
# Based on: http://www.informit.com/articles/article.aspx?p=2215520            #
#       Jockers (2014) Text Analysis with R for Students of Literature         #
#       http://www.omegahat.net/RSXML/shortIntro.html                          #
# Data Used: Letters in json format                                            #
# Packages Used: XML, httr, jsonlite                                           #
# Input: keyword (string)                                                      #
# Output: json collection as R object                                          #
# Last Updated:                                                                #
################################################################################

# This function calls a RESTful webservice and passes a base64 encoded string in 
# order to search by keyword and generate a subset of documents
# NOTE: This function is currently not working as it seems there is a problem with the
# web api

get_from_webCall <- function(keyword) {
  base_qs <- ""
  
  if(is.null(keyword) || keyword == "") {
    base_qs <- "all"
  } else {
    query_string <- paste("textString", keyword, sep="=")
    base_qs <- base64enc::base64encode(charToRaw(query_string))
  }
  
  uri <- paste(config_api_baseuri, base_qs, sep="/")
  
  print(uri)
  
  rest_result <- httr::GET(uri)
  print(rest_result)
  data_collection <- jsonlite::fromJSON(content(rest_result, as="text"), flatten=TRUE)
  return(data_collection)
}

get_all_from_db <- function() {
  mydb = dbConnect(MySQL(), user=config_db_user, password=config_db_pass, dbname=config_db_dbname, host=config_db_host)
 
  #this method gets just the id and body (with all HTML characters removed) from the database for ALL letters in explore
  query_result = dbSendQuery(conn=mydb, statement='SELECT publicIdentifier, StripHtmlTags(HtmlDecode(body)) AS "body" FROM letters;')
  data = dbFetch(query_result, n=-1)
  
  #clean up the connection
  dbClearResult(query_result)
  on.exit(disconnect())
  
  return(data)
}

disconnect <- function() {
  ile <- length(dbListConnections(MySQL()))
  lapply(dbListConnections(MySQL()), function(x) dbDisconnect(x))
  cat(sprintf("%s connection(s) closed.\n", ile))
}