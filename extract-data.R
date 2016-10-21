################################################################################
# File-Name: extract-data.R                                                    #
# Date: 7 October 2016                                                         #
# Author: Sara J Kerr                                                          # 
# ORCID:orcid.org/0000-0002-2322-1178                                          #
# Institution: Maynooth University                                             #
# Project: Letters of 1916 - Prof Susan Schreibman                             #
# Purpose: Extraction of information from the letters                          #
# Based on: http://www.informit.com/articles/article.aspx?p=2215520            #
#       Jockers (2014) Text Analysis with R for Students of Literature         #
# Data Used: Letters in xml format                                             #
# Packages Used: XML, plyr,                                                    #
# Input:   an XML document                                                     #
# Output:  a list containing the 3 parts of the data                           #
# Last Updated: 21 October 2016                                                #
################################################################################

# Function to read data from letter, save in variables and create data frames
# for the key information. 
# Argument:
#       xmlfile - an xml document (TEI encoded letters)
# Returns a list containing the 3 parts of the data - called letters_data, 
# letters_keywords, and letters_words

extract_data <- function(xmlfile, ...) {
        xmltop <- xmlRoot(xmlfile) 
        # Extract the required information from the teiHeader
        let_title <- xmlToList(xmltop[["teiHeader"]][["fileDesc"]]
                                     [["titleStmt"]][["title"]]) 
        let_author <- xmlToList(xmltop[["teiHeader"]][["fileDesc"]]
                                      [["titleStmt"]][["author"]]) 
        let_id <- xmlToList(xmltop[["teiHeader"]][["fileDesc"]]
                                  [["publicationStmt"]][["idno"]]) 
        let_summary <- xmlToList(xmltop[["teiHeader"]][["fileDesc"]]
                                       [["notesStmt"]][["note"]] [["p"]]) 
        let_repo <- xmlToList(xmltop[["teiHeader"]][["fileDesc"]]
                                    [["sourceDesc"]][["msDesc"]] 
                                    [["msIdentifier"]] [["repository"]]
                                    [["text"]])
        # let_date <- xmlToList(xmltop[["teiHeader"]][["profileDesc"]]
        #                             [["correspDesc"]][[2]][[3]])
        let_keywords <- sapply(xmlChildren(xmltop[["teiHeader"]]
                                    [["profileDesc"]][["textClass"]]
                                    [["keywords"]][["list"]]), xmlValue)
        
        
        # Extract the main text content from the body
        bod_vec <- xmlValue(xmltop[["text"]][["group"]][["text"]][["body"]])   
        bod_vec <- strsplit(bod_vec, "\\W")
        bod_vec <- unlist(bod_vec)
        bod_vec <- bod_vec[which(bod_vec != "")]
        
        # A vector where each element is a word
        let_body1 <- bod_vec 
        
        # A vector with the whole text as an element
        let_body2 <- paste0(bod_vec, collapse = " ")
        
        
        letters_data <- as.data.frame(cbind(let_id, let_title, let_author,
                                            let_repo, let_summary, 
                                            let_body2))
        
        # let_keywords and let_words are saved as separate data frames as they
        # have multiple elements 
        letters_keywords <- as.data.frame(rbind(c(let_id, let_keywords)))
        
        letters_words <- as.data.frame(rbind(c(let_id,let_body1)))
        
        # As R doesn't allow multiple results to be returned a list containing
        # the three data frames is created - they can be extracted easily by
        data_bundle <- list(letters_data, letters_keywords, letters_words)
        return(data_bundle)
}




