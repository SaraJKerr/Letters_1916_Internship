################################################################################
# File-Name: Text_Extract.R                                                     #
# Date: 5 October 2016                                                         #
# Author: Sara J Kerr                                                          # 
# ORCID:orcid.org/0000-0002-2322-1178                                          #
# Institution: Maynooth University                                             #
# Project: Letters of 1916 - Prof Susan Schreibman                             #
# Purpose: Extract body of Letters 1916 corpus for internship                  #
# Based on: http://www.informit.com/articles/article.aspx?p=2215520            #
#       Jockers (2014) Text Analysis with R for Students of Literature         #
#       http://www.omegahat.net/RSXML/shortIntro.html                          #
# Data Used: Letters in xml format, downloaded from LetEd                      #
# Packages Used: XML,                                                               #
# Input:                                                                       #
# Output:                                                                      #
# Last Updated: 27 March 2017                                                  #
################################################################################

# This function extracts the letter id and the body text from the XML text.
# It then saves the result as a text file with the letter id as its name.

# Identify folder where XML files are saved and which format they are in
# input_dir <- "RawData/Letters" # path to xml letters
# files <- dir(input_dir, "\\.xml") # vector of file names
# input <- file.path(input_dir,files) # this creates a path to each letter

text_extract <- function(letter) {
        xmlfile <- xmlParse(letter, useInternalNodes = T)
        xmltop <- xmlRoot(xmlfile)
        id <- xmlToList(xmltop[["teiHeader"]][["fileDesc"]]
                        [["publicationStmt"]][["idno"]])
        bod <- xmlValue(xmltop[["text"]][["group"]][["text"]][["body"]])   
        bod <- strsplit(bod, "\\W")
        bod <- unlist(bod)
        bod <- bod[which(bod != "")]
        bod <- paste0(bod, collapse = " ")
        write(bod, paste0("Text_Files/", id, ".txt"))
}


# To run:
# result <- lapply(input, text_extract)
