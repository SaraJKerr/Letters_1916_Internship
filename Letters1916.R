################################################################################
# File-Name: Letters1916.R                                                     #
# Date: 5 October 2016                                                         #
# Author: Sara J Kerr                                                          # 
# ORCID:orcid.org/0000-0002-2322-1178                                          #
# Institution: Maynooth University                                             #
# Project: Letters of 1916 - Prof Susan Schreibman                             #
# Purpose: Analysis and visualisation of Letters 1916 corpus for internship    #
# Based on: http://www.informit.com/articles/article.aspx?p=2215520            #
#       Jockers (2014) Text Analysis with R for Students of Literature         #
# Data Used: Letters in xml format, downloaded from LetEd                      #
# Packages Used: XML, plyr,                                                    #
# Input:                                                                       #
# Output:                                                                      #
# Last Updated: 7 October 2016                                                 #
################################################################################

# Install packages
install.packages("XML")
install.packages("gridExtra")

# Load packages
require("XML")
require("plyr")
require("ggplot2")
require("gridExtra")

# Set working directory using Session option in R Studio

# Read in the file
xmlfile <- xmlParse("RawData/Letters/32.xml")

# Check file has loaded ok
class(xmlfile)

# save content of root - this saves the whole tree structure
xmltop <- xmlRoot(xmlfile)
class(xmltop)

# Give the name of the node
xmlName(xmltop) 

# How many children in node
xmlSize(xmltop)
# In this case the node has 3 children - to access them
xmlName(xmltop[[1]])
xmlName(xmltop[[2]])
xmlName(xmltop[[3]])

# Root node's children
xmlSize(xmltop[[1]])
xmlSApply(xmltop[[1]], xmlName)
xmlSize(xmltop[[2]])
xmlSApply(xmltop[[2]], xmlName)
xmlSize(xmltop[[3]])
xmlSApply(xmltop[[3]], xmlName)

xmlSApply(xmltop[[1]], xmlAttrs) #attribute(s)
xmlSApply(xmltop[[1]], xmlSize) #size
xmlSApply(xmltop[[2]], xmlAttrs) #attribute(s)
xmlSApply(xmltop[[2]], xmlSize) #size
xmlSApply(xmltop[[3]], xmlAttrs) #attribute(s)
xmlSApply(xmltop[[3]], xmlSize) #size

# Look at the subnodes which contain the data of interest
xmltop[[1]][[1]] # Shows the content of fileDesc
xmltop[[3]][[1]] # Shows the content of group

xmlSApply(xmltop[[1]][[1]], xmlName) # Elements of fileDesc

xmltop[["teiHeader"]][["fileDesc"]][["titleStmt"]][["title"]] # Showing the title
xmltop[["teiHeader"]][["fileDesc"]][["titleStmt"]][["author"]] # Showing the author
xmltop[["teiHeader"]][["profileDesc"]][["correspDesc"]]      # Shows correspondence details
xmltop[["teiHeader"]][["fileDesc"]][["publicationStmt"]][["idno"]] # Letter id number
xmltop[["teiHeader"]][["fileDesc"]][["notesStmt"]][["p"]] # Letter Summary
xmltop[["text"]][["group"]][["text"]][["body"]] # Body text


let_title <- xmlToList(xmltop[["teiHeader"]][["fileDesc"]][["titleStmt"]]
                       [["title"]]) # Save title
let_author <- xmlToList(xmltop[["teiHeader"]][["fileDesc"]][["titleStmt"]]
                        [["author"]]) # Save Author
let_id <- xmlToList(xmltop[["teiHeader"]][["fileDesc"]][["publicationStmt"]]
                    [["idno"]]) # Save letter id
let_summary <- xmlToList(xmltop[["teiHeader"]][["fileDesc"]][["notesStmt"]]
                         [["note"]] [["p"]]) # Save Summary

bod_list <- xmlToList(xmltop[["text"]][["group"]][["text"]][["body"]])
let_date <- bod_list[["opener"]][["dateline"]][["date"]]
let_salute <- bod_list[["opener"]][["salute"]]
let_signed <- bod_list[["closer"]][["signed"]]
let_close <- bod_list[["closer"]][["salute"]]
let_loc <- xmlToList(xmltop[[1]][[2]][[1]][[2]][[2]][[1]])

bod_vec <- xmlValue(xmltop[["text"]][["group"]][["text"]][["body"]])   # character vector
bod_vec <- strsplit(bod_vec, "\\W")
bod_vec <- unlist(bod_vec)
bod_vec <- bod_vec[which(bod_vec != "")]
let_body1 <- bod_vec
let_body2 <- paste0(bod_vec, collapse = " ")


