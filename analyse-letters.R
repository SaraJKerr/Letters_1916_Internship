################################################################################
# File-Name: analyse-letters.R                                                 #
# Date: 7 October 2016                                                         #
# Author: Sara J Kerr                                                          # 
# ORCID:orcid.org/0000-0002-2322-1178                                          #
# Institution: Maynooth University                                             #
# Project: Letters of 1916 - Prof Susan Schreibman                             #
# Purpose: workflow script for analysing letters                               #
# Based on: http://www.informit.com/articles/article.aspx?p=2215520            #
#       Jockers (2014) Text Analysis with R for Students of Literature         #
# Data Used: Letters in xml format                                             #
# Packages Used: XML, plyr,                                                    #
# Input:                                                                       #
# Output:                                                                      #
# Last Updated: 21 October 2016                                                #
################################################################################

# Load packages
require("XML")
require("plyr")

# Set working directory using Session option in R Studio

# Source functions to be used

source("Code/Letters_1916_Internship/extract-data.R")

# Identify the folder where the data is stored
input_dir <- "RawData/Letters"

# Create a vector with the contents of the directory
files <- dir(input_dir, "\\.xml$")

# Create a vector with the full filepath
# Problematic files (42) have been removed - to be dealt with later
x <- file.path(input_dir,files) 


# Create empty lists
letter_data <- list()
keywords <- list()
words <- list()

# Loop over the files extracting the information - takes about 2 minutes for
# 1000 files
for (i in 1:length(x)) {
        xmlfile <- xmlParse(x[i], useInternalNodes = T)  
        results <- extract_data(xmlfile)
        letter_data <- c(letter_data, results[1])
        keywords <- c(keywords, results[2])
        words <- c(words, results[3])
}

# Convert lists into data frames
letter_data <- ldply(letter_data, data.frame)
keywords <- ldply(keywords, data.frame)
words <- ldply(words, data.frame)

# Create .txt files from letter body in letter_data and write to folder - this
# uses the let_id as the file name and also includes it as part of the body
# text for ease of tracking.

a <- as.vector(letter_data$let_id)
b <- as.vector(letter_data$let_body2)
        for (i in 1:length(a)) {
                y <- c(a[i], b[i])
                write(y, paste0("Data/Letters_txt/", a[i], ".txt"))
        } 

# Topic Modelling



# Vector Space Model

