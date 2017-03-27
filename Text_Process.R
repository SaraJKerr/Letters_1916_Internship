################################################################################
# File-Name: Text_Process.R                                                    #
# Date: 23 October 2016                                                        #
# Author: Sara J Kerr                                                          # 
# ORCID:orcid.org/0000-0002-2322-1178                                          #
# Institution: Maynooth University                                             #
# Project: Letters of 1916 - Prof Susan Schreibman                             #
# Purpose: Process texts in preparation for analysis                           #
# Based on:                         #
# Data Used: Letters in .txt format                                            #
# Packages Used: devtools, wordVectors, tm                                     #
# Input:                                                                       #
# Output:                                                                      #
# Last Updated: 27 March 2017                                                  #
################################################################################

# This script processes the .txt files and saves them in the formats needed for
# the analyses which follow

# Identify folder where .txt files are saved and which format they are in
# input_dir2 <- "Text_files" # path to .txt letters

text_process <- function(input_dir2) {
        prep_word2vec(input_dir2, "Processed_Files/Letters_corpus.txt", 
                      lowercase = T)
        
        let <- dir(input_dir2) # Saves file names (letter id)
        
        docs <- Corpus(DirSource(input_dir2)) # Create V corpus
        docs <- tm_map(docs, removePunctuation)    
        docs <- tm_map(docs, tolower)   # Convert to lowercase   
        docs <- tm_map(docs, removeWords, stopwords("english")) 
        docs <- tm_map(docs, stripWhitespace)   
        docs <- tm_map(docs, PlainTextDocument)
        
        dtm <- DocumentTermMatrix(docs) # Create a DTM
        
        m <- as.matrix(dtm) # Convert to a matrix
        rownames(m) <- let # add letter id as rownames
        
        write.csv(m, "Processed_Files/DTM_Letters") # Writes DTM to file
        
}

