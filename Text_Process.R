################################################################################
# File-Name: Text_Process.R                                                    #
# Date: 23 October 2016                                                        #
# Author: Sara J Kerr                                                          # 
# ORCID:orcid.org/0000-0002-2322-1178                                          #
# Institution: Maynooth University                                             #
# Project: Letters of 1916 - Prof Susan Schreibman                             #
# Purpose: Process texts in preparation for analysis                           #
# Based on: https://github.com/bmschmidt/wordVectors/tree/master/R             #
# https://cran.r-project.org/web/packages/koRpus/vignettes/koRpus_vignette.pdf #
# Data Used: Letters in .txt format                                            #
# Packages Used: devtools, wordVectors, tm, koRpus                             #
# Input:  Folder of .txt files                                                 #
# Output: A combined .txt file for w2v analysis, a combined .txt file with     #
#         upper case maintained, a DTM, list of nouns, list proper nouns       #
# Last Updated: 29 March 2017                                                  #
################################################################################

# These functions processes the .txt files and saves them in the formats needed
# for the analyses which follow. 
# Prior to running the script TreeTagger needs to be downloaded to your computer
# it can be downloaded from http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/
# Instructions for downloading and set up of TreeTagger are on the site.
# If error 'error TreeTagger/lib/english.par not found' appears check the
# TreeTagger 'lib' folder and rename the 'english-utf8.par' file 'english.par'.

################################################################################

# text_process takes in a corpus of .txt files and outputs files for w2v 
# analysis, text tagging and topic modelling.

# Identify folder where .txt files are saved and which format they are in
# input_dir2 <- "Text_files" # path to .txt letters


text_process <- function(input_dir2) {
        # Prepare .txt file for w2v analysis
        prep_word2vec(input_dir2, "Processed_Files/Letters_corpus.txt", 
                     lowercase = T)
        
        # Prepare .txt file for tagging
        prep_word2vec(input_dir2, "Processed_Files/Letters_cap.txt", 
                      lowercase = F)
        
        
        # Prepare Document Term Matrix
        let <- dir(input_dir2) # Saves file names (letter id)
        
        docs <- Corpus(DirSource(input_dir2)) # Create V corpus
        docs <- tm_map(docs, removePunctuation)   
        docs <- tm_map(docs, removeNumbers)
        docs <- tm_map(docs, tolower)   # Convert to lowercase   
        docs <- tm_map(docs, removeWords, stopwords("english")) 
        docs <- tm_map(docs, stripWhitespace)   
        docs <- tm_map(docs, PlainTextDocument)
        
        
        dtm <- DocumentTermMatrix(docs) # Create a DTM
        saveRDS(dtm, file = "Processed_Files/DTM_Letters.rds") # Write DTM to file
         
        
}

################################################################################

# text_tag uses the .txt file created in text_process with capital letters 
# intact. Output is two .txt files, one of nouns and one of proper nouns for use
# in topic modelling. 

text_tag <- function(text) {
       
         # Read in text and apply POS tagger. This creates a kRp.tagged file
        text_tagged <- treetag(text,
                               treetagger="manual", lang="en", 
                               TT.options=list(path="TreeTagger", preset="en"))
        
        # Extract the words, tags and description
        tagged_doc <- text_tagged@TT.res[, c(1,2,6)]
        
        # Extract nouns
        single_nouns <- subset(tagged_doc, tag == "NN")
        plural_nouns <- subset(tagged_doc, tag == "NNS")
        nouns <- rbind(single_nouns, plural_nouns)
        nouns <- nouns$token
        
        # Save noun file as plain text
        write(nouns, paste0("Processed_Files/Tagged/", let ,"_NN.txt"))
        
        # Extract proper nouns
        proper_single_nouns <- subset(tagged_doc, tag == "NP")
        proper_plural_nouns <- subset(tagged_doc, tag == "NPS")
        proper_nouns <- rbind(proper_single_nouns, proper_plural_nouns)
        proper_nouns <- proper_nouns$token
        
        # Save proper noun file as plain text
        write(proper_nouns, paste0("Processed_Files/Tagged/", let ,"_NP.txt"))

}


