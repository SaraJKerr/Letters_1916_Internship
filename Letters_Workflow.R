################################################################################
# File-Name: Letters_Workflow.R                                                #
# Date: 27 March 2017                                                          #
# Author: Sara J Kerr                                                          # 
# ORCID:orcid.org/0000-0002-2322-1178                                          #
# Institution: Maynooth University                                             #
# Project: Letters of 1916 - Prof Susan Schreibman                             #
# Purpose: Workflow script combining functions                                 #
# Data Used: Letters in xml format, downloaded from LetEd                      #
# Packages Used: XML, devtools, wordVectors, tm, koRpus, ggplot2               #
# Input:                                                                       #
# Output:                                                                      #
# Last Updated: 29 March 2017                                                  #
################################################################################

# This script allows access to a variety of functions to access, process and 
# analyse the letters

# Set working directory

# Load packages
library(XML)
library(devtools)
install_github("bmschmidt/wordVectors", force = T) # Check Xcode license agreed
install_github("trestletech/plumber")
library(wordVectors)
library(tm)
library(ggplot2)
library(koRpus)
library(ggrepel)
library(stringi)
library(magrittr)
library(igraph)
library(RColorBrewer)
library(visNetwork)
library(tsne)
library(Rtsne)
library(httr)
library(base64enc)
library(RMySQL)
library(sqldf)
source("config.R")

################################################################################
# Step 0: Get the letters from the Explore DB                                  #
################################################################################
source("Text_Get_From_DB.R")
data <- get_all_from_db()

################################################################################
# Step 1: Extract body text from letters                                       #
################################################################################

#OLD CODE FOR GENERATING FILES FROM TEI XML
# Load the function to extract the body text and save each as .txt files
#source("Code/Letters_1916_Internship/Text_Extract.R") # From my Mac

# Identify folder where XML files are saved and which format they are in
#input_dir1 <- "RawData/Letters" # path to xml letters
#files1 <- dir(input_dir1, "\\.xml") # vector of file names
#input_xml <- file.path(input_dir1,files1) # this creates a path to each file

# Run the function
#x <- lapply(input_xml, text_extract) # x returns NULL but the files are written

#NEW CODE FOR GENERATING FILES FROM DATABASE
source("Text_Extract.R")
apply(data, 1, text_extract_fromdb)

################################################################################
# Step 2: Process the texts                                                    #
################################################################################

# Prior to running the script TreeTagger needs to be downloaded to your computer
# it can be downloaded from http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/
# Instructions for downloading and set up of TreeTagger are on the site.
# If error 'error TreeTagger/lib/english.par not found' appears check the
# TreeTagger 'lib' folder and rename the 'english-utf8.par' file 'english.par'.

# Load the function to process the files
source("Text_Process.R") # From my Mac

# Identify folder where .txt files are saved and which format they are in
#input_dir2 <- "Text_Files" # path to .txt letters' folder

y1 <- lapply(config_extract_folderpath, text_process) # combined .txt files and a DTM created

#input_dir3 <- "Processed_Files/Letters_cap.txt" # This output by the line above

y2 <- text_tag(paste0(config_process_folderpath, "/Letters_cap.txt")) # 

################################################################################
# Step 3: Word2Vec                                                             #
################################################################################

source("Text_Word_Vec_Analysis.R") # From my Mac

# Train multiple models - see source file to amend parameters
#text <- "Processed_Files/Letters_corpus.txt"
text <- paste0(config_process_folderpath, "/Letters_corpus.txt")

w2v_train(text)

# Search for chosen word in corpus
#input_dir2 <- "Text_Files" # path to .txt letters' folder
files2 <- dir(config_extract_folderpath, "\\.txt") # vector of file names

# text_kwic(list of file names, input directory, word, context )
text_kwic(files2[1:100], config_extract_folderpath, "rising", 6)

input_dir4 <- paste0(config_results_folderpath, "/W2V")
files4 <- dir(input_dir4, "\\.bin") 
input_bin <- file.path(input_dir4,files4)

# Load VSMs
vsm1 <- read.vectors(input_bin[1])
vsm4 <- read.vectors(input_bin[4])

# 10 nearest words 
nearest_to(vsm1, vsm1[["rising"]])
nearest_to(vsm4, vsm4[["rising"]])

# Clustering on a small subsection
set.seed(42)
centers <- 5
x <- nearest_to(vsm1, vsm1[["rising"]], 50) # 50 nearest words to rising
y <- vsm1[[names(x), average = F]] # creates a VSM of nearest 50 words
clustering <- kmeans(y, centers = centers, iter.max = 40)
w2v_clus_vsm1 <- sapply(sample(1:centers, 5), function(n){
        names(clustering$cluster[clustering$cluster ==n][1:10])
}) 

set.seed(42)
centers <- 5
x <- nearest_to(vsm4, vsm4[["rising"]], 50) # 50 nearest words to rising
y <- vsm4[[names(x), average = F]] # creates a VSM of nearest 50 words
clustering <- kmeans(y, centers = centers, iter.max = 40)
w2v_clus_vsm4 <- sapply(sample(1:centers, 5), function(n){
        names(clustering$cluster[clustering$cluster ==n][1:10])
}) 


# Explore the vector space model - word list and plots for chosen term
# w2v_analysis2(VSM, word, seed, path for output, output file name, total words)

w2v_analysis2(vsm1, "theatre", 42, paste0(config_results_folderpath, "/VSM1_Trial/"), "theatre200", 200)

w2v_analysis2(vsm4, "prisoner", 42, paste0(config_results_folderpath, "/VSM4_Trial/"), "prisoner300", 300)



