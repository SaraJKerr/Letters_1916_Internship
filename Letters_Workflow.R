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
# Last Updated: 27 March 2017                                                  #
################################################################################

# This script allows access to a variety of functions to access, process and 
# analyse the letters

# Set working directory

# Load packages
library(XML)
library(devtools)
install_github("bmschmidt/wordVectors", force = T) # Check Xcode license agreed
library(wordVectors)
library(tm)
library(ggplot2)
library(koRpus)

##########################################
# Step 1: Extract body text from letters #
##########################################

# Load the function to extract the body text and save each as .txt files
source("Code/Letters_1916_Internship/Text_Extract.R") # From my Mac


# Identify folder where XML files are saved and which format they are in
input_dir1 <- "RawData/Letters" # path to xml letters
files <- dir(input_dir1, "\\.xml") # vector of file names
input_xml <- file.path(input_dir1,files) # this creates a path to each file

# Run the function
x <- lapply(input_xml, text_extract) # x returns NULL but the files are written

#############################
# Step 2: Process the texts #
#############################
# Prior to running the script TreeTagger needs to be downloaded to your computer
# it can be downloaded from http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/
# Instructions for downloading and set up of TreeTagger are on the site.
# If error 'error TreeTagger/lib/english.par not found' appears check the
# TreeTagger 'lib' folder and rename the 'english-utf8.par' file 'english.par'.

# Load the function to process the files
source("Code/Letters_1916_Internship/Text_Process.R") # From my Mac

# Identify folder where .txt files are saved and which format they are in
input_dir2 <- "Text_Files" # path to .txt letters' folder

y1 <- lapply(input_dir2, text_process) # combined .txt files and a DTM created

input_dir3 <- "Processed_Files/Letters_cap.txt" # This output by the line above

y2 <- text_tag(input_dir3) # 

#####################################
# Step 3: Word2Vec
#####################################

source("Code/Letters_1916_Internship/Text_Word_Vec_Analysis.R") # From my Mac

text <- "Processed_Files/Letters_corpus.txt"
w2v_train(text)

