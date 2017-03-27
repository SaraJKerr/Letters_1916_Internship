################################################################################
# File-Name: Letters_Workflow.R                                                #
# Date: 27 March 2017                                                          #
# Author: Sara J Kerr                                                          # 
# ORCID:orcid.org/0000-0002-2322-1178                                          #
# Institution: Maynooth University                                             #
# Project: Letters of 1916 - Prof Susan Schreibman                             #
# Purpose: Workflow script combining functions                                 #
# Data Used: Letters in xml format, downloaded from LetEd                      #
# Packages Used: XML, devtools, wordVectors, tm, ggplot2                                #
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
# install_github("bmschmidt/wordVectors", force = T) # Check Xcode license agreed
library(wordVectors)
library(tm)
library(ggplot2)

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

# Load the function to process the files
source("Code/Letters_1916_Internship/Text_Process.R") # From my Mac

# Identify folder where .txt files are saved and which format they are in
input_dir2 <- "Text_Files" # path to .txt letters' folder

y <- lapply(input_dir2, text_process)

#####################################
# Step 3:
#####################################

