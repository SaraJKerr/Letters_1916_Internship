################################################################################
# File-Name: Letters_Workflow.R                                                #
# Date: 27 March 2017                                                          #
# Author: Sara J Kerr                                                          # 
# ORCID:orcid.org/0000-0002-2322-1178                                          #
# Institution: Maynooth University                                             #
# Project: Letters of 1916 - Prof Susan Schreibman                             #
# Purpose: Workflow script combining functions                                 #
# Based on:                         #
# Data Used: Letters in xml format, downloaded from LetEd                      #
# Packages Used: XML,                                                               #
# Input:                                                                       #
# Output:                                                                      #
# Last Updated: 27 March 2017                                                  #
################################################################################

# This script allows access to a variety of functions to access, process and 
# analyse the letters

# Set working directory

# Load packages
library(XML)

##########################################
# Step 1: Extract body text from letters #
##########################################

# Load the function to extract the body text and save each as .txt files
source("Code/Letters_1916_Internship/Text_Extract.R") # From my Mac

# Identify folder where XML files are saved and which format they are in
input_dir <- "RawData/Letters" # path to xml letters
files <- dir(input_dir, "\\.xml") # vector of file names
input <- file.path(input_dir,files) # this creates a path to each file

# Run the function
x <- lapply(input, text_extract) # x will return as NULL but files are written
