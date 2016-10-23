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
# Last Updated: 23 October 2016                                                #
################################################################################

# Load packages
require("XML")
require("plyr")

# Set working directory using Session option in R Studio

# Source functions to be used - these will eventually be updated to the GitHub
# versions

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

# Load the wordVectors package
library(devtools)
install_github("bmschmidt/wordVectors") # Needed to check that Xcode license agreed
library(wordVectors)

# Prepare the text file
# If a prepared text file has not already been created follow this step - it 
# takes in a folder of .txt files and outputs a single .txt file which combines
# the texts in one document removes punctuation and converts all words to lower
# case.

prep_word2vec("Data/Letters_txt", "Data/Combined_Letters.txt", lowercase =  T)

# Corpus of letters saved as Combined_Letters.txt in Data folder

# Train the model

# train_word2vec takes several parameters - an input prepared .txt file, an 
# output file, vectors are the number of dimensions the default is 100, and
# window is the number of words either side of the context word, by default
# the function uses skip-gram this can be changed by including cbow = 1

let <- train_word2vec("Data/Combined_Letters.txt", output = "Data/letters.bin", 
                     threads = 3, vectors = 100, window = 12)

# if a model is already created:

let <- read.vectors("Data/Letters.bin")

library(tsne)
library(Rtsne)
library(ggplot2)
library(ggrepel)
library(magrittr) 

reduction <- Rtsne(as.matrix(let), dims = 2, initial_dims = 100, 
                   perplexity = 30, theta = 0.5, check_duplicates = F,
                   pca = F, max_iter = 1000, verbose = F, 
                   is_distance = F, Y_init = NULL)

df <- as.data.frame(reduction$Y)
rows <- rownames(let)
rownames(df) <- rows

write.csv(df, paste0("Data/Letters.csv"))

# With ggrepel
ggplot(df) +
        geom_point(aes(x = V1, y = V2), color = "red") +
        geom_text_repel(aes(x = V1, y = V2, label = rownames(df))) +
        xlab("Dimension 1") +
        ylab("Dimension 2 ") +
        # geom_text(fontface = 2, alpha = .8) +
        theme_bw(base_size = 12) + 
        theme(legend.position = "none") +
        ggtitle("2D reduction of VSM Letters using t_SNE")

ggsave("Letters.jpeg", path = "Viz/", width = 24, 
       height = 18, dpi = 100)

# Without ggrepel

ggplot(df, aes(x = V1, y = V2), label = rownames(df)) +
        xlab("Dimension 1") +
        ylab("Dimension 2 ") +
        geom_text(fontface = 2, alpha = .8, label = rownames(df)) +
        theme_bw(base_size = 12) + 
        theme(legend.position = "none") +
        ggtitle("2D reduction of VSM Letters using t_SNE v.2")

ggsave("Letters_v2.jpeg", path = "Viz/", width = 24, 
       height = 18, dpi = 100)

# Network analysis

# Create a matrix of cosine difference between each word in model and every 
# other word in the same model closer to 0 = similarity and closer to 2 = total
# difference

similarity <- cosineSimilarity(let, let) %>% round(2)

# Identifies index of matrix elements which are greater than 0.5
y <- which(similarity > 0.5, arr.in = TRUE)

# Edge list - 
# create vectors of words for from and to
from <- rownames(y)
to_words <- colnames(similarity)
to <- to_words[y[ , 2]]
y1 <- y[ , 1]
y2 <- y[ , 2]

# create vector for weight - this takes time but only needs done once
w <- vector()
for (i in 1:length(y1)) {
        w <- c(w, similarity[y1[i], y2[i]])
}

edgelist1 <- as.data.frame(cbind(from, to))

colnames(edgelist1) <- c("from", "to")

edgelist1$weight <- as.numeric(w) # Ensures that weight is numeric not a factor

# Checking - if max is more than 1 and min less than -1 there is a problem 
max(edgelist1$weight)
min(edgelist1$weight)

write.csv(edgelist1, "Data/Letters_edgelist.csv")

# Node list - unique rownames and colnames

words <- unique(c(from, to))

nodelist1 <- as.data.frame(cbind(words, words))
colnames(nodelist1) <- c("ID", "label")

write.csv(nodelist1, "Data/Letters_nodelist.csv")



