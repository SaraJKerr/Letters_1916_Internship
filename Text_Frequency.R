################################################################################
# File-Name: Text_Frequency.R                                                  #
# Date: 27 March 2017                                                          #
# Author: Sara J Kerr                                                          # 
# ORCID:orcid.org/0000-0002-2322-1178                                          #
# Institution: Maynooth University                                             #
# Project: Letters of 1916 - Prof Susan Schreibman                             #
# Purpose: To explore word frequency in the corpus                             #
# Based on:                           #
# Data Used:                       #
# Packages Used: ggplot2                                                    #
# Input:                                                                       #
# Output:                                                                      #
# Last Updated: 27 March 2017                                                  #
################################################################################

# This function explores word frequency in the corpus 

# Explore your data - this gives the raw frequency for each word in the corpus      
freq <- colSums(as.matrix(dtm))   
length(freq)   # the number of unique words in the corpus
head(freq) # view the top 6 elements

# Create a frequency graph of the words which appear more than n times  
library(ggplot2)

word_freq <- data.frame(word=names(freq), freq=freq)   
p <- ggplot(subset(word_freq, freq>200), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1)) 
p <- p + ggtitle("Words with Frequency of 200 or More") +
        xlab("Word") + ylab("Word Frequency")
p  
