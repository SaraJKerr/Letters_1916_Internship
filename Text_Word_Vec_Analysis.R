################################################################################
# File-Name: Text_Word_Vec_Analysis.R                                          #                        
# Date: 1 September 2016                                                       #
# Author: Sara J Kerr                                                          #
# ORCID:orcid.org/0000-0002-2322-1178                                          #
# Purpose: Word2Vec creation, analysis and visualisation                       #
# Based on: https://github.com/bmschmidt/wordVectors/tree/master/R             #
#           http://www.codeproject.com/Tips/788739/Visualization-of-High-      #
#               Dimensional-Data-using-t-SNE                                   #
#           Matthew L. Jockers (2014) Text Analysis with R                     #
#           https://eight2late.wordpress.com/2015/12/02/a-gentle-introduction- #
#               to-network-graphs-using-r-and-gephi/                           #
#           http://kateto.net/networks-r-igraph                                #
#                                                                              #
# Data Used: Plain text file of combined texts                                 #
# Packages Used: wordVectors, tsne, Rtsne, magrittr, ggplot2, ggrepel,         #
#                stringi                                                       #
# Input: folder of plain text files                                            #
# Output: VSMs, csv files, wordlists, t-SNE plots, html network graphs                                    #
# Last Updated: 8th May 2017                                                   #
################################################################################

# w2v_train uses train_word2vec and allows several variables to be used
# to create 4 VSM, one based on the default settings, one based on Baroni, Dinu
# and Kruszewski's (2014) suggestions for predictive models, and one with a larger
# window and negative samples also per Baroni et al.
# train_word2vec takes several parameters - an input prepared .txt file, an 
# output file, vectors are the number of dimensions the default is 100, and
# window is the number of words either side of the context word, by default
# the function uses skip-gram this can be changed by including cbow = 1

# text <- "Processed_Files/Letters_corpus.txt"

w2v_train <- function(text) {
        train_word2vec(text, output= "Results/W2V/Let_default.bin",
                       threads = 2, vectors = 100, window = 12,
                       negative_samples = 5)
        
        train_word2vec(text, output = "Results/W2V/Let_win5.bin", 
                       threads = 2, vectors = 400, window = 5,
                       negative_samples = 10)
        
        train_word2vec(text, output = "Results/W2V/Let_win2.bin", 
                       threads = 2, vectors = 300, window = 2,
                       negative_samples = 10)
        
        train_word2vec(text, output = "Results/W2V/Let_win15.bin", 
                       threads = 2, vectors = 300, window = 15,
                       negative_samples = 10)
        
}
        
################################################################################

# text_kwic takes in a .txt file checks whether a target word is present and,
# if so, creates a dataframe with text name and keyword in context and saves it.

text_kwic <- function(files, input, word, context) {
        corpus <- make_word_list(files, input)
        context <- as.numeric(context)
        keyword <- tolower(word)
        result <- NULL
        # create the KWIC readout
        for (i in 1:length(corpus)) {
                hits <- which(corpus[[i]] == keyword)
                let <- files[i]
                if(length(hits) > 0){
                        for(j in 1:length(hits)) {
                                start <- hits[j] - context
                                if(start < 1) {
                                        start <- 1
                                }
                        end <- hits[j] + context
                        myrow <- cbind(let, hits[j],
                               paste(corpus[[i]][start: (hits[j] -1)],
                                                     collapse = " "),
                                paste(corpus[[i]][hits[j]],
                                                     collapse = " "),
                                paste(corpus[[i]][(hits[j] +1): end],
                                                     collapse = " "))
                                result <- rbind(result, myrow)
                        }
                        
                } else {
                        z <- paste0(let, " YOUR KEYWORD WAS NOT FOUND\n")
                        cat(z)
                }
        }
        colnames(result) <- c("file", "position", "left",
                              "keyword", "right")
        write.csv(result, paste0("Results/KWIC/", word, "_", 
                                 context, ".csv"))
        cat("Your results have been saved")
}

# Function used within text_kwic
        
make_word_list <- function(files, input.dir) {
        # create an empty list for the results
        word_list <- list()
        # read in the files and process them
        for(i in 1:length(files)) {
                text <- scan(paste(input.dir, files[i], sep = "/"), 
                             what = "character", sep = "\n")   
                text <- paste(text, collapse = " ")
                text_lower <- tolower(text)
                text_words <- strsplit(text_lower, "\\W")
                text_words <- unlist(text_words)
                text_words <- text_words[which(text_words != "")]
                word_list[[files[i]]] <- text_words
        }
        return(word_list)
}


################################################################################

# w2v_analysis2 analyses a chosen term in a vector space model
# The function takes 6 arguments:
# vsm - a vector space model 
# words - a character vector of focus words
# seed - an integer
# path - the path to the folder you want files saved to 
# ref_name - the reference name for the exported files 
# num - the number of nearest words you wish to examine

# The function will create a vector which is the average of the words input and 
# will output a wordlist of the n nearest words, a csv of the words and their
# positions, and a plot of the 2D reduction of the vector space
# model using the Barnes-Hut implementation of t-SNE. The points for each word
# are marked in red so the labels can be moved by ggrepel for ease of reading.
# An HTML network graph for the chosen word will also be created
# set.seed is used to ensure replicability

w2v_analysis2 <- function(vsm, words, seed, path, ref_name, num) {
        # Set the seed
        if (!missing(seed))
                set.seed(seed)
        
        # Identify the nearest 10 words to the average vector of search terms
        ten <- nearest_to(vsm, vsm[[words]])
        
        # Identify the nearest n words to the average vector of search terms and 
        # save as a .txt file
        main <- nearest_to(vsm, vsm[[words]], num)
        wordlist <- names(main)
        filepath <- paste0(path, ref_name)
        write(wordlist, paste0(filepath, ".txt"))
        
        
        # Create a subset vector space model
        new_model <- vsm[[wordlist, average = F]]
        
        # Run Rtsne to reduce new VSM to 2D (Barnes-Hut)
        reduction <- Rtsne(as.matrix(new_model), dims = 2, initial_dims = 50, 
                           perplexity = 30, theta = 0.5, check_duplicates = F,
                           pca = F, max_iter = 1000, verbose = F, 
                           is_distance = F, Y_init = NULL)
        
        # Extract Y (positions for plot) as a dataframe and add row names
        df <- as.data.frame(reduction$Y)
        rows <- rownames(new_model)
        rownames(df) <- rows
        
        # Save dataframe as .csv file
        write.csv(df, paste0(filepath, ".csv"))
        
        # Create t-SNE plot and save as jpeg
        ggplot(df) +
                geom_point(aes(x = V1, y = V2), color = "red") +
                geom_text_repel(aes(x = V1, y = V2, label = rownames(df),
                                    size = 8)) +
                xlab("Dimension 1") +
                ylab("Dimension 2 ") +
                theme_bw(base_size = 16) + 
                theme(legend.position = "none") +
                ggtitle(paste0("2D reduction of VSM ", ref_name, " using t_SNE"))
        
        ggsave(paste0(ref_name, ".jpeg"), path = path, width = 24, 
               height = 18, dpi = 100)
        
        # Create a network plot of the words
        sim <- cosineSimilarity(new_model, new_model) %>% round(2)
        # convert those below threshold to 0
        sim[sim < max(sim)/2] <- 0
        g <- graph_from_incidence_matrix(sim)
        # Create a graph object
        edges <- get.edgelist(g)
        # Name columns
        colnames(edges) <- c("from", "to")
        g2 <- graph(edges = edges)
        g2 <- simplify(g2) # removes loops
        # Community detection based on greedy optimization of modularity
        cfg <- cluster_fast_greedy(as.undirected(g2))
        V(g2)$community <- cfg$membership
        pal2 <- rainbow(33, alpha = 0.7) 
        V(g2)$color <- pal2[V(g2)$community]
        data <- toVisNetworkData(g2)
        visNetwork(nodes = data$nodes, edges = data$edges, main = "Network of Letters
           Clustered by Fast Greedy") %>%
                visOptions(highlightNearest = T,
                           selectedBy = "community") %>%
                visSave(paste0(ref_name, ".html"))
        
                
        new_list <- list("Ten nearest" = ten, "Status" = "Analysis Complete") 
        return(new_list)
        
}

