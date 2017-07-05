################################################################################
# File-Name: Extract_Neighbourhood.R                                           #
# Date: 5th July 2017                                                          #
# Author: Sara J Kerr                                                          # 
# ORCID:orcid.org/0000-0002-2322-1178                                          #
# Institution: Maynooth University                                             #
# Project: Letters of 1916 - Prof Susan Schreibman                             #
# Purpose: To extract the neighbourhood of a specific term                     #
# Data Used:                                                                   #
# Packages Used:                                                               #
# Input:                                                                       #
# Output:                                                                      #
# Last Updated: 5th July 2017                                                  #
################################################################################

# Read in prepared model

vsm <- read.vectors("W2V/Let_300.bin")

set.seed(42)

# Chosen word for test 
word <- "prison"

# Identify the nearest n words to the average vector of search terms and 
main <- nearest_to(vsm, vsm[[word]], 500)
wordlist <- names(main)


# Create a subset vector space model
new_model <- vsm[[wordlist, average = F]]

# Calculate cosine similarity and round to 2 decimal places
sim <- cosineSimilarity(new_model, new_model) %>% round(2)

# convert those below threshold to 0
sim[sim < max(sim)/2] <- 0
g <- graph_from_incidence_matrix(sim)

# This extracts the first step network - going beyond this makes the plot too complex
g3 <- make_ego_graph(graph = g, order = 1, nodes = V(g)[name == word], 
                     mode = "out", mindist = 0)

# As g3 is a list the first element needs to be selected
edges <- get.edgelist(g3[[1]])
edges <- as.data.frame(edges)

nodes <- as.data.frame(unique(edges[, 2])) # Removes duplicate words

net <- graph_from_data_frame(d = edges, vertices = nodes, directed = T)

net <- simplify(net, remove.multiple = T, remove.loops = T) # removes loops
net # This prints a section of the vertex names and links

# This acts as a test that the graph will plot (lables have been removed)
plot(net, edge.arrow.size = .4, vertex.label = NA)


# Create visNetwork version of graph

data <- toVisNetworkData(net)
visNetwork(nodes = data$nodes, edges = data$edges, main = "Prison") %>%
  visOptions(highlightNearest = T) %>%
  visSave("Prison.html")


