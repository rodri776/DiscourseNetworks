# Created table of mean shortest path from each community to node of interest

library(Matrix)
#library(arrow)
library(ggplot2)
library(ggraph)
#library(RColorBrewer)
library(igraph)
library(tidygraph)
library(plyr)

sites <- list('atlantic','motherjones','breitbart','thehill','gatewaypundit')
#Enter Platform (MANUALLY, copy and paste from sites list)
my.site <- readline(prompt= "Enter Platform: ")
my.node <- readline(prompt= "Enter Node: ")

#Get Files
dir_nodes <-paste("~/Dropbox/Belief networks/Data/networks_plots_all/nodes/",my.site,"/",sep="")
dir_edges <-paste("~/Dropbox/Belief networks/Data/networks_plots_all/edges/edges200/pcor/",my.site,sep="")
files_nodes<-list.files(path=dir_nodes, pattern='csv', full.names = TRUE)
files_edges<-list.files(path=dir_edges, pattern='csv', full.names = TRUE)

#Sorting Files
files_edges1 <- as.matrix(files_edges)
xye=c()
xmo=c()
for (i in 1:nrow(files_edges1)) {
  x1 <- unlist(gregexpr('.csv', files_edges1[i]))
  xye[i] <- substr(files_edges1[i], x1-2, x1-1)
  xmo[i] <- substr(files_edges1[i], x1-4, x1-3)
}
files_edges_sorted = data.frame(t(rbind(files_edges, xye, xmo)))
files_edges_sorted = arrange(files_edges_sorted, xye, xmo) 
files_sorted = files_edges_sorted[,1]

#File Path for Exporting
graph_matrix <- list()
commNodes_folder <- "~/Desktop/CliqueMeas"
meas_name <- paste(my.site, "_ShortestPath.csv")

# Create the full file path for the CSV file & Export
csv_file <- file.path(meas_folder, meas_name)


#Big File Loop to Make & Export Data Visualizations and Network Measure Matrix
for (file in files_sorted){
  try ({
    #Creates Date 
    x <- unlist(gregexpr(".", file, fixed=TRUE))
    dte <- substr(file[1], x-4, x-1)        
    mdte = (paste("20",substr(dte,3,4),"-",substr(dte,1,2),"-01", sep=""))
  
    print(dte) # Prints for Debugging 
    # load nodes
    nodes <- read.csv(paste(dir_nodes, "nodes_", dte, ".csv", sep=""), sep = ",")
    NodesList <- (nodes$topic)
    NodesList <- NodesList[NodesList != -1] #Exclude -1, BERTopic leftovers 
    
    # load edges
    pCorMat <- as.matrix(read.csv(file, sep="", header = FALSE))
    pCorMat <- pCorMat[-1, ]
    pCorMat <- pCorMat[, -1]
    rownames(pCorMat) <- NodesList
    colnames(pCorMat) <- NodesList
    row.index <-rownames(pCorMat)
    filt <- pCorMat[row.index,] > 0 
    y <- pCorMat
    y[y < 0] <- 0
    
    #Filtering Nodes
    rows_to_keep <- c()
    for (i in 1:nrow(nodes)) {
      for (j in 1:length(NodesList)){ 
        if (as.numeric(nodes[i, 'topic']) == as.numeric(NodesList[[j]])) {
          rows_to_keep <- c(rows_to_keep, i)
        } 
      }
    }
    nodes <- nodes[rows_to_keep, ] 
    
    #Use Node Number Column for Data Vis
    baseNum <- nodes[, 4]
    NodeNum <- as.list(baseNum) #Represented in DataVis as Text
    
    #Use Name Node Column for Data Vis
    baseName <- nodes[, 17]
    Name <- as.list(baseName) #Represented in DataVis as Text
    
    # Creates a graph
    g <- graph_from_adjacency_matrix(y, mode = "undirected", weighted = TRUE)  
    # Fixes Label Formatting
    V(g)$words3 <- gsub(",", "\n", nodes$words3) 
    V(g)$words3 <- gsub(",", "", V(g)$words3) 
    
    #find communities
    communities <- cluster_louvain(g)
    # Get the community membership for each node
    node_communities <- communities$membership
    # Assign colors to cliques
    num_communities <- max(node_communities) # + 1
    community_colors <- hcl(h = seq(15, 375, length = num_communities + 1), c = 100, l = 65)
    
    # Assuming communities$membership and communities$names are already defined
    community_sizes <- sizes(communities)
    biggest_community_size <- max(community_sizes) + 1
    # Create an empty community matrix with the correct number of columns
    community_matrix <- matrix("", nrow = biggest_community_size, ncol = length(community_sizes))
    
    #store central nodes of each community
    length(central_nodes_num) <- num_communities
    ShortPathMeans <- list()
    #creates community networks & Finds centrality
    for (k in seq_along(communities)){
      # Get the nodes in the current community
      nodes_new <- as.list(communities[[k]])
      shortPath_List <- list()
      for (node in nodes_new){
        tmp2 <- get.shortest.paths(g, from = node, to = my.node, weights = E(g))$vpath
        shortPath_List <- append(shortPath_List, tmp2)
      }
      # Calculate the average shortest path for the current community
      if (length(shortPath_List) > 0) {
        # Filter out empty lists from shortPath_List
        non_empty_paths <- shortPath_List[lengths(shortPath_List) > 0]
        # Calculate the average shortest path for the current community
        shortest_lengths <- sapply(non_empty_paths, length)
        shortPath <- mean(shortest_lengths)
      } else {
        shortPath <- 0  # No paths found for this community
      }
      # Store the average shortest path for the current community
      ShortPathMeans[[k]] <- shortPath
    }
    
    # Extract the elements from ShortPathMeans into separate columns
    row_data <- paste(my.site, "_", dte)
    row_data_with_shortpaths <- list(row_data)
    counter <- 1
    for (value in ShortPathMeans){
      row_data_with_shortpaths <- append(row_data_with_shortpaths, ShortPathMeans[counter])
      counter <- counter+1
    }
    if (is.null(ncol(graph_matrix))){
    } else if (length(row_data_with_shortpaths) < ncol(graph_matrix) & !is.null(ncol(graph_matrix))) {
      # Append NA to the rest of the data points
      missing_nas <- rep(NA, ncol(graph_matrix) - length(row_data_with_shortpaths))
      row_data_with_shortpaths <- c(row_data_with_shortpaths, missing_nas)
    } 
    graph_matrix <- rbind(graph_matrix, row_data_with_shortpaths)

  })
}
write.csv(graph_matrix, file = csv_file, row.names = FALSE)

