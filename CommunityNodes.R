# Created table of communities with a list of each node within any given community. 

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
commNodes_folder <- "~/Desktop/CliqueMeas"


#Big File Loop to Make & Export Data Visualizations and Network Measure Matrix
for (file in files_sorted){
  try ({
    #Creates Date 
    x <- unlist(gregexpr(".", file, fixed=TRUE))
    dte <- substr(file[1], x-4, x-1)        
    mdte = (paste("20",substr(dte,3,4),"-",substr(dte,1,2),"-01", sep=""))
    
    if (dte == "0815" || dte == "1116" || dte == "0218" || dte == "0519") {
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
      colnames(community_matrix) <- paste("Community", 1:length(community_sizes))
      
      for (node_id in seq_along(communities)) {
        community_string <- list()
        for (name in communities$names) {
          name_index <- which(communities$names == name)
          number <- communities$membership[[name_index]]
          if (number == node_id) {
            row <- nodes[nodes$topic == name, ]
            community_string <- paste(community_string, " (", name, ")", row$words3)
          }
        }
        # Convert the concatenated string to a list
        community_list <- strsplit(paste(community_string, collapse = ","), "  ")[[1]]
        community_list <- community_list[-1]
        # Create the expanded vector with empty values
        community_list_expanded <- c(community_list, vector("character", biggest_community_size - length(community_list)))
        community_matrix[, node_id] <- community_list_expanded
      }
      
      meas_name <- paste(dte, my.site, "_community_nodes.csv")
      print(meas_name)
      # Create the full file path for the CSV file & Export
      csv_file <- file.path(meas_folder, meas_name)
      write.csv(community_matrix, file = csv_file, row.names = FALSE)
    }
  })
}

