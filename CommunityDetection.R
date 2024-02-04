# Created script to Create network visualizations with colored cliques, a matrix that shows which nodes are present in the biggest clique each month, 
# and a sheet showing the size of the biggest clique, as well as which nodes it contains

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
dir_topics <- paste("~/Dropbox/Belief networks/Analyses/Juniper/MainTopics.csv")
files_nodes<-list.files(path=dir_nodes, pattern='csv', full.names = TRUE)
files_edges<-list.files(path=dir_edges, pattern='csv', full.names = TRUE)
files_topics<-read.csv(dir_topics)

#Get Column of Topics for site
topic_list <- as.list(files_topics[, my.site])


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
output_file <- pdf(paste('~/Desktop/NetVis/networks',my.site, 'MACRO_small.pdf', sep="_")) 

# Create a Network Measure matrix
  graph_matrix <- matrix(ncol = 7)
  colnames(graph_matrix) <- c("Month", "# of Community", "Modularity", "Central Node (Degree)", "Central Node (Weight)", "Central Node (Eigen)", "Central Nodes") 
  meas_folder <- "~/Desktop/CliqueMeas"
  meas_name <- paste(my.site, "_louvian_meas_small.csv")

# Create the full file path for the CSV file
  csv_file <- file.path(meas_folder, meas_name)

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
      
      #Find Ego Node
      shape_list <- character(length(row.index))
      shape_list <- rep("Null", length(row.index))
      for (k in topic_list) {
        ego_condition <- NodeNum == k
        shape_list[ego_condition] <- "Ego"
      }
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
      
      #store central nodes of each community
      length(central_nodes_num) <- num_communities
      #creates community networks & Finds centrality
      for (k in seq_along(communities)){
        # Get the nodes in the current community
        nodes_new <- as.list(communities[[k]])
        if (length(nodes_new) > 1) {
          # Create a subgraph for the current community
          row.index <- which(rownames(pCorMat) %in% nodes_new)
          filt <- pCorMat[row.index, row.index] 
  
          edge_number <- numeric(length = ncol(filt))
          # Loop through each column and count the number of positive values in each row
          for (i in seq_along(edge_number)) {
            edge_number[i] <- sum(filt[, i] > 0)
          }
          # Find the index of the column with the maximum count of positive values
          degree_centrality <- which.max(edge_number)
          central_index <- row.index[degree_centrality]
          central_nodes_num[k] <- central_index
          # # Fill the matrix with Central at the index of the central node
          shape_list[central_index] <- "Central"
        } else if (length(nodes_new) == 1) {
          degree_centrality <- nodes_new[[1]]
          central_index <- row.index[degree_centrality]
          shape_list[central_index] <- "Central"
          central_nodes_num[k] <- nodes_new
        }
      }
      
      # Plot the graph
      NodeNum = as.list(nodes$topic) #Labels the nodes by their topic
      graph <- ggraph(g, layout = 'graphopt') +
        geom_edge_fan(aes(edge_linewidth = weight, alpha = weight), edge_colour = "grey66", show.legend = FALSE) +
        geom_node_point(aes(size = centrality_degree(), color = as.factor(node_communities), shape =  shape_list), show.legend = FALSE) +
        scale_edge_width(range = c(0.6, 4)) +
        scale_shape_manual(values = c("Null" =  19, "Ego" = 17, "Central" = 15)) +
        scale_color_manual(values = community_colors) +
        geom_node_text(aes(label = words3), vjust = 0.75, repel=FALSE, lineheight = 0.75, size = 0.6) +
        scale_size(range = c(3,6)) +
        geom_node_text(aes(label = NodeNum), repel = FALSE, vjust = -1, fontface = "bold", size = 1) +
        theme_graph(fg_text_colour = 'black', base_family = "Arial")
      
      text_label <- dte  # Specify the text you want to add
      graph <- graph + annotate("text", x = -50, y = 50, label = text_label, hjust = 0, vjust = 0, size = 10)  
      
          # Save the graph in the PDF
      plot(graph)
      
      # Calculate graph statistics
      # Number of Communities
      comm_num <- length(communities)
      # Modularity of entire graph
      modularity <- modularity(communities)
      # List of Central Nodes
      central_nodes <- NULL
      counter <- 1
      for (n in central_nodes_num){
        selected_row <- nodes[nodes$topic == as.numeric(n)-1, ]
        node_info <- paste("(", as.numeric(n)-1, ")", selected_row$words3)
        central_nodes <- c(central_nodes, node_info)
        counter <- counter+1
      }
      central_nodes_string <- paste(central_nodes, collapse = "; ")
      # The most central node (Degree)
      node_degrees <- degree(g, mode = "all", loops = FALSE)
      node_degrees <- node_degrees[-1] 
      max_node_degree <- which.max(node_degrees)
      central_node <- paste("(", max_node_degree, ")", nodes$words3[as.numeric(max_node_degree)+1])
      # The most central node (edge weight)
      node_weight <- strength(g, mode = "all", loops = FALSE)
      node_weight <- node_weight[-1] 
      max_node_weight <- which.max(node_weight)
      weight_node <- paste("(", max_node_weight, ")", nodes$words3[as.numeric(max_node_weight)+1])
      #The most central node (eigencentrality)
      node_centralities <- evcent(g)$vector
      max_eigen_degree <- which.max(node_centralities)
      eigen_node <- paste("(", as.numeric(max_eigen_degree)-1, ")", nodes$words3[max_eigen_degree])
      colnames(graph_matrix) <- c("Month", "# of Community", "Modularity", "Central Node (Degree)", "Central Node (Weight)", "Central Node (Eigen)", "Central Nodes")
      #plug statistics into matrix 
      row_data <- c(paste(my.site, "_", dte),
                    comm_num,
                    modularity,
                    central_node,
                    weight_node,
                    eigen_node,
                    central_nodes_string)
      graph_matrix <- rbind(graph_matrix, row_data)
    }
  })
}
dev.off()
graph_matrix <- graph_matrix[-1, ]

# Save the graph matrix as a CSV file
write.csv(graph_matrix, file = csv_file, row.names = FALSE)
