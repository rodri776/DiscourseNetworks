library(Matrix)
#library(arrow)
library(ggraph)
library(ggplot2)
#library(RColorBrewer)
library(igraph)
library(tidygraph)
library(extrafont)

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
output_file <- pdf(paste('~/Desktop/NetVis/networks',my.site, 'MACRO.pdf', sep="_")) 

# Create a Network Measure matrix
graph_matrix <- matrix(ncol = 6)
colnames(graph_matrix) <- c("Graph Name", "Mean Degree", "Clustering Coefficient", "Edge Weight Mean", "Avg. Path Length", "Eigencentrality") 
meas_folder <- "~/Desktop/NetMeas"
meas_name <- paste(my.site, "_MACRO_meas.csv")

# Create the full file path for the CSV file
csv_file <- file.path(meas_folder, meas_name)

#Big File Loop to Make & Export Data Visualizations and Network Measure Matrix
for (file in files_sorted){
  try ({
    #Creates Date 
    x <- unlist(gregexpr(".", file, fixed=TRUE))
    dte <- substr(file[1], x-4, x-1)        
    print(dte) # Prints for Debugging 
    mdte = (paste("20",substr(dte,3,4),"-",substr(dte,1,2),"-01", sep=""))
    
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
    
    # Create Internal Network Graphs for the Topics of Interest
    counter <- 0
    for (k in topic_list) {
      as.numeric(k)
      row.index <- which(rownames(pCorMat) == k)
      filt <- pCorMat[row.index,] > 0 
      filt[row.index] <- TRUE
      q <- rownames(pCorMat)[filt == TRUE]
      if (counter == 0){
        a_p <- pCorMat[q, q]
        a_p[a_p < 0] <- 0
        #Filtering Nodes
        New_rows_to_keep <- c()
        for (i in 1:nrow(nodes)) {
          for (j in 1:length(q)){ 
            if (as.numeric(nodes[i, 'topic']) == as.numeric(q[[j]])) {
              New_rows_to_keep <- c(New_rows_to_keep, i)
            } 
          }
        }
        a_topic_nodes <- nodes[New_rows_to_keep, ] 
        #Find Topic Network Nodes
        a_condition <- NodeNum %in% a_topic_nodes$topic & !NodeNum %in% topic_list[c(2, 3)] 
        a_condition <- a_condition | is.na(a_condition)
        New_nodes <- ifelse(a_condition, "Abortion", "Other") #Represented in DataVis as Color
        #Find Ego Node
        ego_condition <- NodeNum == k
        EgoNode <- ifelse(ego_condition, "TrueValue", "FalseValue") #Represented in DataVis as Triangle
        counter <- counter+1
        #V(g)$color <- node_color[counter]
        #E(g)$color <- edge_color[counter]
      } else if (counter == 1){
          v_p <- pCorMat[q, q]
          v_p[v_p < 0] <- 0
          #Filtering Nodes
          New_rows_to_keep <- c()
          for (i in 1:nrow(nodes)) {
            for (j in 1:length(q)){ 
              if (as.numeric(nodes[i, 'topic']) == as.numeric(q[[j]])) {
                New_rows_to_keep <- c(New_rows_to_keep, i)
              } 
            }
          }
          v_topic_nodes <- nodes[New_rows_to_keep, ] 
          #Find Topic Network Nodes
          v_condition <- NodeNum %in% v_topic_nodes$topic & !NodeNum %in% topic_list[c(1, 3)] 
          v_condition <- v_condition | is.na(v_condition)
          overlap_condition <- list()
          New_nodes <- ifelse(v_condition, "Vaccines", New_nodes) #Represented in DataVis as Color
          for (v in v_condition) {
            overlap_condition <- v_condition == a_condition & v_condition == TRUE
            New_nodes <- ifelse(overlap_condition, "OverlapAV", New_nodes) #Represented in DataVis as Color
          }
          #Find Ego Node
          ego_condition <- NodeNum == k
          EgoNode <- ifelse(ego_condition, "TrueValue", EgoNode) #Represented in DataVis as Triangle
          counter <- counter+1
      } else if (counter == 2){
        c_p <- pCorMat[q, q]
        c_p[c_p < 0] <- 0
        #Filtering Nodes
        New_rows_to_keep <- c()
        for (i in 1:nrow(nodes)) {
          for (j in 1:length(q)){ 
            if (as.numeric(nodes[i, 'topic']) == as.numeric(q[[j]])) {
              New_rows_to_keep <- c(New_rows_to_keep, i)
            } 
          }
        }
        c_topic_nodes <- nodes[New_rows_to_keep, ] 
        #Find Topic Network Nodes
        c_condition <- NodeNum %in% c_topic_nodes$topic & !NodeNum %in% topic_list[c(1, 2)] 
        c_condition <- c_condition | is.na(c_condition)
        v_overlap_condition <- list()
        a_overlap_condition <- list()
        z_overlap_condition <- list()
        New_nodes <- ifelse(c_condition, "Climate", New_nodes) #Represented in DataVis as Color
        for (c in c_condition) {
          a_overlap_condition <- c_condition == a_condition & c_condition == TRUE
          v_overlap_condition <- c_condition == v_condition & c_condition == TRUE
          z_overlap_condition <- c_condition == v_condition & c_condition == TRUE & c_condition == a_condition 
          New_nodes <- ifelse(v_overlap_condition, "OverlapCV", New_nodes) #Represented in DataVis as Color
          New_nodes <- ifelse(a_overlap_condition, "OverlapCA", New_nodes) #Represented in DataVis as Color
          New_nodes <- ifelse(z_overlap_condition, "OverlapAVC", New_nodes) #Represented in DataVis as Color
        }
        #Find Ego Node
        ego_condition <- NodeNum == k
        EgoNode <- ifelse(ego_condition, "TrueValue", EgoNode) #Represented in DataVis as Triangle
        counter <- counter+1
      }
    }

    #For coloring Edges
    color_matrix <- matrix(0, nrow = nrow(pCorMat), ncol = ncol(pCorMat))
    for (i in 1:nrow(pCorMat)) {
      for (j in 1:ncol(pCorMat)) {
        if (pCorMat[i, j] > 0) { 
          color_matrix[i, j] <- "Other"
          if (pCorMat[i, j] %in% a_p) {
            color_matrix[i, j] <- "Abortion"
          } else if (pCorMat[i, j] %in% v_p) {
            color_matrix[i, j] <- "Vaccines"
          } else if (pCorMat[i, j] %in% c_p) {
            color_matrix[i, j] <- "Climate"
          }
        } 
      }
    }
    rownames(color_matrix) <- rownames(pCorMat)
    colnames(color_matrix) <- colnames(pCorMat)
    
    # Creates a graph
    g <- graph_from_adjacency_matrix(y, mode = "undirected", weighted = TRUE)  
    # Fixes Label Formatting
    V(g)$words3 <- gsub(",", "\n", nodes$words3) 
    V(g)$words3 <- gsub(",", "", V(g)$words3) 
    
    #For Coloring edges
    edge_list <- get.edgelist(g)
    color_df <- data.frame(matrix(ncol = 1, nrow = nrow(edge_list)))
    colnames(color_df) <- "Colors"
    # Assign color_matrix values to the edges
    for (i in 1:nrow(edge_list)) {
      from_node <- edge_list[i, 1]
      to_node <- edge_list[i, 2]
      color_df[i, 1] <- color_matrix[from_node, to_node]
    }
    # Convert "Colors" column to factor
    #color_df$Colors <- factor(color_df$Colors)
    
    # Add color_df as an edge attribute
    # Assign the edge colors to the "color" attribute of the graph
    E(g)$color <- color_df$Colors
    #E(g)$color <- set_edge_attr(g, "color", values = color_df$Colors)
    
    # Plot the graph
    NodeNum = as.list(nodes$topic) #Labels the nodes by their topic
    graph <- ggraph(g, layout='graphopt') + 
      geom_edge_fan(aes(edge_linewidth = weight, alpha = weight, edge_colour = color), show.legend = TRUE) +  #show.legend = FALSE
      scale_edge_width(range = c(0.6, 4)) +
      geom_node_point(aes(size = centrality_degree(), colour = New_nodes , shape = EgoNode), show.legend = TRUE) + 
      scale_edge_color_manual(
        values = c("Other" = "grey", "Abortion" = "red", "Climate" = "#40a302", "Vaccines" = "#0080ff")) +
      scale_color_manual(
        values = c("Abortion" = "red", "Climate" = "#40a302", "Vaccines" = "#0080ff", "OverlapCV" = "#219281", "OverlapAV" = "#814181", "OverlapCA" = "#A05202", "OverlapAVC" = "gold", "Other" = "grey50")) +
      geom_node_text(aes(label = words3), vjust = 0.75, repel=FALSE, lineheight = 0.75, size = 0.6) +
      scale_size(range = c(3,6)) +
      geom_node_text(aes(label = NodeNum), repel = FALSE, vjust = -1, fontface = "bold", size = 1) +
      theme_graph(fg_text_colour = 'black', base_family = "Times") +
      guides(colour = guide_legend(title = "Node Colors"),
             size = FALSE,
             shape = FALSE,
             alpha = FALSE,
             title = FALSE,  # This will remove the title from the legend
             "centrality_degree()" = FALSE,
             "weight" = FALSE,
             EgoNode = FALSE)
      #guides(colour = guide_legend(title = "Node Colors"), "centrality_degree()" = FALSE, weight = FALSE, EgoNode = FALSE)
    text_label <- dte  # Specify the text you want to add
    graph <- graph + annotate("text", x = -50, y = 50, label = text_label, hjust = 10, vjust = 0, size = 10)  
    # Save the graph in the PDF
    plot(graph)
    
    # Calculate graph statistics
    node_degrees <- degree(g, mode = "all", loops = FALSE)
    mean_degree <- mean(node_degrees)
    clustering_coefficient <- transitivity(g, type = "global")
    edge_weight_mean <- mean(E(g)$weight) 
    path_lengths <- distances(g, mode = "all", weights = E(g)$weight)
    average_path_length <- mean(path_lengths, na.rm = TRUE)
    eigen_centrality <- eigen_centrality(g, scale = TRUE)$vector
    eigencentrality <- mean(eigen_centrality, na.rm = TRUE)
    
    #plug statistics into matrix
    row_data <- c(paste(my.site, "_", my.node, "_", dte),
                  mean_degree,
                  clustering_coefficient,
                  edge_weight_mean,
                  average_path_length,
                  eigencentrality)
    graph_matrix <- rbind(graph_matrix, row_data)
  })
}
dev.off()
graph_matrix <- graph_matrix[-1, ]

# Save the graph matrix as a CSV file
write.csv(graph_matrix, file = csv_file, row.names = FALSE)

