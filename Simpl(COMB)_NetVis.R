# Created script to create SIMPLIFIED network visualizations and store them in a pdf
# Based on the avg. Edge Weights of Nodes AND the Edge Weights Sums of Nodes

library(Matrix)
#library(arrow)
library(grDevices)
library(ggraph)
library(ggplot2)
#library(RColorBrewer)
library(igraph)
library(tidygraph)

sites <- list('atlantic','motherjones','breitbart','thehill','gatewaypundit')
#Enter Platform (MANUALLY, copy and paste from sites list)
my.site <- readline(prompt= "Enter Platform: ")
#Enter Node (MANUALLY, Nodes of Interest can be found in "/Dropbox/Belief networks/Analyses/Juniper/Main Node Topics.pdf")
my.node <- readline(prompt="Enter Node: ")
my.node <- as.numeric(my.node)
topic <- list('Abortion','Climate Change','Vaccines')
#Enter Platform (MANUALLY, copy and paste from topic list)
my.top <- readline(prompt="Enter topic: ")

#Get Files
dir_nodes <-paste("~/Dropbox/Belief networks/Data/networks_plots_all/nodes/",my.site,"/",sep="")
dir_edges <-paste("~/Dropbox/Belief networks/Data/networks_plots_all/edges/edges200/pcor/",my.site,sep="")
dir_prev <-paste0("~/Dropbox/Belief networks/Analyses/Juniper/", my.top, "/Node Prevalence/", my.site, " _ ", my.node, " _prev.csv")
files_nodes<-list.files(path=dir_nodes, pattern='csv', full.names = TRUE)
files_edges<-list.files(path=dir_edges, pattern='csv', full.names = TRUE)
files_prev<-read.csv(dir_prev)


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

#Standardize Threshold
if (my.site == 'atlantic') {
  threshold <- 0.7
} else if (my.site == 'motherjones') {
  threshold <- 0.87
} else if (my.site == 'breitbart' || my.site == 'thehill') {
  threshold <- 1.08
} else if (my.site == 'gatewaypundit') {
  threshold <- 0.77
}  
   
  
#File Path for Exporting
pdfComb2 <- pdf(paste('~/Desktop/NetVis/networks',my.site, my.node, 'Comb_simpl.pdf', sep="_")) 

for (file in files_sorted){
  #Creates Date 
  x <- unlist(gregexpr(".", file, fixed=TRUE))
  dte <- substr(file[1], x-4, x-1)        
  print(dte) # Prints for Debugging 
  mdte = (paste("20",substr(dte,3,4),"-",substr(dte,1,2),"-01", sep=""))
  
  # load nodes
  nodes <- read.csv(paste(dir_nodes, "nodes_", dte, ".csv", sep=""), sep = ",")
  NodesList <- (nodes$topic)
  NodesList <- NodesList[NodesList != -1] #Exclude -1, BERTopic leftovers 
  
  #Create NodePrev lists
  Comb_List <- list()
  for (i in 1:nrow(files_prev)) {
    if (as.numeric(files_prev[i, 'Continuous..edge.weight.']) > as.numeric(threshold) || as.numeric(files_prev[i, 'Mean.Prevalence']) > 0.03) {
     Comb_List <- c(Comb_List, files_prev[i, 'Topic..Num.'])
    } 
  }
  #Filter Further
  Nodes_to_keep <- c()
  for (i in NodesList) {
    if (as.numeric(i) %in% Comb_List || as.numeric(i) == as.numeric(my.node)) {
      Nodes_to_keep <- c(Nodes_to_keep, i)
    } 
  }
  
  # load edges
  pCorMat <- as.matrix(read.csv(file, sep="", header = FALSE))
  pCorMat <- pCorMat[-1, ]
  pCorMat <- pCorMat[, -1]
  rownames(pCorMat) <- NodesList
  colnames(pCorMat) <- NodesList
  row.index <- which(rownames(pCorMat) == my.node)
  filt <- pCorMat[row.index,] > 0 & as.numeric(rownames(pCorMat)) %in% Nodes_to_keep
  filt[row.index] <- c(TRUE)
  x <- rownames(pCorMat)[filt == TRUE]
  y <- pCorMat[x, x]
  y[y < 0] <- 0
  
  #Debug 
  if (length(x) == 0){
    next
  }
  
  #Filtering Nodes
  rows_to_keep <- c()
  for (i in 1:nrow(nodes)) {
    for (j in 1:length(x)){ 
      if (as.numeric(nodes[i, 'topic']) == as.numeric(x[[j]])) {
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
  
  #Use Neg Node Column for Data Vis
  baseNeg <- nodes[, 19]
  Neg <- as.list(baseNeg)
  Neg <- as.numeric(unlist(Neg)) #Represented in DataVis as Color
  
  #Find Ego Node
  condition <- NodeNum == my.node
  EgoNode <- ifelse(condition, "TrueValue", "FalseValue") #Represented in DataVis as Triangle
  
  # Creates a graph
  if (length(y) == 1){
    next
  } else {
    g <- graph_from_adjacency_matrix(y, mode = "undirected", weighted = TRUE)
  }
  
  # Fixes Label Formatting
  V(g)$words3 <- gsub(",", "\n", nodes$words3) 
  V(g)$words3 <- gsub(",", "", V(g)$words3) 
  
  # Plot the graph
  NodeNum = as.list(nodes$topic) #Labels the nodes by their topic
  graph <- ggraph(g, layout = 'graphopt') + 
    geom_edge_fan(aes(edge_linewidth = weight, alpha = weight), edge_colour = "grey66", show.legend = FALSE) + 
    geom_node_point(aes(size = centrality_degree(), color = Neg, shape = EgoNode), show.legend = FALSE) + 
    scale_color_gradientn( 
      colors = colorRampPalette(c("#ffed24", "#FFEC57", "#fff494", "#FF4327", "#c90202"))(100), 
      limits = c(-3, 3)) + 
    geom_node_text(aes(label = words3), vjust = 0.75, repel=FALSE, lineheight = 0.75, size = 2.6) +
    scale_size(range = c(12,20)) +
    geom_node_text(aes(label = NodeNum), repel = FALSE, vjust = -1, fontface = "bold", size = 3.2) +
    theme_graph(fg_text_colour = 'black', base_family = "Arial")   
  text_label <- dte  # Specify the text you want to add
  graph <- graph + annotate("text", x = -50, y = 50, label = text_label, hjust = 0, vjust = 0, size = 10)  
  
  # Save the graph in the PDF
  plot(graph)
}
dev.off()
